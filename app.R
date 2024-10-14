library(shiny)
library(bslib)
library(ragg)# fix font size on labels
source("R/modular_functions.R")

palette_choices <-
  paletteer::palettes_d_names |>
  filter(length >= 4) |>
  mutate(
    output = paste(package, palette, sep = "::")
  ) |>
  slice_sample(n = 10) |>
  pull(output) |>
  c("rtist::hopper", "wesanderson::Royal1") |>
  unique() |>
  sort()

##### UI components ######
options_sidebar <- sidebar(
  accordion(
    id = "side_accordion",
    multiple = FALSE,
    open = "Focus",
    accordion_panel(
      title = "Load",
      icon = icon("cloud"),
      textInput(
        "choose_url", "URL of GeoJSON or CSV file with spatial boundaries:",
        value = "https://data.cityofchicago.org/api/geospatial/bbvz-uum9?method=export&format=GeoJSON"
      ),
      p("Examples:"),
      tags$ul(
        tags$li(actionLink("link_boston", "Boston")),
        tags$li(actionLink("link_chicago", "Chicago")),
        tags$li(actionLink("link_nyc", "New York City")),
        # tags$li(actionLink("link_paris", "Paris")),
        tags$li(actionLink("link_sf", "San Francisco")),
      ),
      radioButtons(
        "url_type", "File type:",
        choices = c("(detect)", "geojson", "csv")),
      actionButton(
        "load_button", "Start mapping",
        icon = icon("route")
      )
    ),
    accordion_panel(
      title = "Select",
      icon = icon("table-columns"),
      selectInput(
        "choose_column", "Select labelling column",
        choices = c("Choose" = "")
      )
    ),
    accordion_panel(
      title = "Focus",
      icon = icon("magnifying-glass"),
      selectInput(
        "choose_region", "Select grouping",
        choices = c("all"),
        selected = "all"
      )
    ),
    accordion_panel(
      title = "Customize",
      icon = icon("palette"),
      selectInput(
        "choose_palette", "Choose a palette",
        choices = palette_choices,
        selected = "rtist::hopper"
      ),
      p("The", a(href="https://emilhvitfeldt.github.io/r-color-palettes/discrete/rtist/hopper/","default palette"), "is derived from Edward Hopper's", em("Nighthawks,"), "on display at the Art Institute of Chicago in Grant Park."))
  ),
  uiOutput("page_explanation")
)

page_learn <- nav_panel(
  "Learn",
  fillRow(
    height = "20%",
    layout_columns(
      height = "100%",
      value_box(
        title = "Grouping",
        value = textOutput("learn_region"),
        showcase = icon("map-location-dot")),
      value_box(
        title = "Regions",
        value = textOutput("learn_count"),
        showcase = icon("hashtag"),
        theme = "secondary"),
      value_box(
        title = "Portion of data",
        value = textOutput("learn_percent"),
        showcase = icon("chart-pie"),
        theme = "secondary")
    )
  ),
  card(plotOutput("learn_map"))
)

page_study <- nav_panel(
  "Explore",
  leafletOutput("study_map")
)

page_test <- nav_panel(
  "Test",
  fillRow(
    height = "20%",
    layout_columns(
      height = "100%",
      actionButton(
        inputId="refreshbutton",
        label = textOutput("the_target"),
        icon = icon("puzzle-piece"),
        class = "card"),
      value_box(
        title = "Selected",
        value = textOutput("the_selected"),
        showcase = icon("object-group")),
      uiOutput("the_status"))
  ),
  card(fill = TRUE,
       plotOutput("test1", brush = "test_brush"))
)

page_review <- nav_panel(
  "Review",
  card(plotOutput("review_plot_map")))

link_source <- tags$a(
  shiny::icon("github"), "Source",
  href = "https://github.com/jmclawson/geoflash",
  target = "_blank",
  align = "right"
)

##### UI object #####

ui <- page_navbar(
  id = "active_tab",
  title = "FlashCartes",
  theme = bs_theme(
    bootswatch = "cosmo", version = 5,
  ) |>
    bs_add_rules(
      list(
        ".navbar.navbar-default {background-color: $secondary !important;}",
        "#refreshbutton {height: 100%;}",
        "#refreshbutton i {font-size: 40pt; margin-bottom: 0.3em; display: block; margin-top: 8%; margin-left: auto; margin-right: auto;}",
       '#refreshbutton:hover i::before {content: "\\f021";}',
        "#the_target {font-size: 2rem !important; text-transform: none !important; display: block; margin-left: auto; margin-right: auto;}",
        "#the_status .card {height: 100%;}")),
  sidebar = options_sidebar,
  page_learn,
  page_study,
  page_test,
  page_review,
  nav_spacer(),
  nav_item(link_source)
)

##### Server function #####

server <- function(input, output, session) {
  # Try to load something at start
  app_params <- reactiveValues(
    the_url = "https://data.cityofchicago.org/api/geospatial/bbvz-uum9?method=export&format=GeoJSON",
    the_type = "(detect)",
    the_brushed = NA,
    neighborhood = NA,
    attempts = NA,
    accuracy = NA
  )

  the_tally <- reactive(
    data.frame(
      neighborhood = app_params$neighborhood,
      attempts = app_params$attempts,
      accuracy = app_params$accuracy
    )
  )

  output$page_explanation <- renderUI({
    if (input$active_tab == "Learn") {
      p("Set a grouping to focus more closely on one part of the city.")
    } else if (input$active_tab == "Explore") {
      p("Zoom in and move the map to identify cross streets and areas of interest.")
    } else if (input$active_tab == "Test") {
      div(p("Click and drag your mouse cursor to select ", strong(target_neighborhood()),
        "as your target. Click the puzzle piece to choose another, or ", shiny::actionLink("refreshlink", "refresh"), "here."))
    } else {
      p("Check the accuracy of your recent testing. Regions with deeper colors probably need to be studied further.")
    }
  })

  observeEvent(input$load_button,{
    app_params$the_url = input$choose_url
    app_params$the_type = input$url_type
  })

  observeEvent(input$choose_url,{
    updateSelectInput(
      session, "choose_column",
      choices = c("Choose" = "")
    )
  })

  init_data <- reactive({
    load_geo_file(app_params$the_url,
                  type = app_params$the_type)
  })

  column_options <- reactive({
    colnames(init_data()) |>
      str_subset("geometry", negate = TRUE) |>
      str_subset("shape_area", negate = TRUE) |>
      str_subset("shape_len", negate = TRUE) |>
      str_subset("objectid", negate = TRUE) |>
      str_subset("BoroCode", negate = TRUE)
  })

  observe(
    updateSelectInput(
      session, "choose_column",
      choices = column_options(),
      selected = column_options()[1]
    )
  )

  data <- reactive({
    req(input$choose_column)
    req(input$choose_column != "geometry")
    req(input$choose_column != "such_trouble")
    init_data() |>
      choose_label(input$choose_column) |>
      add_colors() |>
      add_regions()
  })

  ready_data <- reactive({
    # print(data())
    req(input$choose_region)
    data() |>
      choose_region(
        portion = input$choose_region)
  })

  output$learn_table <- renderTable(
    data() |>
      select(neighborhood, portion) |>
      as.data.frame() |>
      head()
  )

  output$learn_text <- renderText(
    paste0(
      "typeof = ",
      typeof(as.data.frame(init_data())),
      "; class = ",
      class(as.data.frame(init_data())) |>
        paste0(collapse = ", "),
      "; with dim = ",
      dim(as.data.frame(init_data())) |>
        paste0(collapse = " by "))
  )

  output$learn_region <- renderText(
    str_to_title(input$choose_region)
  )

  output$learn_count <- renderText(
    ready_data() |>
      nrow()
  )

  output$learn_percent <- renderText(
    (nrow(ready_data()) / nrow(data()) * 100) |>
      round(1) |>
      paste0("%")
  )

  output$learn_map <- renderPlot(
    ready_data() |>
      make_sf_map(
        palette = paletteer::paletteer_d(input$choose_palette),
        label = TRUE
      )
  )

  output$study_map <- renderLeaflet(
    ready_data() |>
      make_leaflet(
        palette = paletteer::paletteer_d(input$choose_palette)
      )
  )

  target_neighborhood <- reactive({
    ready_data() |>
      pull(neighborhood) |>
      sample(1)
  })

  output$the_target <- renderText(
    target_neighborhood()
  )

  the_brush <- reactive({
    input$test_brush
  })

  target_neighborhood <- reactive({
    input$refreshbutton
    input$refreshlink
    ready_data() |>
      pull(neighborhood) |>
      sample(1)
  })

  observeEvent(input$link_chicago, {
    updateTextInput(session, "choose_url",
                    value = "https://data.cityofchicago.org/api/geospatial/bbvz-uum9?method=export&format=GeoJSON")
  })

  observeEvent(input$link_boston, {
    updateTextInput(session, "choose_url",
                    value = "https://hub.arcgis.com/api/v3/datasets/2ac87de59a2c47ca8e6d44da598b8832_79/downloads/data?format=geojson&spatialRefId=4326&where=1%3D1")
  })

  observeEvent(input$link_nyc, {
    updateTextInput(session, "choose_url",
                    value = "https://data.cityofnewyork.us/api/views/7t3b-ywvw/rows.csv?accessType=DOWNLOAD")
  })

  observeEvent(input$link_sf, {
    updateTextInput(session, "choose_url",
                    value = "https://data.sfgov.org/api/views/mw29-m2za/rows.csv?date=20241014&accessType=DOWNLOAD")
  })

  observeEvent(input$link_paris, {
    updateTextInput(session, "choose_url",
                    value = "https://www.data.gouv.fr/fr/datasets/r/4765fe48-35fd-4536-b029-4727380ce23c")
    updateRadioButtons(session, "url_type", selected = "geojson")
  })

  observeEvent(
    input$choose_region, {
      app_params$neighborhood <-
        app_params$neighborhood |>
        c(target_neighborhood())
      app_params$attempts <-
        app_params$attempts |>
        c(0)
      app_params$accuracy <-
        app_params$accuracy |>
        c(NA)
    })

  observeEvent(input$refreshbutton, {
    session$resetBrush("test_brush")
    app_params$neighborhood <-
      app_params$neighborhood |>
      c(target_neighborhood())
    app_params$attempts <-
      app_params$attempts |>
      c(0)
    app_params$accuracy <-
      app_params$accuracy |>
      c(NA)
    # print(the_tally())
    })

  observeEvent(input$refreshlink, {
    session$resetBrush("test_brush")
    app_params$neighborhood <-
      app_params$neighborhood |>
      c(target_neighborhood())
    app_params$attempts <-
      app_params$attempts |>
      c(0)
    app_params$accuracy <-
      app_params$accuracy |>
      c(NA)
    # print(the_tally())
  })

  plus_attempt <- function() {
    app_params$attempts[length(app_params$attempts)] <- app_params$attempts[length(app_params$attempts)] + 1
  }

  avg_accuracy <- function() {
    num_attempts <- app_params$attempts[length(app_params$attempts)]

    current_accuracy <- ifelse(
      target_neighborhood() %in% brushed_neighborhoods()$neighborhood,
      1/nrow(brushed_neighborhoods()),
      0)

    the_mean <-
      # approximate past attempts
      rep(app_params$accuracy[length(app_params$accuracy)],
          length = (num_attempts - 1)) |>
      # add current attempt
      c(current_accuracy) |>
      # average
      mean(na.rm = TRUE)

    # update param
    app_params$accuracy[length(app_params$accuracy)] <- the_mean
  }

  observeEvent(input$test_brush, {
    plus_attempt()
    avg_accuracy()
    output$the_status <- renderUI(
      if (!isTruthy(input$test_brush)) {
        value_box(
          title = "No selection",
          value = "Select",
          showcase = icon("arrow-pointer"),
          theme = "primary"
        )
      } else if (isTruthy(brushed_neighborhoods()) && nrow(brushed_neighborhoods()) == 1 &&
                 brushed_neighborhoods()$neighborhood == target_neighborhood()) {
        value_box(
          title = "Status",
          value = "You did it!",
          showcase = icon("face-smile-beam"),
          theme = "success")
      } else if (isTruthy(brushed_neighborhoods()) && target_neighborhood() %in% brushed_neighborhoods()$neighborhood && 1/nrow(brushed_neighborhoods()) >= 0.3) {
        value_box(
          title = "Status",
          value = "Almost!",
          showcase = icon("face-surprise"),
          theme = "cyan"
        )
      }  else if (isTruthy(brushed_neighborhoods()) && target_neighborhood() %in% brushed_neighborhoods()$neighborhood && 1/nrow(brushed_neighborhoods()) > 0) {
        value_box(
          title = "Status",
          value = "Meh",
          showcase = icon("face-meh"),
          theme = "warning"
        )
      } else {
        value_box(
          title = "Status",
          value = "Try again",
          showcase = icon("face-sad-tear"),
          theme = "danger"
        )
      })
    })

  brushed_neighborhoods <- reactive({
    req(input$test_brush)
    # brushedPoints() doesn't work for sf
    # brushedPoints(ready_data(), input$test_brush)
    brushedMapPoints(
      ready_data(),
      input$test_brush,
      cache = FALSE)
  })

  output$the_selected <- renderText(
    if (!isTruthy(input$test_brush)) {
      0
    } else {
      nrow(brushed_neighborhoods())
    }
  )

  output$the_status <- renderUI(
    value_box(
      title = "No selection",
      value = "Select",
      showcase = icon("arrow-pointer"),
      theme = "primary"
    ))

  output$test1 <- renderPlot(
    ready_data() |>
      make_sf_map(
        palette = paletteer::paletteer_d(input$choose_palette),
        label = FALSE
      )
  )

  review_map <- reactive(
    data() |>
      filter(neighborhood %in% the_tally()$neighborhood) |>
      left_join(
        the_tally() |>
          drop_na() |>
          summarize(
            accuracy = mean(accuracy, na.rm = TRUE),
            .by = neighborhood
            ))
  )

  output$review_plot_map <- renderPlot(
    review_map() |>
      make_sf_map(
        label = TRUE,
        show_accuracy = TRUE
      )
  )

  observe(
    updateSelectInput(
      session, "choose_region",
      choices = c("all", unique(pull(data(), region)))
    )
  )
}

##### Run the application #####
shinyApp(ui = ui, server = server)
