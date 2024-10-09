library(shiny)
library(bslib)
source("R/modular_functions.R")

##### UI components ######
options_sidebar <- sidebar(
  # p("This is where I'll put options for loading a data set, setting a palette, and choosing regions."),
  shiny::selectInput(
    "choose_column", "Select neighborhood column",
    choices = c("pri_neigh"),
    selected = "pri_neigh"
  ),
  shiny::selectInput(
    "choose_region", "Select region",
    choices = c("all"),
    selected = "all"
  ),
  shiny::selectInput(
    "choose_palette", "Choose a palette",
    choices = c("rtist::hopper", "wesanderson::Royal1"),
    selected = "rtist::hopper"
  )
  )

page_learn <- nav_panel(
  "Learn",
  plotOutput("learn_map")
  )

page_study <- nav_panel(
  "Study",
  leafletOutput("study_map")
  )

page_clusters <- nav_panel(
  "Clusters",
  p("The idea here is to try k-means clustering to get to know areas by groups of neighborhood names. I think this may be close to how we tend to think of neighborhoods, by relating, for instance, neighborhood X as being near neighborhoods A, B, and C."))

page_test_1 <- nav_panel(
  "Test - 1",
  p("On this page, users will be shown a map with one neighborhood named. The user will use the brush stroke to select the neighborhood. A status bar or value box will let them know how close they're getting based on which neighborhoods have been selected: If the target neighborhood represents 0% of the selected neighborhoods, it's red with an accompanying bad icon; if it represents greater than 0% of the selected neighborhoods, the color jumps to a scale between white and green, with an accompanying middling icon; as the percentage approaches 100%, it's an affirming green color with a smiley face."))

page_test_2 <- nav_panel(
  "Test - 2",
  p("On this page, one neighborhood will be colored and the rest will be blank. The user has to choose the neighborhood's name out of an incomplete list of possible neighborhoods. Optional control: A sliding scale lets the user choose difficulty level; this control will vary the selection of possible neighborhoods, possibly adding additional options or limiting the selection to those neighborhoods that are especially nearby."))

link_source <- tags$a(
  shiny::icon("github"), "Source",
  href = "https://github.com/jmclawson/geoflash",
  target = "_blank",
  align = "right"
)

##### UI object #####

ui <- page_navbar(
  title = "GeoFlash",
  sidebar = options_sidebar,
  page_learn,
  page_study,
  page_clusters,
  page_test_1,
  page_test_2,
  nav_spacer(),
  nav_item(link_source)
)

##### Server object #####

server <- function(input, output, session) {
  init_data <- reactive({
    load_geojson()
  })

  data <- reactive({
    init_data() |>
      choose_label(input$choose_column) |>
      add_colors() |>
      add_regions()
  })

  column_options <- reactive({
    colnames(init_data()) |>
      str_subset("geometry", negate = TRUE) |>
      str_subset("shape_area", negate = TRUE) |>
      str_subset("shape_len", negate = TRUE)
  })

  output$learn_map <- renderPlot(
    data() |>
      choose_region(
        portion = input$choose_region) |>
      make_sf_map(
        palette = paletteer::paletteer_d(input$choose_palette)
      )
  )

  output$study_map <- renderLeaflet(
    data() |>
      choose_region(
        portion = input$choose_region) |>
      make_leaflet(
        palette = paletteer::paletteer_d(input$choose_palette)
      )
  )

  observe(
    updateSelectInput(
      session, "choose_column",
      choices = column_options()
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
