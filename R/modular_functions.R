library(tidyverse)
library(sf)
library(leaflet)

# csv url for testing: https://data.cityofnewyork.us/api/views/7t3b-ywvw/rows.csv?accessType=DOWNLOAD
load_geo_file <- function(url = "https://data.cityofchicago.org/api/geospatial/bbvz-uum9?method=export&format=GeoJSON", type = NULL) {
  if (is.null(type) | type == "(detect)") {
    if (str_detect(tolower(url), "geojson")) {
      type <- "geojson"
    } else if (str_detect(tolower(url), "csv")) {
      type <- "csv"
    }
  }

  # download data if it doesn't exist
  the_name <- url |>
    urltools::domain() |>
    str_replace_all("[.]", "_") |>
    paste0(".", type)
  file <- fs::path("data", the_name)

  if (!dir.exists("data")) {
    dir.create("data")
  }

  if (!file.exists(file)) {
    download.file(url, file, quiet = TRUE)
  }

  if (type == "csv") {
    result <- file |>
      st_read(options = c("GEOM_POSSIBLE_NAMES=*geom*")) |>
      st_set_crs(4326)
  } else if (type == "geojson") {
    result <- geojsonsf::geojson_sf(file)
  } else {
    print("File type is unclear.")
  }

  if (!"geometry" %in% colnames(result)) {
    result <- result |>
      rename(geometry = colnames(result)["sfc_MULTIPOLYGON" == unlist(map(result, class))])
  }

  result |>
    st_make_valid()
}

choose_label <- function(df, label = pri_neigh) {
  df |>
    rename(neighborhood = {{ label }}) |>
    mutate(
      # sec_neigh is in all caps
      neighborhood = neighborhood |>
        str_to_title() |>
        str_replace_all("Uic", "UIC") |>
        str_replace_all("O'hare", "O'Hare") |>
        str_replace_all("Ohare", "O'Hare") |>
        str_replace_all("Mckin", "McKin") |>
        str_replace_all("Depaul", "DePaul") |>
        str_replace_all("nters ", "nter's ") |>
        str_replace_all(",", ", ") |>
        str_replace_all(" Of ", " of ") |>
        str_replace_all(" The ", " the ") |>
        str_replace_all(" And ", " and ") |>
        str_replace_all(
          "Kenwood, Oakland",
          "Oakland, Kenwood") |>
        str_replace_all(
          "Craigin", "Cragin") |>
        str_squish()
    ) |>
    group_by(neighborhood) |>
    summarise(geometry = st_union(geometry)) |>
    ungroup()
}

add_colors <- function(df) {
  geo_colors <- df |>
    st_touches(sparse = TRUE) |>
    as.matrix() |>
    igraph::graph_from_adjacency_matrix() |>
    igraph::greedy_vertex_coloring("dsatur")

  df |>
    mutate(fill_colors = geo_colors) |>
    optimize_neighborhood_colors()
}

optimize_neighborhood_colors <- function(df) {
  lincoln_sq <- df |>
    filter(neighborhood == "Lincoln Square") |>
    pull(fill_colors) |>
    {\(x) ifelse(length(x) == 0, 1, x)}()

  df |>
    mutate(
      # optimize bad / unideal solutions
      fill_colors = case_when(
        neighborhood %in% c("Chinatown", "Mckinley Park","McKinley Park", "Kenwood", "Englewood", "Printers Row", "Oakland, Kenwood", "Woodlawn", "Museum Campus", "Armour Square, Chinatown", "Manhattan", "East Boston") ~ 4, # dark
        str_detect(tolower(neighborhood),"mckinley park") ~ 4,
        neighborhood == "Wrigleyville" ~ lincoln_sq,
        neighborhood %in% c("Loop", "Brooklyn") ~ 1, # red
        neighborhood %in% c("Staten Island") ~ 2, # yellow
        neighborhood %in% c("Millenium Park", "Near South Side", "Queens") ~ 3, # green
        TRUE ~ fill_colors
      )
    )
}

# Determine whether to divide north/south
# or east/west. The goal is to decide
# automatically, dividing on the larger
# division, but it gets Chicago wrong.
# For now, I'll toggle manually.
add_regions <- function(df, split_north = TRUE){
  # coordinates_box <- df |>
  #   st_minimum_rotated_rectangle() |>
  #   st_bbox()
  # x_distance <-
  #   st_distance(
  #     st_sfc(st_point(
  #       c(coordinates_box$xmin,
  #         coordinates_box$ymin))) |>
  #       st_set_crs(4326),
  #     st_sfc(st_point(
  #       c(coordinates_box$xmax,
  #         coordinates_box$ymin))) |>
  #       st_set_crs(4326),
  #     )
  # y_distance <-
  #   st_distance(
  #     st_sfc(st_point(
  #       c(coordinates_box$ymin,
  #         coordinates_box$xmin))) |>
  #       st_set_crs(4326),
  #     st_sfc(st_point(
  #       c(coordinates_box$ymax,
  #         coordinates_box$xmin))) |>
  #       st_set_crs(4326),
  #   )
  #   if (y_distance > x_distance) {
  #   # map = tall & skinny; divide north/south.
  #   split_north <- TRUE
  #   } else {
  #   # map = short & wide; divide east/west.
  #   split_north <- FALSE
  #   }

  df <- df |>
    mutate(
      # get a polygon's midpoint
      y = st_coordinates(suppressWarnings(st_point_on_surface(geometry)))[, 2],
      x = st_coordinates(suppressWarnings(st_point_on_surface(geometry)))[, 1]
    )

  if (split_north) {
    df <- df |>
      mutate(
        # divide city in three regions
        region = case_when(
          y >= quantile(y, probs = seq(0,1,0.33))[3] ~ "north",
          y >= quantile(y, probs = seq(0,1,0.33))[2] ~ "central",
          TRUE ~ "south"
        )
      )
  } else {
    df <- df |>
      mutate(
        # divide city in three regions
        region = case_when(
          x >= quantile(x, probs = seq(0,1,0.33))[3] ~ "east",
          x >= quantile(x, probs = seq(0,1,0.33))[2] ~ "central",
          TRUE ~ "west"
        )
      )
  }
  df |>
    optimize_chicago_regions(split_north)
}

optimize_chicago_regions <- function(df, split_north){
  if (split_north) {
    df <- df |>
      mutate(
        region = case_when(
          neighborhood %in% c("West Town", "Gold Coast") ~ "central",
          neighborhood %in% c("Old Town") ~ "north",
          TRUE ~ region
        ))
  }
  df
}

choose_region <- function(df, portion, split_x = NULL, split_y = NULL) {
  # Split allows arbitrary divisions.
  if (!is.null(split_y)) {
    portion <- "all"
    if (split_y > 0 ) {
      df <- df |>
        filter(y > split_y)
    } else {
      df <- df |>
        filter(y < split_y)
    }
  }
  if (!is.null(split_x)) {
    portion <- "all"
    if (split_x > 0 ) {
      df <- df |>
        filter(x > split_x)
    } else {
      df <- df |>
        filter(x < split_x)
    }
  }

  if (portion != "all") {
    df <- df |>
      filter(region == portion)
  }
  df
}

make_sf_map <- function(df = geo_shapes, palette = paletteer::paletteer_d("rtist::hopper"), label = TRUE, show_accuracy = FALSE) {
  states_df <- usmap::us_map("states") |>
    st_transform(crs = 4326)

  states_abbr <- df |>
    st_join(states_df) |>
    pull(abbr) |>
    unique()

  counties_df <- usmap::us_map("counties", include = states_abbr) |>
    st_transform(crs = 4326)

  df_roads <- get_roads(df)

  df_water <- get_water(df)

  geo_centroids <- suppressWarnings(st_point_on_surface(df))

  df <- df |>
    mutate(
      label_placement = geo_centroids$geometry
    )

  the_plot <- df |>
    ggplot() +
    geom_sf(data = df_roads,
            color = "gray")

  if (!show_accuracy) {
    the_plot <- the_plot +
      geom_sf(aes(fill = LETTERS[fill_colors]),
              alpha = 0.6,
              show.legend = FALSE)
  } else {
    the_plot <- the_plot +
      geom_sf(aes(fill = accuracy),
              alpha = 0.6)
  }
  the_plot <- the_plot +
    geom_sf(data = df_water,
            fill = "skyblue",
            color = NA)

  if (label) {
    the_plot <- the_plot +
      ggrepel::geom_label_repel(
        aes(label = neighborhood,
            geometry = label_placement),
        stat = "sf_coordinates",
        size = 6,
        min.segment.length = 0.3
      )
  }

  if (!show_accuracy) {
    the_plot <- the_plot +
      scale_fill_manual(
        values = palette[1:4]
      )
  } else {
    the_plot <- the_plot +
      scale_fill_viridis_c(option = "magma")
  }
  the_plot +
    theme_void()
}

get_named_intersects <- function(df) {
  states_df <- usmap::us_map("states") |>
    st_transform(crs = st_crs(df))

  state_intersect <- df |>
    st_join(states_df) |>
    pull(abbr) |>
    unique()

  counties_df <-
    usmap::us_map(
      "counties",
      include = state_intersect) |>
    st_transform(crs = st_crs(df))

  the_df <- df |>
    st_join(counties_df) |>
    mutate(
      county = county |>
        str_remove_all("County") |>
        trimws()
    )

  data.frame(
    state = the_df$abbr,
    county = the_df$county) |>
    distinct() |>
    drop_na()
}

get_roads <- function(df) {
  inner_roads <- function(df, state) {
    suppressMessages(tigris::primary_secondary_roads(
      state = state,
      filter_by = st_bbox(df),
      progress_bar = FALSE)) |>
      st_transform(st_crs(df)) |>
      {\(x) suppressWarnings(
        st_intersection(x, st_as_sfc(st_bbox(df)))
      )}()
  }

  unique(get_named_intersects(df)$state) |>
    map({\(x) inner_roads(df, x)}) |>
    list_rbind() |>
    st_as_sf()
}

get_water <- function(df) {
  inner_water <- function(df, state, county) {
    suppressMessages(tigris::area_water(
      state = state,
      county = county,
      progress_bar = FALSE)) |>
      st_transform(st_crs(df)) |>
      {\(x) suppressWarnings(
        st_intersection(x, st_as_sfc(st_bbox(df)))
      )}()
  }

  get_named_intersects(df) |>
    pmap(inner_water, df = df) |>
    list_rbind() |>
    st_as_sf()
}

make_leaflet <- function(df, palette = paletteer::paletteer_d("rtist::hopper")) {
  df |>
    as_Spatial() |>
    leaflet() |>
    addProviderTiles("OpenStreetMap.Mapnik") |>
    addPolygons(
      fillColor = palette[df$fill_colors],
      col = "black",
      weight = 1,
      fillOpacity = 0.5,
      popup = df$neighborhood)
}

# sf equivalent of brushedPoints for polygons
brushedMapPoints <- function(df_sf, brush, cache = FALSE) {
  if (cache) {
    saveRDS(brush, "~/Developer/geoflash/data/brushPoints.RDS")
  }
  the_x <- unlist(brush[1:2])
  the_y <- unlist(brush[3:4])
  the_all <- data.frame(
    longitude = the_x[c(1, 2, 2, 1, 1)],
    latitude = the_y[c(1, 1, 2, 2, 1)])

  brush_polygon <- the_all |>
    as.matrix() |>
    list() |>
    st_polygon() |>
    st_sfc(crs = st_crs(df_sf))

  st_intersection(
    df_sf,
    brush_polygon)
}
