library(tidyverse)
library(sf)
library(leaflet)

load_geojson <- function(url = "https://data.cityofchicago.org/api/geospatial/bbvz-uum9?method=export&format=GeoJSON") {
  # limit color palette and try to avoid
  # neighboring regions of same-color


  # download data if it doesn't exist
  file <- "data/shapefile.geojson"

  if (!dir.exists("data")) {
    dir.create("data")
  }

  if (!file.exists(file)) {
    download.file(url, file, quiet = TRUE)
  }

  geojsonsf::geojson_sf(file)
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
    optimize_chicago_colors()
}

optimize_chicago_colors <- function(df) {
  lincoln_sq <- df |>
    filter(neighborhood == "Lincoln Square") |>
    pull(fill_colors)

  df |>
    mutate(
      # optimize bad / unideal solutions
      fill_colors = case_when(
        neighborhood %in% c("Chinatown", "Mckinley Park", "Kenwood", "Englewood", "Printers Row", "Oakland, Kenwood", "Woodlawn", "Museum Campus", "Armour Square, Chinatown") ~ 4, # dark
        neighborhood == "Wrigleyville" ~ lincoln_sq,
        neighborhood == "Loop" ~ 1, # red
        neighborhood %in% c("Millenium Park", "Near South Side") ~ 3, # green
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


make_sf_map <- function(df = geo_shapes, hide_ohare = FALSE, palette = paletteer::paletteer_d("rtist::hopper")) {
  if (hide_ohare) {
    df <- df |>
      filter(neighborhood != "O'Hare")
  }

  df_roads <- suppressMessages(tigris::primary_secondary_roads(
    state = "IL",
    filter_by = st_bbox(df),
    progress_bar = FALSE)) |>
    st_transform(st_crs(df)) |>
    {\(x) suppressWarnings(
      st_intersection(x, st_as_sfc(st_bbox(df)))
    )}()

  df_water <- suppressMessages(tigris::area_water(
    state = "IL",
    county = "Cook",
    progress_bar = FALSE)) |>
    st_transform(st_crs(df)) |>
    {\(x) suppressWarnings(
      st_intersection(x, st_as_sfc(st_bbox(df)))
      )}()

  geo_centroids <- suppressWarnings(st_point_on_surface(df))

  df <- df |>
    mutate(
      label_placement = geo_centroids$geometry
    )

  df |>
    ggplot() +
    geom_sf(data = df_roads,
            color = "gray") +
    geom_sf(aes(fill = LETTERS[fill_colors]),
            alpha = 0.6,
            show.legend = FALSE) +
    geom_sf(data = df_water,
            fill = "skyblue",
            color = NA) +
    ggrepel::geom_label_repel(
      aes(label = neighborhood,
          geometry = label_placement),
      stat = "sf_coordinates",
      size = 1.7,
      min.segment.length = 0.3
    ) +
    theme_void() +
    scale_fill_manual(
      values = palette[1:4]
    )
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

##### Use: #####
# load_geojson() |>
#   make_leaflet()
#
# load_geojson() |>
#   make_sf_map(portion = "central",
#               palette = paletteer::paletteer_d("wesanderson::Royal1"))

