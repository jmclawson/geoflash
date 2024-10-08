library(tidyverse)
library(geojsonsf)
library(sf)
library(leaflet)

load_geojson <- function(url = "https://data.cityofchicago.org/api/geospatial/bbvz-uum9?method=export&format=GeoJSON") {
  add_colors <- function(df) {
    geo_colors <- df |>
      st_touches(sparse = TRUE) |>
      as.matrix() |>
      igraph::graph_from_adjacency_matrix() |>
      igraph::greedy_vertex_coloring("dsatur")

    df |>
      mutate(
        fill_colors = geo_colors
      )
  }

  file <- "data/shapefile.geojson"

  if (!dir.exists("data")) {
    dir.create("data")
  }

  if (!file.exists(file)) {
    download.file(url, file, quiet = TRUE)
  }

  geojson_sf(file) |>
    add_colors()
}

make_sf_map <- function(df = geo_shapes, label = pri_neigh, portion = c("all", "north", "central", "south"), split = NULL, hide_ohare = FALSE) {
  portion <- match.arg(portion)
  df <- df |>
    mutate(
      y = st_coordinates(suppressWarnings(st_point_on_surface(geometry)))[, 2],
      region = case_when(
        pri_neigh %in% c("West Town", "Gold Coast") ~ "central",
        y >= quantile(y, probs = seq(0,1,0.33))[3] ~ "north",
        y >= quantile(y, probs = seq(0,1,0.33))[2] ~ "central",
        TRUE ~ "south",
      ),
      {{ label }} := {{ label }} |>
        str_to_title() |>
        str_replace_all("Uic", "UIC") |>
        str_replace_all("O'hare", "O'Hare") |>
        str_replace_all("Ohare", "O'Hare") |>
        str_replace_all("Mckin", "McKin") |>
        str_replace_all("Depaul", "DePaul") |>
        str_replace_all(",", ", ") |>
        str_squish()
    )

  if (hide_ohare) {
    df <- df |>
      filter({{ label }} != "O'Hare")
  }

  if (!is.null(split)) {
    portion <- "all"
    df <- df |>
      filter(y > split)
  }

  if (portion != "all") {
    df <- df |>
      filter(region == portion)
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

  df <- df |>
    group_by({{ label }}, fill_colors) |>
    summarise(geometry = st_union(geometry)) |>
    ungroup()

  geo_centroids <- suppressWarnings(st_point_on_surface(df))

  df <- df |>
    mutate(
      label_placement = geo_centroids$geometry
    )

  df |>
    ggplot() +
    geom_sf(data = df_roads,
            color = "gray") +
    geom_sf(aes(fill = LETTERS[fill_colors]),# {{ label }}),
            alpha = 0.4,
            show.legend = FALSE) +
    geom_sf(data = df_water,
            fill = "skyblue",
            color = NA) +
    ggrepel::geom_label_repel(
      aes(label = {{ label }},
          # fill = LETTERS[fill_colors],
          geometry = label_placement),
      # show.legend = FALSE,
      # alpha = 0.3,
      stat = "sf_coordinates",
      size = 1.7,
      min.segment.length = 0.3
    ) +
    theme_void() +
    scale_fill_manual(
      values = paletteer::paletteer_d("rtist::hopper")[1:4]
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
      popup = df$pri_neigh)
}

##### Use: #####
# load_geojson() |>
#   make_leaflet()
#
# load_geojson() |>
#   make_sf_map(portion = "central")

