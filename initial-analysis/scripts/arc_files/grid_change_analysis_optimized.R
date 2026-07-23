## ============================================================================
## San Diego pedestrian crash grid hotspot analysis
## ============================================================================
##
library(sf)
library(here)
library(leaflet)
library(leafgl)
library(tigris)
library(lubridate)
library(ggplot2)
library(lmtest)
library(sandwich)
library(stargazer)
library(scales)
library(Matrix)
library(dplyr)
library(tidyr)
library(data.table)
library(spdep)
library(future)
library(furrr)

## ----------------------------------------------------------------------
##  Parallel + caching setup
## ----------------------------------------------------------------------

n_cores <- 16

# Fork-based parallelism: cheap, no object serialization, Linux/macOS only.
# Swap to `multisession` if running on Windows.
future::plan(future::multicore, workers = n_cores)

# data.table's internal multithreaded aggregation
data.table::setDTthreads(n_cores)

# Cache tigris downloads to disk so repeat runs skip the network/IO wait
options(tigris_use_cache = TRUE)
tigris_cache_dir <- here::here("data-clean", ".tigris_cache")
dir.create(tigris_cache_dir, showWarnings = FALSE, recursive = TRUE)
options(tigris_cache_dir = tigris_cache_dir)

## ----------------------------------------------------------------------
##  Load data
## ----------------------------------------------------------------------

sd_crash_data <- readRDS(here("data-clean", "02_TIMS_Geocoded.rds")) |>
  dplyr::filter(COUNTY == "SAN DIEGO" & (PEDESTRIAN_ACCIDENT == "Y" | BICYCLE_ACCIDENT == "Y")) |>
  st_transform(2230)

san_diego_boundary <- tigris::counties(state = "CA", cb = FALSE) |>
  dplyr::filter(COUNTYFP == "073") |>
  st_transform(2230) |>
  st_buffer(dist = 100)

sd_cities_geo <- tigris::places(state = "CA", cb = TRUE) |>
  st_transform(2230) |>
  st_filter(san_diego_boundary, .predicate = st_intersects)

san_diego_city <- sd_cities_geo |>
  dplyr::filter(NAME == "San Diego")

san_diego_city_leaflet <- st_transform(san_diego_city, 4326)

roads <- read_sf(here("data-raw", "Roads_All_shapefile", "Roads_All.shp"))


## ----------------------------------------------------------------------
##  Hex grid
## ----------------------------------------------------------------------

san_diego_grid <- st_make_grid(san_diego_city, cellsize = 600, square = FALSE)
san_diego_grid_sf <- st_sf(geometry = san_diego_grid) |>
  st_filter(san_diego_city) |>
  st_filter(roads)

grid_chunks <- split(
  san_diego_grid_sf,
  cut(seq_len(nrow(san_diego_grid_sf)), n_cores, labels = FALSE)
)

san_diego_grid_sf <- furrr::future_map(
  grid_chunks,
  ~ st_intersection(.x, san_diego_city),
  .options = furrr::furrr_options(seed = TRUE)
) |>
  dplyr::bind_rows() |>
  st_as_sf()

san_diego_grid_sf$id <- seq_len(nrow(san_diego_grid_sf))

san_diego_grid_leaflet <- st_transform(san_diego_grid_sf, 4326)

leaflet() |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(
    data = san_diego_grid_leaflet,
    fillColor = "lightblue",
    fillOpacity = 0.2,
    weight = 2
  )

## ----------------------------------------------------------------------
##  Crash - grid spatial join
## ----------------------------------------------------------------------

sd_crash_in_city <- sd_crash_data |> st_filter(san_diego_city)

crash_chunks <- split(
  sd_crash_in_city,
  cut(seq_len(nrow(sd_crash_in_city)), n_cores, labels = FALSE)
)

crash_joined <- furrr::future_map(
  crash_chunks,
  ~ st_join(.x, san_diego_grid_sf, join = st_intersects),
  .options = furrr::furrr_options(seed = TRUE)
) |>
  dplyr::bind_rows() |>
  st_drop_geometry() |>
  data.table::as.data.table()

crash_counts_yearly <- crash_joined[
  , .(crash_count = .N), by = .(id, ACCIDENT_YEAR)
]

## ----------------------------------------------------------------------
##  Year Panels
## ----------------------------------------------------------------------

years <- 2022:2025

grid_ids <- san_diego_grid_sf$id
grid_panel <- data.table::CJ(id = grid_ids, ACCIDENT_YEAR = years)

final_panel <- merge(
  grid_panel, crash_counts_yearly,
  by = c("id", "ACCIDENT_YEAR"), all.x = TRUE
)
final_panel[is.na(crash_count), crash_count := 0]

final_panel_sf <- san_diego_grid_sf |>
  dplyr::select(id) |>
  dplyr::right_join(as.data.frame(final_panel), by = "id")

final_panel_leaflet <- final_panel_sf |> st_transform(4326)

## ----------------------------------------------------------------------
##  Year-by-year choropleth map
## ----------------------------------------------------------------------

pal <- colorNumeric(
  palette = "YlOrRd",
  domain = final_panel_leaflet$crash_count,
  na.color = "transparent"
)

available_years <- sort(unique(final_panel_leaflet$ACCIDENT_YEAR))

map <- leaflet() |> addProviderTiles(providers$CartoDB.Positron)

for (yr in available_years) {
  year_data <- final_panel_leaflet |> dplyr::filter(ACCIDENT_YEAR == yr)
  map <- map |>
    addPolygons(
      data = year_data,
      stroke = FALSE,
      fillColor = ~pal(crash_count),
      fillOpacity = 0.6,
      color = "#444444",
      weight = 1,
      popup = ~paste("Year:", ACCIDENT_YEAR, "<br>Grid ID:", id, "<br>Crashes:", crash_count),
      group = as.character(yr)
    )
}

map <- map |>
  addLayersControl(
    baseGroups = as.character(available_years),
    options = layersControlOptions(collapsed = FALSE),
    position = "topright"
  ) |>
  addLegend(
    pal = pal,
    values = final_panel_leaflet$crash_count,
    title = "Crash Count",
    position = "bottomright"
  )

map

## ----------------------------------------------------------------------
## 2022 - 2025 raw change map
## ----------------------------------------------------------------------

crash_change <- final_panel_leaflet |>
  st_drop_geometry() |>
  dplyr::filter(ACCIDENT_YEAR %in% c(2022, 2025)) |>
  dplyr::select(id, ACCIDENT_YEAR, crash_count) |>
  tidyr::pivot_wider(names_from = ACCIDENT_YEAR, names_prefix = "yr_", values_from = crash_count) |>
  dplyr::mutate(crash_diff = yr_2025 - yr_2022) |>
  dplyr::arrange(id)                       # <- enforce row order, fixes latent alignment bug

change_sf <- san_diego_grid_leaflet |>
  dplyr::select(id) |>
  dplyr::right_join(crash_change, by = "id") |>
  dplyr::arrange(id)

max_val <- max(abs(change_sf$crash_diff), na.rm = TRUE)

pal_change <- colorNumeric(
  palette = "RdYlBu",
  domain = c(-max_val, max_val),
  reverse = TRUE,
  na.color = "transparent"
)

leaflet(change_sf) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(
    stroke = FALSE,
    fillColor = ~pal_change(crash_diff),
    fillOpacity = 0.7,
    popup = ~paste0(
      "Grid ID: ", id, "<br>",
      "2022 Crashes: ", yr_2022, "<br>",
      "2025 Crashes: ", yr_2025, "<br>",
      "<b>Change: ", ifelse(crash_diff > 0, paste0("+", crash_diff), crash_diff), "</b>"
    )
  ) |>
  addLegend(pal = pal_change, values = c(-max_val, max_val), position = "bottomright")

wide_panel <- final_panel_leaflet |>
  st_drop_geometry() |>
  dplyr::filter(ACCIDENT_YEAR %in% c(2022, 2025)) |>
  dplyr::select(id, ACCIDENT_YEAR, crash_count) |>
  tidyr::pivot_wider(names_from = ACCIDENT_YEAR, names_prefix = "yr_", values_from = crash_count) |>
  dplyr::arrange(id)

grid_trends_sf <- san_diego_grid_sf |>
  dplyr::select(id) |>
  dplyr::right_join(wide_panel, by = "id") |>
  dplyr::arrange(id)

## ----------------------------------------------------------------------
##  Poly2nb neighbors
## ----------------------------------------------------------------------

neighbors <- poly2nb(grid_trends_sf, queen = TRUE)
neighbors_with_self <- include.self(neighbors)
weights_matrix_B <- nb2listw(neighbors_with_self, style = "B", zero.policy = TRUE)

grid_trends_sf <- grid_trends_sf |>
  dplyr::mutate(
    cluster_crashes_2022 = lag.listw(weights_matrix_B, yr_2022, zero.policy = TRUE),
    cluster_crashes_2025 = lag.listw(weights_matrix_B, yr_2025, zero.policy = TRUE),
    cluster_change = cluster_crashes_2025 - cluster_crashes_2022
  )

grid_trends_leaflet <- grid_trends_sf |>
  st_transform(4326) |>
  dplyr::mutate(map_change = ifelse(cluster_change == 0, NA, cluster_change))

max_change_val <- max(abs(grid_trends_leaflet$cluster_change), na.rm = TRUE)
pal_smoothing <- colorNumeric(
  palette = "RdYlBu",
  domain = c(-max_change_val, max_change_val),
  reverse = TRUE
)

leaflet(grid_trends_leaflet) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(stroke = FALSE, fillColor = ~pal_smoothing(cluster_change), fillOpacity = .5) |>
  addLegend(
    pal = pal_smoothing, values = c(-max_change_val, max_change_val),
    title = "Cluster Change (2022 to 2025)", position = "bottomright"
  )

## KNN double smooth neighbors (19 cells total avg)

centroids <- st_centroid(grid_trends_sf)

knn_neighbors_19 <- knearneigh(centroids, k = 19) |> knn2nb()
knn_with_self_19 <- include.self(knn_neighbors_19)
weights_matrix_knn_B <- nb2listw(knn_with_self_19, style = "B", zero.policy = TRUE)

grid_trends_sf <- grid_trends_sf |>
  dplyr::mutate(
    knn_crashes_2022 = lag.listw(weights_matrix_knn_B, yr_2022, zero.policy = TRUE),
    knn_crashes_2025 = lag.listw(weights_matrix_knn_B, yr_2025, zero.policy = TRUE),
    knn_change       = knn_crashes_2025 - knn_crashes_2022
  )

grid_trends_leaflet <- grid_trends_sf |> st_transform(4326)

max_change_val <- max(abs(grid_trends_leaflet$knn_change), na.rm = TRUE)
pal_smoothing <- colorNumeric(
  palette = "RdYlBu",
  domain = c(-max_change_val, max_change_val),
  reverse = TRUE,
  na.color = "transparent"
)

leaflet(grid_trends_leaflet) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(
    stroke = FALSE,
    fillColor = ~pal_smoothing(ifelse(knn_change == 0, NA, knn_change)),
    fillOpacity = .5
  ) |>
  addLegend(
    pal = pal_smoothing, values = c(-max_change_val, max_change_val),
    title = "KNN Change (2022 to 2025)", position = "bottomright"
  )

## ----------------------------------------------------------------------
##  Getis Ord (Fixed with broader neighborhood)
## ----------------------------------------------------------------------

grid_trends_sf <- grid_trends_sf |>
  dplyr::mutate(raw_change = yr_2025 - yr_2022)

knn_smooth <- knearneigh(centroids, k = 73) |> knn2nb()
knn_smooth_self <- include.self(knn_smooth)

weights_matrix_smooth <- nb2listw(knn_smooth_self, style = "W", zero.policy = TRUE)

grid_trends_sf$gi_z_score <- as.numeric(
  localG(grid_trends_sf$raw_change, weights_matrix_smooth, zero.policy = TRUE)
)

grid_trends_sf <- grid_trends_sf |>
  dplyr::mutate(
    p_value = 2 * (1 - pnorm(abs(gi_z_score))),
    significance_raw = dplyr::case_when(
      p_value <= 0.01 & gi_z_score > 0 ~ "Hot Spot (99% Conf.)",
      p_value <= 0.05 & gi_z_score > 0 ~ "Hot Spot (95% Conf.)",
      p_value <= 0.10 & gi_z_score > 0 ~ "Hot Spot (90% Conf.)",
      p_value <= 0.01 & gi_z_score < 0 ~ "Cold Spot (99% Conf.)",
      p_value <= 0.05 & gi_z_score < 0 ~ "Cold Spot (95% Conf.)",
      p_value <= 0.10 & gi_z_score < 0 ~ "Cold Spot (90% Conf.)",
      TRUE ~ NA_character_
    ),
    significance_raw = factor(
      significance_raw,
      levels = c(
        "Hot Spot (99% Conf.)", "Hot Spot (95% Conf.)", "Hot Spot (90% Conf.)",
        "Cold Spot (90% Conf.)", "Cold Spot (95% Conf.)", "Cold Spot (99% Conf.)"
      )
    )
  )

grid_trends_leaflet <- grid_trends_sf |> st_transform(4326)

hot_cold_colors <- c(
  "Hot Spot (99% Conf.)"  = "#b2182b",
  "Hot Spot (95% Conf.)"  = "#ef8a62",
  "Hot Spot (90% Conf.)"  = "#fddbc7",
  "Cold Spot (90% Conf.)" = "#d1e5f0",
  "Cold Spot (95% Conf.)" = "#67a9cf",
  "Cold Spot (99% Conf.)" = "#2166ac"
)

pal_gi_raw <- colorFactor(
  palette = hot_cold_colors,
  domain = grid_trends_leaflet$significance_raw,
  ordered = TRUE,
  na.color = "transparent"
)
leaflet(grid_trends_leaflet) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(
    stroke = FALSE,
    fillColor = ~pal_gi_raw(significance_raw),
    fillOpacity = 0.75,
  ) |>
  addLegend(
    pal = pal_gi_raw,
    values = grid_trends_leaflet$significance_raw,
    title = "Crash Change 2022-2025",
    position = "bottomright",
    na.label = "Not Significant"
  )






##### Getis Ord for 2023-2024 and 2024-2025 graph hotspot change comparisons 


wide_periods <- final_panel_sf |>
  st_drop_geometry() |>
  dplyr::filter(ACCIDENT_YEAR %in% c(2022, 2023, 2024, 2025)) |> 
  dplyr::select(id, ACCIDENT_YEAR, crash_count) |>
  tidyr::pivot_wider(names_from = ACCIDENT_YEAR, names_prefix = "yr_", values_from = crash_count) |>
  dplyr::mutate(
    change_22_23 = yr_2023 - yr_2022, 
    change_23_24 = yr_2024 - yr_2023,
    change_24_25 = yr_2025 - yr_2024
  ) |>
  dplyr::arrange(id)

grid_periods_sf <- san_diego_grid_sf |>
  dplyr::select(id) |>
  dplyr::right_join(wide_periods, by = "id") |>
  dplyr::arrange(id)

centroids_p <- st_centroid(grid_periods_sf)
knn_37 <- knearneigh(centroids_p, k = 19) |> knn2nb() |> include.self()
weights_matrix_37 <- nb2listw(knn_37, style = "W", zero.policy = TRUE)

grid_periods_sf <- grid_periods_sf |>
  dplyr::mutate(
    # 2022 to 2023
    z_22_23 = as.numeric(localG(change_22_23, weights_matrix_37, zero.policy = TRUE)),
    p_22_23 = 2 * (1 - pnorm(abs(z_22_23))),
    cat_22_23 = dplyr::case_when(
      p_22_23 <= 0.01 &                   z_22_23 > 0 ~ 3,
      p_22_23 <= 0.05 & p_22_23 > 0.01 & z_22_23 > 0 ~ 2, 
      p_22_23 <= 0.10 & p_22_23 > 0.05 & z_22_23 > 0 ~ 1,
      p_22_23 <= 0.10 & p_22_23 > 0.05 & z_22_23 < 0 ~ -1,
      p_22_23 <= 0.05 & p_22_23 > 0.01 & z_22_23 < 0 ~ -2, 
      p_22_23 <= 0.01 &                   z_22_23 < 0 ~ -3, 
      TRUE ~ 0
    ),
    # 2023 to 2024
    z_23_24 = as.numeric(localG(change_23_24, weights_matrix_37, zero.policy = TRUE)),
    p_23_24 = 2 * (1 - pnorm(abs(z_23_24))),
    cat_23_24 = dplyr::case_when(
      p_23_24 <= 0.01 &                   z_23_24 > 0 ~ 3,
      p_23_24 <= 0.05 & p_23_24 > 0.01 & z_23_24 > 0 ~ 2, 
      p_23_24 <= 0.10 & p_23_24 > 0.05 & z_23_24 > 0 ~ 1,
      p_23_24 <= 0.10 & p_23_24 > 0.05 & z_23_24 < 0 ~ -1,
      p_23_24 <= 0.05 & p_23_24 > 0.01 & z_23_24 < 0 ~ -2, 
      p_23_24 <= 0.01 &                   z_23_24 < 0 ~ -3, 
      TRUE ~ 0
    ),
    # 2024 to 2025 
    z_24_25 = as.numeric(localG(change_24_25, weights_matrix_37, zero.policy = TRUE)),
    p_24_25 = 2 * (1 - pnorm(abs(z_24_25))),
    cat_24_25 = dplyr::case_when(
      p_24_25 <= 0.01 &                   z_24_25 > 0 ~ 3,
      p_24_25 <= 0.05 & p_24_25 > 0.01 & z_24_25 > 0 ~ 2, 
      p_24_25 <= 0.10 & p_24_25 > 0.05 & z_24_25 > 0 ~ 1,
      p_24_25 <= 0.10 & p_24_25 > 0.05 & z_24_25 < 0 ~ -1,
      p_24_25 <= 0.05 & p_24_25 > 0.01 & z_24_25 < 0 ~ -2, 
      p_24_25 <= 0.01 &                   z_24_25 < 0 ~ -3, 
      TRUE ~ 0
    )
  )

counts_hs1 <- grid_periods_sf |>
  st_drop_geometry() |>
  dplyr::count(cat_22_23, cat_23_24) |>
  tidyr::complete(cat_22_23 = -3:3, cat_23_24 = -3:3, fill = list(n = 0)) |>
  dplyr::mutate(
    label_x = factor(cat_22_23, levels = 3:-3, labels = c("Hot (99%)","Hot (95%)","Hot (90%)","Neutral","Cold (90%)","Cold (95%)","Cold (99%)")),
    label_y = factor(cat_23_24, levels = 3:-3, labels = c("Hot (99%)","Hot (95%)","Hot (90%)","Neutral","Cold (90%)","Cold (95%)","Cold (99%)"))
  )

ggplot(counts_hs1, aes(x = label_x, y = label_y, fill = n)) + 
  geom_tile(color = "white", lwd = 1.5) +
  scale_fill_gradient(low = "#f7fbff", high = "#08306b", trans = "pseudo_log", label = scales::comma) +
  geom_vline(xintercept = "Neutral", linetype = "dashed") +
  geom_hline(yintercept = "Neutral", linetype = "dashed") +
  labs(title = "Hotspot (2022-23 to 2023-24)", subtitle = "k = 19", x = "2022 to 2023", y = "2023 to 2024", fill = "Hexagons") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

counts_raw1 <- grid_periods_sf |> st_drop_geometry() |> dplyr::count(change_22_23, change_23_24)

ggplot(counts_raw1, aes(x = change_22_23, y = change_23_24, fill = n)) +
  geom_tile(color = "white", lwd = 1.5) +
  scale_fill_gradient(low = "#f7fbff", high = "#08306b", trans = "log10", label = scales::comma) +
  geom_vline(xintercept = 0, color = "black", lwd = 0.8) +
  geom_hline(yintercept = 0, color = "black", lwd = 0.8) +
  labs(title = "Raw Change (2022-23 to 2023-24)", x = "Crash Change (2022 to 2023)", y = "Crash Change (2023 to 2024)", fill = "Hexagons") +
  theme_minimal()

counts_hs2 <- grid_periods_sf |>
  st_drop_geometry() |>
  dplyr::count(cat_23_24, cat_24_25) |>
  tidyr::complete(cat_23_24 = -3:3, cat_24_25 = -3:3, fill = list(n = 0)) |>
  dplyr::mutate(
    label_x = factor(cat_23_24, levels = 3:-3, labels = c("Hot (99%)","Hot (95%)","Hot (90%)","Neutral","Cold (90%)","Cold (95%)","Cold (99%)")),
    label_y = factor(cat_24_25, levels = 3:-3, labels = c("Hot (99%)","Hot (95%)","Hot (90%)","Neutral","Cold (90%)","Cold (95%)","Cold (99%)"))
  )

ggplot(counts_hs2, aes(x = label_x, y = label_y, fill = n)) + 
  geom_tile(color = "white", lwd = 1.5) +
  scale_fill_gradient(low = "#f7fbff", high = "#08306b", trans = "pseudo_log", label = scales::comma) +
  geom_vline(xintercept = "Neutral", linetype = "dashed") +
  geom_hline(yintercept = "Neutral", linetype = "dashed") +
  labs(title = "Hotspot Transitions (2023-24 to 2024-25)", subtitle = "k = 19", x = "2023 to 2024", y = "2024 to 2025", fill = "Hexagons") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

counts_raw2 <- grid_periods_sf |> st_drop_geometry() |> dplyr::count(change_23_24, change_24_25)

ggplot(counts_raw2, aes(x = change_23_24, y = change_24_25, fill = n)) +
  geom_tile(color = "white", lwd = 1.5) +
  scale_fill_gradient(low = "#f7fbff", high = "#08306b", trans = "log10", label = scales::comma) +
  geom_vline(xintercept = 0, color = "black", lwd = 0.8) +
  geom_hline(yintercept = 0, color = "black", lwd = 0.8) +
  labs(title = "Raw Change (2023-24 vs 2024-25)", x = "Crash Change (2023 to 2024)", y = "Crash Change (2024 to 2025)", fill = "Hexagons") +
  theme_minimal()


