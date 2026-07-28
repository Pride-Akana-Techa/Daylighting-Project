## ============================================================
## Getis-Ord Gi* on pre/post crash change (Block Group + Hexagons)
## ============================================================
#### libraries #### 
library(sf)
library(here)
library(tigris)
library(dplyr)
library(tidyr)
library(data.table)
library(spdep)
library(tidycensus)
library(ggplot2)
library(lubridate)

options(tigris_use_cache = TRUE)
tigris_cache_dir <- here::here("data-clean", ".tigris_cache")
dir.create(tigris_cache_dir, showWarnings = FALSE, recursive = TRUE)
options(tigris_cache_dir = tigris_cache_dir)

out_dir <- here("initial-analysis", "outputs")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

target_crs <- 3310

#### city implementation dates ####
city_impl <- tibble::tibble(
  CITY         = c("San Diego", "San Francisco", "Los Angeles"),
  cutoff_date  = as.Date(c("2025-03-01", "2025-01-01", "2025-11-01")),
  scenario     = "Enforcement_Date"
)

city_fixed_2024 <- tibble::tibble(
  CITY         = c("San Diego", "San Francisco", "Los Angeles"),
  cutoff_date  = as.Date("2024-01-01"),
  scenario     = "Warning_Date"
)

city_info <- bind_rows(city_impl, city_fixed_2024)

#### crash data ####
crash_data <- readRDS(here("initial-analysis", "data-clean", "02_TIMS_Geocoded.rds")) |>
  dplyr::filter((PED_ACTION == "B" & INTERSECTION == "Y")) |>
  dplyr::filter(ACCIDENT_YEAR %in% 2020:2026) |>
  st_transform(target_crs)

crash_data <- crash_data[!st_is_empty(crash_data), ]

data_end_date <- max(crash_data$COLLISION_DATE, na.rm = TRUE)
data_end_date <- as.Date(data_end_date)

city_info <- city_info |>
  mutate(
    post_days  = if_else(
      scenario == "Warning_Date", 
      365, 
      as.numeric(data_end_date - cutoff_date)
    ),
    post_start = cutoff_date,
    post_end   = cutoff_date + post_days,
    
    pre_start  = cutoff_date - years(1),
    pre_end    = (cutoff_date + post_days) - years(1)
  )

#### block groups and boundaries ####
ca_places <- places(state = "CA", cb = TRUE, year = 2024) |>
  st_transform(target_crs)
sf_raw <- ca_places |> 
  filter(NAME == "San Francisco")|> 
  st_union()
sf_crop_box <- st_bbox(c(xmin = -230000, xmax = -170000, ymin = -100000, ymax = 0), crs = st_crs(target_crs))
sf_mainland <- st_crop(sf_raw, sf_crop_box)

city_boundaries <- list(
  "San Diego"     = ca_places |> filter(NAME == "San Diego") |> st_union(),
  "San Francisco" = sf_mainland,
  "Los Angeles"   = ca_places |> filter(NAME == "Los Angeles") |> st_union()
)

bg_3county <- get_acs(
  geography = "block group",
  variables = "B01001_001",
  state     = "CA",
  county    = c("San Diego", "San Francisco", "Los Angeles"),
  year      = 2023,
  cb        = TRUE,
  geometry  = TRUE
) |>
  st_transform(target_crs) |>
  st_make_valid()

bg_3county <- bg_3county[!st_is_empty(bg_3county), ]

tract_3county <- get_acs(
  geography = "tract",
  variables = "B01001_001",
  state     = "CA",
  county    = c("San Diego", "San Francisco", "Los Angeles"),
  year      = 2023,
  cb        = TRUE,
  geometry  = TRUE
) |>
  st_transform(target_crs) |>
  st_make_valid()

tract_3county <- tract_3county[!st_is_empty(tract_3county), ]


#### Morans I KNN sweep ####
sweep_knn_moran <- function(centroids, values, k_range) {
  out <- vector("list", length(k_range))
  for (i in seq_along(k_range)) {
    k <- k_range[i]
    nb <- knearneigh(centroids, k = k) |> knn2nb()
    lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
    mt <- moran.test(values, lw, zero.policy = TRUE)
    out[[i]] <- data.frame(
      param   = k,
      moran_i = unname(mt$estimate[["Moran I statistic"]]),
      z_score = unname(mt$statistic),
      p_value = mt$p.value
    )
  }
  do.call(rbind, out)
}

k_range <- seq(6, 16, by = 2)

calc_gi_panel <- function(polygons, crash_city, scenario_row, city_name, k_range) {
  months_in_window <- scenario_row$post_days / 30.44
  
  # pre and post crash spatial join
  crash_dt <- crash_city |>
    st_join(polygons |> select(GEOID), join = st_within) |>
    st_drop_geometry() |>
    as.data.table()
  
  crash_dt[, period := fifelse(
    COLLISION_DATE >= scenario_row$pre_start & COLLISION_DATE < scenario_row$pre_end, "pre",
    fifelse(COLLISION_DATE >= scenario_row$post_start & COLLISION_DATE <= scenario_row$post_end, "post", NA_character_)
  )]
  crash_dt <- crash_dt[!is.na(period) & !is.na(GEOID)]
  
  counts <- crash_dt[, .N, by = .(GEOID, period)]
  panel_base <- CJ(GEOID = polygons$GEOID, period = c("pre", "post"))
  panel <- merge(panel_base, counts, by = c("GEOID", "period"), all.x = TRUE)
  panel[is.na(N), N := 0]
  wide <- dcast(panel, GEOID ~ period, value.var = "N")
  
  poly_panel <- polygons |>
    select(GEOID) |>
    right_join(as.data.frame(wide), by = "GEOID")
  poly_panel <- poly_panel[!st_is_empty(poly_panel), ]
  
  poly_panel <- poly_panel |>
    mutate(
      change         = post - pre,
      pre_rate_mo    = pre  / months_in_window,
      post_rate_mo   = post / months_in_window,
      change_rate_mo = post_rate_mo - pre_rate_mo
    )
  
  centroids <- st_centroid(poly_panel)
  
  # Local Moran's I Sweep
  knn_results <- sweep_knn_moran(centroids, poly_panel$change_rate_mo, k_range)
  best_k <- knn_results$param[which.max(knn_results$z_score)]
  
  nb <- knearneigh(centroids, k = best_k) |> knn2nb()
  nb_self <- spdep::include.self(nb)
  lw <- nb2listw(nb_self, style = "W", zero.policy = TRUE)
  poly_panel$neighbor_geoids <- I(lapply(nb_self, function(idx) poly_panel$GEOID[idx]))
  
  poly_panel$local_gi_mean <- spdep::lag.listw(lw, poly_panel$change_rate_mo, zero.policy = TRUE)
 
  citywide_mean_change <- mean(poly_panel$change_rate_mo, na.rm = TRUE)
  poly_panel$change_rate_mo_adj <- poly_panel$change_rate_mo - citywide_mean_change
  poly_panel$gi_z_score <- as.numeric(localG(poly_panel$change_rate_mo_adj, lw, zero.policy = TRUE))
  poly_panel <- poly_panel |>
    mutate(
      p_value = 2 * (1 - pnorm(abs(gi_z_score))),
      significance = case_when(
        p_value <= 0.01 & gi_z_score > 0 ~ "Hot Spot (99% Conf.)",
        p_value <= 0.05 & gi_z_score > 0 ~ "Hot Spot (95% Conf.)",
        p_value <= 0.10 & gi_z_score > 0 ~ "Hot Spot (90% Conf.)",
        p_value <= 0.10 & gi_z_score < 0 ~ "Cold Spot (90% Conf.)",
        p_value <= 0.05 & gi_z_score < 0 ~ "Cold Spot (95% Conf.)",
        p_value <= 0.01 & gi_z_score < 0 ~ "Cold Spot (99% Conf.)",
        TRUE ~ NA_character_
      ),
      significance = factor(significance, levels = c(
        "Hot Spot (99% Conf.)", "Hot Spot (95% Conf.)", "Hot Spot (90% Conf.)",
        "Cold Spot (90% Conf.)", "Cold Spot (95% Conf.)", "Cold Spot (99% Conf.)"
      )),
      city = city_name,
      scenario = scenario_row$scenario,
      window_months = round(months_in_window, 1),
      pre_start = scenario_row$pre_start, pre_end = scenario_row$pre_end,
      post_start = scenario_row$post_start, post_end = scenario_row$post_end
    )
  
  list(panel = poly_panel, k = best_k, months = months_in_window)
}

process_city_scenario <- function(city_name, scenario_row, city_boundary, bg_all, tract_all, crash_all, k_range) {
  message("Processing: ", city_name, " | ", scenario_row$scenario)
  
  bg_sub  <- bg_all[st_intersects(bg_all, city_boundary, sparse = FALSE)[, 1], ]
  bg_city <- st_intersection(bg_sub, city_boundary) |> st_make_valid()
  bg_city <- bg_city[st_geometry_type(bg_city) %in% c("POLYGON", "MULTIPOLYGON"), ]
  bg_city <- bg_city[as.numeric(st_area(bg_city)) > 1000, ]
  
  tract_sub  <- tract_all[st_intersects(tract_all, city_boundary, sparse = FALSE)[, 1], ]
  tract_city <- st_intersection(tract_sub, city_boundary) |> st_make_valid()
  tract_city <- tract_city[st_geometry_type(tract_city) %in% c("POLYGON", "MULTIPOLYGON"), ]
  tract_city <- tract_city[as.numeric(st_area(tract_city)) > 1000, ]
  
  hex_cellsize_ft <- 1000
  hex_cellsize_m  <- hex_cellsize_ft * 0.3048  # 152.4
  
  hex_grid <- st_make_grid(city_boundary, cellsize = hex_cellsize_m, square = FALSE) |>
    st_as_sf() |> 
    st_intersection(city_boundary) |>
    st_make_valid()
  
  hex_city <- hex_grid[st_geometry_type(hex_grid) %in% c("POLYGON", "MULTIPOLYGON"), ]
  hex_city <- hex_city[as.numeric(st_area(hex_city)) > 1000, ]
  hex_city$GEOID <- paste0("HEX_", seq_len(nrow(hex_city)))
  
  crash_city <- crash_all[st_within(crash_all, city_boundary, sparse = FALSE)[, 1], ]
  
  bg_res  <- calc_gi_panel(bg_city, crash_city, scenario_row, city_name, k_range)
  tract_res <- calc_gi_panel(tract_city, crash_city, scenario_row, city_name, k_range)
  hex_res <- calc_gi_panel(hex_city, crash_city, scenario_row, city_name, k_range)
  
  list(
    bg_panel      = bg_res$panel,
    hex_panel     = hex_res$panel,
    tract_panel   = tract_res$panel,
    city_boundary = city_boundary,
    bg_k          = bg_res$k,
    hex_k         = hex_res$k,
    tract_k       = tract_res$k,
    months        = bg_res$months
  )
}

# Run all scenarios
results <- purrr::map(seq_len(nrow(city_info)), function(i) {
  row <- city_info[i, ]
  process_city_scenario(
    city_name     = row$CITY,
    scenario_row  = row,
    city_boundary = city_boundaries[[row$CITY]],
    bg_all        = bg_3county,
    tract_all     = tract_3county,
    crash_all     = crash_data,
    k_range       = k_range
  )
})

names(results) <- paste(city_info$CITY, city_info$scenario, sep = "_")

# Save combined outputs
saveRDS(results, here("initial-analysis", "data-clean", "gi_results_by_city_scenarios.rds"))
saveRDS(results, here("ShinyApp", "shiny-data", "gi_results_by_city_scenarios.rds"))


make_gi_map <- function(city_result, base_size = 14) {
  if (is.null(city_result)) return(NULL)
  
  bg_data <- city_result$bg_panel
  city_boundary <- city_result$city_boundary
  
  city_lab <- unique(bg_data$city)
  scen_lab <- unique(bg_data$scenario)
  months_lab <- unique(bg_data$window_months)
  
  
  
  title_text <- sprintf("%s (%s)", city_lab, gsub("_", " ", scen_lab))
  subtitle_text <- sprintf("Gi* Change in Crashes Per Month (%.f-Month Window | Optimal k = %d)", 
                           
                           months_lab, city_result$k)
  
  ggplot() +
    geom_sf(data = city_boundary, fill = "#F5F5F5", color = NA) +
    
    geom_sf(
      data = bg_data, 
      aes(fill = significance), 
      color = "darkgrey", 
      linewidth = 0.1,
      show.legend = TRUE
    ) +
    
    geom_sf(data = city_boundary, fill = NA, color = "#222222", linewidth = 0.6) +
    
    scale_fill_manual(
      values = hot_cold_colors,
      na.value = "white",                 
      na.translate = FALSE,               
      drop = FALSE,                       
      name = "Confidence"
    ) +
    coord_sf(datum = NA) + 
    theme_minimal(base_size = base_size, base_family = "Helvetica") +
    labs(
      title = title_text,
      subtitle = subtitle_text,
      x = NULL, y = NULL
    ) +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 11, face = "italic", color = "grey30"),
      legend.position = "right",
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 9),
      panel.grid = element_blank(),
      plot.margin = margin(10, 10, 10, 10)
    ) +
    
    guides(
      fill = guide_legend(
        override.aes = list(
          fill = hot_cold_colors
        )
      )
    )
}

purrr::walk(names(results), function(res_key) {
  res_data <- results[[res_key]]
  if (is.null(res_data)) return(NULL)
  
  p_map <- make_gi_map(res_data, base_size = 14)
  out_filename <- file.path(out_dir, sprintf("gi_map_%s.jpg", res_key))
  
  ggsave(
    filename = out_filename,
    plot = p_map,
    width = 10,
    height = 10,
    units = "in",
    dpi = 300
  )
}) 

