# Finding the optimal grid size to use for San Diego and other cities 
# Looking at the number of intersections and crashes that happen in each grid
# cell. Using grid cell sizes of 300ft, 400ft, 500ft, 600ft. 

library(here)
library(sf)
library(dplyr)
library(tigris)
library(leaflet)
library(leafgl)
library(lubridate)
library(tidyverse)
library(future.apply)
plan(multisession, workers = 5)


### For San Diego geography 
 
san_diego <- places(state = "CA", cb = TRUE, year = 2024) |>
  filter(NAME == "San Diego") |>
  st_cast("POLYGON") |>
  mutate(area = st_area(geometry)) |>
  slice_max(area, n = 1) |>
  select(-area) |>
  st_transform(2230) # Cal Albers

crashes <- readRDS(here("data-clean", "02_TIMS_Geocoded.rds")) |>
  dplyr::filter(ACCIDENT_YEAR >= 2022 & ACCIDENT_YEAR <= 2025) |>
  st_transform(2230) |> # Cal Albers
  st_filter(san_diego) 

crashes_intersection <- crashes |>
  filter(INTERSECTION == "Y")

crashes_pedestrian <- crashes |>
  filter(PEDESTRIAN_ACCIDENT == "Y")

 crashes_pedestrian_intersection <- crashes |>
   filter(INTERSECTION == "Y" & PEDESTRIAN_ACCIDENT == "Y")

intersections <- st_read(here("data-clean", "SD_intersections3.geojson"))
intersections <- st_as_sf(
  intersections, 
  coords = c("Longitude", "Latitude"),
  crs = 4326) |> 
  st_transform(2230) 

# citations geocoded using address and routing location type on arcgis
citations <- st_read(here("data-clean", "SD_daylighting_citations", "SD_daylighting_citations_all.shp")) |>
  st_transform(2230) |> 
  mutate(date_issue = ymd(date_issue)) 

grid_sizes <- c(200, 300, 400, 500, 600)

sd_bbox <- st_bbox(san_diego)

all_years_crashes <- unique(year(crashes$COLLISION_DATE))
all_years_citations <- unique(year(citations$date_issue))
all_months <- 1:12

results <- future_lapply(grid_sizes, function(size) {
  
  message(size)
  
  hex_grid_raw <- st_make_grid(
    sd_bbox, cellsize = size, square = FALSE) |> 
    st_sf() |> 
    mutate(id = row_number())
  hex_grid_raw$intersection_count <- lengths(st_intersects(hex_grid_raw, intersections))
  hex_grid_raw$crash_count_total  <- lengths(st_intersects(hex_grid_raw, crashes))
  hex_grid_raw$citation_count_total <- lengths(st_intersects(hex_grid_raw, citations))
  
  hex_grid <- hex_grid_raw |> 
    filter(intersection_count > 0 | crash_count_total > 0 | citation_count_total > 0) 
  
  hex_intersections <- hex_grid |> 
    st_drop_geometry() |> 
    select(id, intersection_count)

# constant intersection statistics 
  
  int_stats <- hex_intersections |>
    summarise(
      int_mean = mean(intersection_count),
      int_median = median(intersection_count),
      int_max = max(intersection_count),
      int_min = min(intersection_count),
      int_sd = sd(intersection_count)
    ) 
  
# crash monthly statistics 
  
  crash_join <- hex_grid |>
    st_join(crashes, join = st_intersects, left = FALSE)
  
  crash_summary <- crash_join |> 
    st_drop_geometry() |> 
    group_by(id, year = year(COLLISION_DATE), month = month(COLLISION_DATE)) |>
    summarise(
      crash_count = n(),
      crash_int_count = sum(INTERSECTION == "Y", na.rm = TRUE),
      crash_ped_count = sum(PEDESTRIAN_ACCIDENT == "Y", na.rm = TRUE),
      crash_ped_int_count = sum(INTERSECTION == "Y" & PEDESTRIAN_ACCIDENT == "Y", na.rm = TRUE),
      .groups = "drop"
    )
  
  complete_grid_crashes <- expand.grid(
    id = hex_grid$id,
    year = all_years_crashes,
    month = all_months
  )
  
  crash_stats <- complete_grid_crashes |> 
    left_join(crash_summary, by = c("id", "year", "month")) |> 
    mutate(across(
      c(crash_count, crash_int_count, crash_ped_count, crash_ped_int_count), 
      ~replace_na(.x, 0)
    )) |>
    group_by(year, month) |>
    summarise(
    # Total Crashes
      crash_mean = mean(crash_count),
      crash_median = median(crash_count),
      crash_max = max(crash_count),
      crash_min = min(crash_count),
      crash_sd = sd(crash_count),
      crash_total = sum(crash_count),
      
    # Intersection Crashes
      crash_int_mean = mean(crash_int_count),
      crash_int_median = median(crash_int_count),
      crash_int_max = max(crash_int_count),
      crash_int_min = min(crash_int_count),
      crash_int_sd = sd(crash_int_count),
      crash_int_total = sum(crash_int_count),
      
  # Pedestrian crashes
      crash_ped_mean = mean(crash_ped_count),
      crash_ped_median = median(crash_ped_count),
      crash_ped_max = max(crash_ped_count),
      crash_ped_min = min(crash_ped_count),
      crash_ped_sd = sd(crash_ped_count),
      crash_ped_total = sum(crash_ped_count),
      
  # Pedestrian intersection crashes
      crash_ped_int_mean = mean(crash_ped_int_count),
      crash_ped_int_median = median(crash_ped_int_count),
      crash_ped_int_max = max(crash_ped_int_count),
      crash_ped_int_min = min(crash_ped_int_count),
      crash_ped_int_sd = sd(crash_ped_int_count),
      crash_ped_int_total = sum(crash_ped_int_count),
      
      .groups = "drop"
    )
  
  
  citation_join <- hex_grid |>
    st_join(citations, join = st_intersects, left = FALSE)
  
  citation_summary <- citation_join |> 
    st_drop_geometry() |> 
    group_by(id, year = year(date_issue), month = month(date_issue)) |>
    summarise(citation_count = n(), .groups = "drop")
  
  complete_grid_citations <- expand.grid(
    id = hex_grid$id,
    year = all_years_citations,
    month = all_months
  )
   

  
  citation_stats <- complete_grid_citations |> 
    left_join(citation_summary, by = c("id", "year", "month")) |> 
    mutate(citation_count = replace_na(citation_count, 0)) |>
    group_by(year, month) |>
    summarise(
      citation_mean = mean(citation_count),
      citation_median = median(citation_count),
      citation_max = max(citation_count),
      citation_min = min(citation_count),
      citation_sd = sd(citation_count),
      citation_total = sum(citation_count),
      .groups= "drop"
    )
  
  temporal_stats <- full_join(crash_stats, citation_stats, by = c("year", "month")) |> 
    mutate(
      grid_size = paste0(size, "ft"),
      total_cells = nrow(hex_grid),
      int_mean = int_stats$int_mean,
      int_median = int_stats$int_median,
      int_max = int_stats$int_max,
      int_min = int_stats$int_min,
      int_sd = int_stats$int_sd
    )
  
  return(temporal_stats)
  
}, future.seed = TRUE)

descriptive_stats_comparison <- bind_rows(results) |> 
  select(grid_size, total_cells, year, month, everything()) |> 
  arrange(grid_size, year, month)

print(descriptive_stats_comparison)


#write_csv(descriptive_stats_comparison, here("data-clean", "grid_size_descriptive_stats.csv"))
