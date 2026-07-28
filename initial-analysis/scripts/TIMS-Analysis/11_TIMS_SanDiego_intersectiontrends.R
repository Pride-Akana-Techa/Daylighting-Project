# -------------------------------------------------------------------------
# 06_TIMS_SanDiego_intersectiontrends 
# This script looks at trends between 10yr, 5yr, 2yr before and 2yr after
# policy implementation at every intersection in San Diego. 
# most of the code is outdated for our project 
# -------------------------------------------------------------------------
# load libraries
library(osmextract)   # download OpenStreetMap data as sf objects
library(sfnetworks)   # road network graph (nodes = intersections, edges = segments)
library(dodgr)        # fast weighted graph distances on street networks
library(tidytransit)  # GTFS transit data parsing
library(sf)           # spatial data manipulation (read, transform, buffer, filter)
library(dplyr)        # data wrangling
library(ggplot2)      # static maps
library(tmap)         # interactive maps
library(stringr)      # string extraction (used for OSM other_tags parsing)
library(tigris)
library(leaflet)
library(leafgl)
library(here)
library(readr)
library(leaflet.extras)
library(units)
library(tidyr)
library(tidyverse)
library(lubridate)
library(tidygeocoder)
options(tigris_use_cache = TRUE)



# -------------------------------------------------------------------------
# load data
crossings <- readRDS(here("initial-analysis", "data-raw", "OSM_california_crossings.rds"))
streets <- readRDS(here("initial-analysis", "data-raw", "OSM_california_streets.rds"))
points <- readRDS(here("initial-analysis", "data-clean", "TIMS_Filtered.rds"))
closest_crash_int <- readRDS(here("initial-analysis", "data-raw", "TIMS_closest_crash_int.rds"))
sd_citation_data <- readRDS(here("SD_citation_geocode.rds"))


ca_boundary <- states(cb = FALSE) |>
  filter(NAME == "California")

san_diego_boundary <- counties(state = "CA", cb = FALSE) |>
  filter(COUNTYFP == "073")

# -------------------------------------------------------------------------
# Filter spatial data to just be within san francisco county boundary 
crossings_san_diego<- st_filter(crossings, 
                                     st_transform(san_diego_boundary, st_crs(crossings)))
streets_san_diego <- st_filter(streets, 
                                   st_transform(san_diego_boundary, st_crs(streets)))
closest_crash_san_diego <- st_filter(closest_crash_int, 
                                         st_transform(san_diego_boundary, st_crs(closest_crash_int)))
# -------------------------------------------------------------------------
# plot data
leaflet() |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(
    data = san_diego_boundary,
    fillColor = "lightblue",
    fillOpacity = 0.2,
    color = "black",
    weight = 2,
    group = "California"
  ) |>
  addGlPoints(
    data = crossings_san_diego,
    group = "Crossings",
    opacity = .3,
    radius = 6,
    fillColor = "black"
  ) |>
  addGlPoints(
    data = closest_crash_san_diego,
    group = "Crashes",
    opacity = .3,
    radius = 6,
    fillCOlor = "blue"
  )

# -------------------------------------------------------------------------
# create buffer around intersections 
BUFFER_FT <- 50
BUFFER_M  <- BUFFER_FT * 0.3048   
CRS_CAL    <- 3310 #NAD83 / California Albers


crossings_proj <- st_transform(crossings_san_diego, CRS_CAL)
buffers_proj   <- st_buffer(crossings_san_diego, dist = BUFFER_M)
buffers_int   <- st_transform(buffers_proj, 4326)   # back to WGS84 for mapping



data_years <- 2014:2025 

crash_per_int <- buffers_int |> 
  st_join(closest_crash_san_diego, join = st_intersects) |> 
  st_drop_geometry() |> 
  # filter(!is.na(osm_id)) |> 
  # filter(!is.na(ACCIDENT_YEAR)) |> 
  count(osm_id, ACCIDENT_YEAR) |> 
  complete(osm_id, ACCIDENT_YEAR = data_years, fill = list(n = 0)) |> 
  pivot_wider(
    names_from = ACCIDENT_YEAR,
    values_from = n,
    names_prefix = "crashes_"
  )

# calculate the number of crashes per intersection 
# calculate for all years, 5 years before, and 2 years before
# because idk which one is best 
crash_per_int <- crash_per_int |>
  mutate(
    total_crashes = rowSums(across(starts_with("crashes_"))),
    before_implementation_avg = rowMeans(
      across(c(crashes_2014, crashes_2015, crashes_2016, crashes_2017,
               crashes_2018, crashes_2019, crashes_2020, crashes_2021,
               crashes_2022, crashes_2023)),
      na.rm = TRUE),
    before_implementation_5year_avg = rowMeans(
      across(c(crashes_2019, crashes_2020, crashes_2021,
               crashes_2022, crashes_2023)),
      na.rm = TRUE),
    before_implementation_2year_avg = rowMeans(
      across(c(crashes_2022, crashes_2023)),
      na.rm = TRUE
    ),
    after_implementation_avg = rowMeans(
      across(c(crashes_2024, crashes_2025)),
      na.rm = TRUE
    )
  )

crash_per_int <- crash_per_int %>%
  mutate(
    average_change = after_implementation_avg - before_implementation_avg,
    average_change_5yr = after_implementation_avg - before_implementation_5year_avg,
    average_change_2yr = after_implementation_avg - before_implementation_2year_avg
  )

# -------------------------------------------------------------------------
# Calculate crashes rate by intersection type 

sd_intersections_crashes <- left_join(
  crossings_san_diego, 
  st_drop_geometry(crash_per_int),  
  by = "osm_id"
)

sd_display <- sd_intersections_crashes |> 
  st_transform(4326) |> 
  mutate(
    total_crashes = rowSums(across(starts_with("crashes_")), na.rm = TRUE),
    radius = pmax(3, sqrt(total_crashes) * 2) 
  )

pal <- colorNumeric(
  palette = c("darkred", "white", "darkblue"),
  domain = sd_display$average_change_2yr,
  na.color = "grey"
)

leaflet(sd_display) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addCircleMarkers(
    radius = 3,
    fillColor = ~pal(average_change_2yr),
    color = "black",       
    weight = 0.5,
    fillOpacity = 0.8,
    stroke = TRUE
  ) |>
  addLegend(
    pal = pal,
    values = ~average_change_2yr,
    title = "Average Change"
  )


t.test(crash_per_int$average_change_2yr, mu = 0)


mean_diff <- -0.02035362 
sd_change <- sd(crash_per_int$average_change_2yr)  
cohens_d <- mean_diff / sd_change


stat <- sd_intersections_crashes %>%
  group_by(crossing_type) %>%
  summarize(
    n_intersections = n(),
    mean_change_2yr = mean(average_change_2yr),
    mean_change_5yr = mean(average_change_5yr),
    mean_change_11yr = mean(average_change),
    se_2yr = sd(average_change_2yr) / sqrt(n()),
    se_5yr = sd(average_change_5yr) / sqrt(n()),
    se_11yr = sd(average_change) / sqrt(n()),
    t_stat_2yr = mean_change_2yr / se_2yr,
    t_stat_5yr = mean_change_5yr / se_5yr,
    t_stat_11yr = mean_change_11yr / se_11yr,
    p_value_2yr = 2 * (1 - pt(abs(t_stat_2yr), n() - 1)),
    p_value_5yr = 2 * (1 - pt(abs(t_stat_5yr), n() - 1)),
    p_value_11yr = 2 * (1 - pt(abs(t_stat_11yr), n() - 1)),
    pct_improved_2yr = sum(average_change_2yr < 0) / n() * 100,
    pct_improved_5yr = sum(average_change_5yr < 0) / n() * 100,
    pct_improved_11yr = sum(average_change < 0) / n() * 100,
    
    .groups = 'drop'
  ) |>
  arrange(mean_change_2yr)


# -------------------------------------------------------------------------
#Dissolving buffers and finding intersection centroids

buffers_int_clean <- st_make_valid(buffers_int)

buffers_dis <- st_union(buffers_int_clean)
sf_use_s2(FALSE)

buffers_dis_extracted <- st_collection_extract(buffers_dis, type = "POLYGON")
intersection_centroids <- st_centroid(buffers_dis_extracted) |>
  st_sf()


buffers_int_clean <- st_make_valid(buffers_int)

intersection_matrix <- st_intersects(buffers_int_clean, sparse = FALSE)

library(igraph)

g <- graph_from_adjacency_matrix(intersection_matrix, mode = "undirected")
clusters <- components(g)
buffer_groups <- clusters$membership

# cluster ID to your buffers
buffers_with_groups <- buffers_int_clean |>
  mutate(cluster_id = buffer_groups)

# cluster union and centroids
intersection_centroids <- buffers_with_groups |>
  st_drop_geometry() |>
  group_by(cluster_id) |>
  summarise(n_buffers = n(), .groups = "drop") |>
  left_join(
    buffers_with_groups |>
      group_by(cluster_id) |>
      summarise(geometry = st_union(geometry), .groups = "drop"),
    by = "cluster_id"
  ) |>
  st_as_sf() |>
  st_transform(CRS_CAL) |>
  st_centroid() |>
  st_transform(4326)

sf_use_s2(TRUE)



leaflet() |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(
    data = san_diego_boundary,
    fillColor = "lightblue",
    fillOpacity = 0.2,
    color = "black",
    weight = 2,
    group = "California"
  ) |>
  addGlPoints(
    data = crossings_san_diego,
    group = "Crossings",
    opacity = .3,
    radius = 6,
    fillColor = "black"
  ) |>
  addGlPoints(
    data = intersection_centroids,
    group = "intersections",
    opacity = .3,
    radius = 6,
    fillCOlor = "blue"
  )




##### OSM vs TIMS distance measures ####

closest_crash_sd$distance_to_osm_int <- drop_units(closest_crash_sd$distance_to_osm_int)



closest_crash_sd |>
  summarize(
    median_osm = median(distance_to_osm_int, na.rm = TRUE),
    q1_osm = quantile(distance_to_osm_int, 0.25, na.rm = TRUE),
    q3_osm = quantile(distance_to_osm_int, 0.75, na.rm = TRUE),
    max_osm = max(distance_to_osm_int, na.rm = TRUE)
  )

closest_crash_sd |>
  summarize(
    median_osm = median(DISTANCE, na.rm = TRUE),
    q1_osm = quantile(DISTANCE, 0.25, na.rm = TRUE),
    q3_osm = quantile(DISTANCE, 0.75, na.rm = TRUE),
    max_osm = max(DISTANCE, na.rm = TRUE)
  )


ggplot(closest_crash_sd, aes(x = distance_to_osm_int)) +
  geom_histogram(binwidth = 2) +
  coord_cartesian(xlim = c(0,150))

ggplot(closest_crash_sd, aes(x = DISTANCE*.3048)) +
  geom_histogram(binwidth = 2) +
  coord_cartesian(xlim = c(0,150))



closest_crash_sd %>%
  summarize(
    pct_within_10ft = mean(distance_to_osm_int <= 6.09, na.rm = TRUE) * 100,
    pct_within_10m = mean(distance_to_osm_int <= 10, na.rm = TRUE) * 100,
    pct_within_20m = mean(distance_to_osm_int <= 20, na.rm = TRUE) * 100,
    pct_within_50m = mean(distance_to_osm_int <= 50, na.rm = TRUE) * 100
  )
# 6.09 meters 20.3%, 10 meters 65.5%, 20 meters 84.3%, 50 meters 93.5%


closest_crash_sd %>%
  summarize(
    pct_within_10ft = mean(DISTANCE <= 6.09, na.rm = TRUE) * 100,
    pct_within_10m = mean(DISTANCE*.3048 <= 10, na.rm = TRUE) * 100,
    pct_within_20m = mean(DISTANCE*.3048 <= 20, na.rm = TRUE) * 100,
    pct_within_50m = mean(DISTANCE*.3048 <= 50, na.rm = TRUE) * 100
  )
# 6.09 meters 66.5%, 10 meters 76.7%, 20 meters 82.7%, 50 meters 93.1%


ggplot() +
  stat_ecdf(
    data = closest_crash_sd,
    aes(x = distance_to_osm_int,
        color = "OSM crossings")
  ) +
  stat_ecdf(
    data = closest_crash_sd,
    aes(x = DISTANCE * 0.3048,
        color = "TIMS crossings")
  ) +
  scale_y_continuous(labels = \(x) x * 100) +
  labs(
    x = "Distance (m)",
    y = "Percent within distance",
    color = "Dataset"
  ) +
  coord_cartesian(xlim = c(0,50)) +
  theme_minimal()



ggplot() +
  stat_ecdf(
    data = closest_crash_sd,
    aes(x = distance_to_osm_int * 3.28084,
        color = "OSM crossings")
  ) +
  stat_ecdf(
    data = closest_crash_sd,
    aes(x = DISTANCE,
        color = "TIMS crossings")
  ) +
  geom_vline(
    xintercept = 20,
    linetype = "dashed",
    color = "black",
    linewidth = 1
  ) +
  scale_y_continuous(
    labels = scales::percent_format(scale = 100)
  ) +
  labs(
    x = "Distance (ft)",
    y = "Percent within distance",
    color = "Dataset"
  ) +
  coord_cartesian(xlim = c(0, 150)) +
  theme_minimal()

closest_crash_sd <- closest_crash_sd |>
  mutate("crossing_type" = closest_osm_int$crossing_type)


ggplot() +
  stat_ecdf(
    data = closest_crash_sd,
    aes(
      x = distance_to_osm_int * 3.28084,
      color = "OSM crossings"
    )
  ) +
  stat_ecdf(
    data = closest_crash_sd,
    aes(
      x = DISTANCE,
      color = "TIMS crossings"
    )
  ) +
  geom_vline(
    xintercept = 20,
    linetype = "dashed",
    color = "black",
    linewidth = 1
  ) +
  scale_y_continuous(
    labels = scales::percent_format(scale = 100)
  ) +
  labs(
    x = "Distance (ft)",
    y = "Percent within distance",
    color = "Dataset"
  ) +
  coord_cartesian(xlim = c(0, 150)) +
  facet_wrap(~ crossing_type) +
  theme_minimal()




closest_crash_sd <- closest_crash_sd |>
  mutate(
    distance_diff_ft = distance_to_osm_int * 3.28084 - DISTANCE
  )

ggplot(closest_crash_sd, aes(x = distance_diff_ft)) +
  geom_histogram(binwidth = 2) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "red"
  ) +
  labs(
    x = "OSM Distance - TIMS Distance (ft)",
    y = "Count",
    title = "Difference in Distance to Nearest Crossing"
  ) +
  coord_cartesian(xlim = c(-150, 150)) +
  theme_minimal()



ggplot(
  closest_crash_sd,
  aes(
    x = DISTANCE,
    y = distance_to_osm_int * 3.28084
  )
) +
  geom_point(alpha = 0.3) +
  labs(
    x = "TIMS Distance (ft)",
    y = "OSM Distance (ft)",
    title = "Distance to Nearest Crossing"
  ) +
  coord_equal() +
  coord_cartesian(xlim = c(0, 150), ylim = c(0, 150)) +
  theme_minimal()