# San Jose schools zones

library(tigris)
library(sf)
library(here)

# load san jose 
san_jose_boundary <- places(state = "CA", cb = TRUE) |>
  filter(NAME == "San Jose") |>
  st_transform(crs = 3310)


# load and filter school data for just san jose 
gdb_path <- "initial-analysis/data-raw/CaliforniaSchools/CSCD_2025.gdb"


# only contains public schools (239 obs)
# i think that they said that there are approx 400 schools in San Jose
san_jose_schools <- st_read(dsn = gdb_path, layer = "Schools_Current_Stacked") |>
  st_transform(crs = 3310) |>
  st_filter(san_jose_boundary)

san_jose_school_zones <- st_buffer(san_jose_schools, dist = units::set_units(600, "ft"))|>
  st_union()


san_jose_crashes <- readRDS(here("initial-analysis", "data-clean", "02_TIMS_Geocoded.rds")) |>
  st_filter(san_jose_boundary) |>
  st_transform(crs = 3310)

schoolzone_crashes <- san_jose_crashes |>
  mutate(
    crash_date = as.Date(COLLISION_DATE), 
    in_school_zone = lengths(st_intersects(geometry, san_jose_school_zones)) > 0,
    policy_period = case_when(
      crash_date < as.Date("2024-01-01") ~ "Pre-Policy (Before 2024)",
      crash_date >= as.Date("2024-01-01") & crash_date < as.Date("2025-01-01") ~ "Warnings (1/1/2024)",
      crash_date >= as.Date("2025-01-01") ~ "Citations (1/1/2025)",
    ),
    policy_number = case_when(
      crash_date < as.Date("2024-01-01") ~ 0,
      crash_date >= as.Date("2024-01-01") & crash_date < as.Date("2025-01-01") ~ 1,
      crash_date >= as.Date("2025-01-01") ~ 2,
    )
  )

# aggregate by month
san_jose_monthly_crashes <- schoolzone_crashes |>
  st_drop_geometry() |>
  mutate(month_date = floor_date(as.Date(COLLISION_DATE), "month")) |>
  filter(year(month_date) >= 2021 & year(month_date) <= 2025) |> 
  group_by(month_date, in_school_zone) |>
  summarise(crashes = n(), .groups = "drop") |>
  mutate(
    zone_label = if_else(in_school_zone, "School Zone (Within 500ft)", "Control (Outside School Zone)"),
    zone_number = if_else(in_school_zone, 1, 0))
    
ped_crashes <- schoolzones_crashes|>
  filter(PEDESTRIAN_ACCIDENT =="Y")

# there are not that many pedestrian observations ngl 

san_jose_ped_monthly_crashes <- ped_crashes |>
  st_drop_geometry() |>
  mutate(month_date = floor_date(as.Date(COLLISION_DATE), "month")) |>
  filter(year(month_date) >= 2021 & year(month_date) <= 2025) |> 
  group_by(month_date, in_school_zone) |>
  summarise(crashes = n(), .groups = "drop") |>
  mutate(
    zone_label = if_else(in_school_zone, "School Zone (Within 500ft)", "Control (Outside School Zone)"),
    zone_number = if_else(in_school_zone, 1, 0))
