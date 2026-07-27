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
    
ped_crashes <- schoolzone_crashes|>
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



# RDiT model for Crashes in School Zones ---------------------------------------

## Jan 2024 Cutoff
# Prepare data for model
in_school_rdit <- san_jose_ped_monthly_crashes |> 
  filter(in_school_zone == "TRUE") |> 
  mutate(Year = year(month_date),
         MONTH = floor_date(ymd(month_date), "month"),
         Time = interval(as.Date("2024-01-01"), MONTH) %/% months(1),
         Post = ifelse(Time >= 0, 1, 0),
         Season_factor = factor(
           case_when(
             month(MONTH) %in% c(12, 1, 2) ~ "Winter",
             month(MONTH) %in% c(3, 4, 5) ~ "Spring",
             month(MONTH) %in% c(6, 7, 8) ~ "Summer",
             month(MONTH) %in% c(9, 10, 11) ~ "Fall"
           ),
           levels = c("Winter", "Spring", "Summer", "Fall")
         )) |> 
  filter(Year %in% c(2023, 2024)) |> 
  select(Time, Post, Season_factor, crashes) 


# Standardizing crash outcomes
pre_mean <- in_school_rdit |>
  filter(Post == 0) |>
  summarise(mean = mean(crashes)) |>
  pull(mean)

pre_sd <- in_school_rdit |>
  filter(Post == 0) |>
  summarise(sd = sd(crashes)) |>
  pull(sd)

in_school_rdit <- in_school_rdit |>
  mutate(Crash_std = (crashes - pre_mean) / pre_sd)


# Run model
in_school_zone_model <- rdrobust(y = in_school_rdit$Crash_std,
                              x = in_school_rdit$Time,
                              covs = model.matrix(~ Season_factor, in_school_rdit)[, -1],
                              c = 0,
                              p = 1,
                              h = 12,
                              kernel = "triangular")

summary(in_school_zone_model)


## Jan 2025 Cutoff
# Prepare data for model
in_school1_rdit <- san_jose_ped_monthly_crashes |> 
  filter(in_school_zone == "TRUE") |> 
  mutate(Year = year(month_date),
         MONTH = floor_date(ymd(month_date), "month"),
         Time = interval(as.Date("2025-01-01"), MONTH) %/% months(1),
         Post = ifelse(Time >= 0, 1, 0),
         Season_factor = factor(
           case_when(
             month(MONTH) %in% c(12, 1, 2) ~ "Winter",
             month(MONTH) %in% c(3, 4, 5) ~ "Spring",
             month(MONTH) %in% c(6, 7, 8) ~ "Summer",
             month(MONTH) %in% c(9, 10, 11) ~ "Fall"
           ),
           levels = c("Winter", "Spring", "Summer", "Fall")
         )) |> 
  filter(Year %in% c(2024, 2025)) |> 
  select(Time, Post, Season_factor, crashes) 


# Standardizing crash outcomes
pre_mean <- in_school1_rdit |>
  filter(Post == 0) |>
  summarise(mean = mean(crashes)) |>
  pull(mean)

pre_sd <- in_school1_rdit |>
  filter(Post == 0) |>
  summarise(sd = sd(crashes)) |>
  pull(sd)

in_school1_rdit <- in_school1_rdit |>
  mutate(Crash_std = (crashes - pre_mean) / pre_sd)


# Run model
in_school_zone1_model <- rdrobust(y = in_school1_rdit$Crash_std,
                                 x = in_school1_rdit$Time,
                                 covs = model.matrix(~ Season_factor, in_school1_rdit)[, -1],
                                 c = 0,
                                 p = 1,
                                 h = 12,
                                 kernel = "triangular")

summary(in_school_zone1_model)


# RDiT model for Crashes in School Zones ---------------------------------------

## Jan 2024 Cutoff
# Prepare data for model
out_school_rdit <- san_jose_ped_monthly_crashes |> 
  filter(in_school_zone == "FALSE") |> 
  mutate(Year = year(month_date),
         MONTH = floor_date(ymd(month_date), "month"),
         Time = interval(as.Date("2024-01-01"), MONTH) %/% months(1),
         Post = ifelse(Time >= 0, 1, 0),
         Season_factor = factor(
           case_when(
             month(MONTH) %in% c(12, 1, 2) ~ "Winter",
             month(MONTH) %in% c(3, 4, 5) ~ "Spring",
             month(MONTH) %in% c(6, 7, 8) ~ "Summer",
             month(MONTH) %in% c(9, 10, 11) ~ "Fall"
           ),
           levels = c("Winter", "Spring", "Summer", "Fall")
         )) |> 
  filter(Year %in% c(2023, 2024)) |> 
  select(Time, Post, Season_factor, crashes) 


# Standardizing crash outcomes
pre_mean <- out_school_rdit |>
  filter(Post == 0) |>
  summarise(mean = mean(crashes)) |>
  pull(mean)

pre_sd <- out_school_rdit |>
  filter(Post == 0) |>
  summarise(sd = sd(crashes)) |>
  pull(sd)

out_school_rdit <- out_school_rdit |>
  mutate(Crash_std = (crashes - pre_mean) / pre_sd)


# Run model
out_school_zone_model <- rdrobust(y = out_school_rdit$Crash_std,
                                 x = out_school_rdit$Time,
                                 covs = model.matrix(~ Season_factor, out_school_rdit)[, -1],
                                 c = 0,
                                 p = 1,
                                 h = 12,
                                 kernel = "triangular")

summary(out_school_zone_model)


## Jan 2025 Cutoff
# Prepare data for model
out_school1_rdit <- san_jose_ped_monthly_crashes |> 
  filter(in_school_zone == "FALSE") |> 
  mutate(Year = year(month_date),
         MONTH = floor_date(ymd(month_date), "month"),
         Time = interval(as.Date("2025-01-01"), MONTH) %/% months(1),
         Post = ifelse(Time >= 0, 1, 0),
         Season_factor = factor(
           case_when(
             month(MONTH) %in% c(12, 1, 2) ~ "Winter",
             month(MONTH) %in% c(3, 4, 5) ~ "Spring",
             month(MONTH) %in% c(6, 7, 8) ~ "Summer",
             month(MONTH) %in% c(9, 10, 11) ~ "Fall"
           ),
           levels = c("Winter", "Spring", "Summer", "Fall")
         )) |> 
  filter(Year %in% c(2024, 2025)) |> 
  select(Time, Post, Season_factor, crashes) 


# Standardizing crash outcomes
pre_mean <- out_school1_rdit |>
  filter(Post == 0) |>
  summarise(mean = mean(crashes)) |>
  pull(mean)

pre_sd <- out_school1_rdit |>
  filter(Post == 0) |>
  summarise(sd = sd(crashes)) |>
  pull(sd)

out_school1_rdit <- out_school1_rdit |>
  mutate(Crash_std = (crashes - pre_mean) / pre_sd)


# Run model
out_school_zone1_model <- rdrobust(y = out_school1_rdit$Crash_std,
                                  x = out_school1_rdit$Time,
                                  covs = model.matrix(~ Season_factor, out_school1_rdit)[, -1],
                                  c = 0,
                                  p = 1,
                                  h = 12,
                                  kernel = "triangular")

summary(out_school_zone1_model)


