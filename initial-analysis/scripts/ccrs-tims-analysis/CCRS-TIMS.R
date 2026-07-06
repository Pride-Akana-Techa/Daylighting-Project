# load packages
library(tidyverse)
library(sf)

# Read Datasets
tims_crashes <- readRDS("initial-analysis/scripts/TIMS_Filtered.rds")
ccrs_crashes <- read_csv("initial-analysis/scripts/updated-crashes.csv")


# Rename the Variables and Standardize the Case ID column
ccrs_clean <- ccrs_crashes |> 
  rename(
    case_id =  `Collision Id`,
    report_number = `Report Number`,
    report_version = `Report Version`,
    is_preliminary = `Is Preliminary`,
    NCIC_code = `NCIC Code`,
    collision_time = `Crash Time Description`,
    city_id = `City Id`,
    city_code = `City Code`,
    city_name =`City Name`,
    county_code = `County Code`,
    city_active = `City Is Active`,
    city_incorporated = `City Is Incorporated`,
    collision_type_code = `Collision Type Code`,
    collision_type_description = `Collision Type Description`,
    day = `Day Of Week`,
    highway_related = IsHighwayRelated,
    judicial_district = JudicialDistrict,
    vehicle_involved_code = MotorVehicleInvolvedWithCode,
    num_injured = NumberInjured,
    num_killed = NumberKilled,
    weather = `Weather 1`,
    road_condition = `Road Condition 1`,
    lighting_code =LightingCode,
    lighting = LightingDescription,
    latitude = Latitude,
    longitude = Longitude,
    ped_action = PedestrianActionCode,
    prepared_date = PreparedDate,
    pcf_vol_category = `Primary Collision Factor Violation`,
    primary_rd = PrimaryRoad,
    reviewed_date = ReviewedDate,
    rd_surface_code = RoadwaySurfaceCode,
    seconadry_direction = SecondaryDirection,
    secondary_distance = SecondaryDistance,
    secondary_rd = SecondaryRoad,
    traffic_control_device_code = TrafficControlDeviceCode,
    created_date = CreatedDate,
    modified_date = ModifiedDate,
    CHP555_version = CHP555Version,
    collision_date = Modified_time,
    month = Month,
    year = Year
    
  ) |> 

  mutate(latitude_clean = latitude,
         longitude_clean = longitude,
         time_clean = collision_time,
         date_str = as.character(collision_date),
         date_short = substr(date_str, 1, 10),
         date_clean = ymd(parse_date_time(date_short, orders = c("ymd", "mdy", "dmy"))))

tims_clean <- tims_crashes |> 
  rename(
    case_id = CASE_ID,
    year = ACCIDENT_YEAR,
    collision_date = COLLISION_DATE,
    collision_time = COLLISION_TIME,
    day = DAY_OF_WEEK,
    pop = POPULATION,
    primary_rd = PRIMARY_RD,
    secondary_rd = SECONDARY_RD,
    secondary_distance = DISTANCE,
    intersection = INTERSECTION,
    weather = WEATHER_1,
    state_hwy = STATE_HWY_IND,
    collision_severity = COLLISION_SEVERITY,
    num_killed = NUMBER_KILLED,
    num_injured = NUMBER_INJURED,
    party_count = PARTY_COUNT,
    pcf_vol_category = PCF_VIOL_CATEGORY,
    ped_action = PED_ACTION,
    road_surface = ROAD_SURFACE,
    lighting = LIGHTING,
    ped_accident = PEDESTRIAN_ACCIDENT,
    bicycle_accident = BICYCLE_ACCIDENT,
    alcohol_invloved = ALCOHOL_INVOLVED,
    count_severe_inj = COUNT_SEVERE_INJ,
    count_complaint_pain = COUNT_COMPLAINT_PAIN,
    num_ped_killed = COUNT_PED_KILLED,
    num_ped_injured = COUNT_PED_INJURED,
    num_bicyclist_killed = COUNT_BICYCLIST_KILLED,
    num_bicyclist_injured = COUNT_BICYCLIST_INJURED,
    county = COUNTY,
    city_name = CITY,
    longitude = POINT_X,
    latitude = POINT_Y,
  ) |> 
  
  mutate(latitude_clean = latitude,
         longitude_clean = longitude,
         time_clean = collision_time,
         date_str = as.character(collision_date),
         date_short = substr(date_str, 1, 10),
         date_clean = ymd(parse_date_time(date_short, orders = c("ymd", "mdy", "dmy"))))


# filter out missing lats, longs, dates, and time
ccrs_spatial_prep <- ccrs_clean |> 
  filter_out(is.na(latitude_clean)) |> 
  filter_out(is.na(longitude_clean)) |> 
  filter_out(is.na(date_clean)) |> 
  filter_out(is.na(time_clean))

tims_spatial_prep <- tims_clean |> 
  filter_out(is.na(latitude_clean)) |> 
  filter_out(is.na(longitude_clean)) |> 
  filter_out(is.na(date_clean)) |> 
  filter_out(is.na(time_clean))

# Convert raw datasets to geographic spatial objects
ccrs_sf <- st_as_sf(ccrs_spatial_prep, coords = c("longitude_clean", "latitude_clean"), crs = 4326)
tims_sf <- st_as_sf(tims_spatial_prep, coords = c("longitude_clean", "latitude_clean"), crs = 4326)


# Transform to NAD83 / California zone 3 (EPSG:2227 or 3310 for statewide)
# so R can calculate distances in feet/meters instead of degrees.
ccrs_projected <- st_transform(ccrs_sf, crs = 3310)
tims_projected <- st_transform(tims_sf, crs = 3310)


# Spatial proximity join with a max distance threshold

library(data.table)

# find common date/time combos
ccrs_dt <- as.data.table(st_drop_geometry(ccrs_projected))
tims_dt <- as.data.table(st_drop_geometry(tims_projected))

common_times <- intersect(
  ccrs_dt[, paste(date_clean, time_clean)],
  tims_dt[, paste(date_clean, time_clean)]
)

# filter by overlapping dates and time
ccrs_filtered <- ccrs_projected |>
  filter(paste(date_clean, time_clean) %in% common_times)

ccrs_filtered |> 
  write_csv(file = "initial-analysis/scripts/filterd-ccrs.csv")

tims_filtered <- tims_projected |>
  filter(paste(date_clean, time_clean) %in% common_times)

tims_filtered |> 
  write_csv(file = "initial-analysis/scripts/filterd-tims.csv")

# spatial join
spatial_matches <- st_join(
  ccrs_filtered,
  tims_filtered,
  join = st_is_within_distance,
  dist = 100,
  suffix = c("_ccrs", "_tims")
)

# Calculate how many meters apart the "matching" events actually are
distances <- st_distance(spatial_matches, tims_projected[st_nearest_feature(spatial_matches, tims_projected), ], by_element = TRUE)
spatial_matches$shift_distance_feet <- as.numeric(distances) * 3.28084


# print results
cat("\n=== SPATIAL-DATE OVERLAP ===\n")
cat("Total matching crashes found (Same Date and Time + Within 100m):", nrow(spatial_matches), "\n")
cat("------------------------------------------------------\n")
print(summary(spatial_matches$shift_distance_feet))

