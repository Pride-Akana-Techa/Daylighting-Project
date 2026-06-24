
# -------------------------------------------------------------------------
# 01_TIMS_cleaning_data
# Code to combine individual county TIMS downloads into statewide
# California dataset. 
# County level data downloaded and combined on 6/3/2026
# Cleans data by removing unused columns and projects coordinates to
# coordinate system 

# -------------------------------------------------------------------------
# load libraries

library(tidyverse)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(leaflet)
library(leaflet.extras)
library(lubridate)

# -------------------------------------------------------------------------
# Data Combining 

# combines county data into statewide dataset and saves as Crashes_California

# combined_data <- list.files(path = "C:/Users/kylek/OneDrive/Desktop/TIMS_Crash_Data", 
#                             pattern = "\\.csv$", full.names = TRUE) |> 
#   map_df(~read_csv(.x, col_types = cols(.default = "c"))) |>
#   
#   write_csv(file.path("initial-analysis/data-raw/TIMS_Crashes_California.csv"))


# loads in california_crashes 
california_crashes <- read_csv("initial-analysis/data-raw/TIMS_Crashes_California.csv")

# -------------------------------------------------------------------------
#Data Filtering

# Remove columns that are not needed for study, removes observations
# that have duplicate CASE_ID
cal_filt <- california_crashes |>
  select(-OFFICER_ID, -REPORTING_DISTRICT, -CITY_DIVISION_LAPD, 
         -BEAT_NUMBER, -DIRECTION, -CALTRANS_COUNTY, -CALTRANS_DISTRICT,
         -STATE_ROUTE, -ROUTE_SUFFIX, -POSTMILE_PREFIX, -POSTMILE, 
         -LOCATION_TYPE, -RAMP_INTERSECTION, -SIDE_OF_HWY, -TOW_AWAY,
         -PRIMARY_COLL_FACTOR, -PCF_CODE_OF_VIOL, -PCF_VIOLATION,
         -PCF_VIOL_SUBSECTION, -HIT_AND_RUN, -TYPE_OF_COLLISION, 
         -CHP_SHIFT, -MVIW, -LATITUDE, -LONGITUDE, -ROAD_COND_1, 
         -ROAD_COND_2, -CONTROL_DEVICE,-CHP_ROAD_TYPE, -MOTORCYCLE_ACCIDENT,
         -TRUCK_ACCIDENT, -NOT_PRIVATE_PROPERTY, -STWD_VEHTYPE_AT_FAULT, 
         -CHP_VEHTYPE_AT_FAULT, -COUNT_MC_KILLED, -COUNT_MC_INJURED, 
         -PRIMARY_RAMP, -SECONDARY_RAMP, -JURIS, -PROC_DATE, 
         -CNTY_CITY_LOC, -BEAT_TYPE, -CHP_BEAT_TYPE, -SPECIAL_COND,
         -CHP_BEAT_CLASS, -WEATHER_2) |>
  distinct(CASE_ID, .keep_all = TRUE) 

# filling column values that are NA with N
# these columns previously only had "Y" or NULL 
cal_filt$PEDESTRIAN_ACCIDENT[is.na(cal_filt$PEDESTRIAN_ACCIDENT)] <- "N"
cal_filt$BICYCLE_ACCIDENT[is.na(cal_filt$BICYCLE_ACCIDENT)] <- "N"
cal_filt$ALCOHOL_INVOLVED[is.na(cal_filt$ALCOHOL_INVOLVED)] <- "N"

# filling column values that are NA with -
# these columns had values that were missing at random 
cal_filt$SECONDARY_RD[is.na(cal_filt$SECONDARY_RD)] <- "-"
cal_filt$PARTY_COUNT[is.na(cal_filt$PARTY_COUNT)] <- "-"
cal_filt$PED_ACTION[is.na(cal_filt$PED_ACTION)] <- "-"
cal_filt$LIGHTING[is.na(cal_filt$LIGHTING)] <- "-"

# save the filtered TIMS data as .rds
saveRDS(cal_filt, "initial-analysis/data-clean/01_TIMS_Cleaned.rds")


# -------------------------------------------------------------------------
# project to coordinate reference system 

#  Filter out observations that do not contain coordinates 
# 54,308 NA coordinates out of 2,090,203 do not have coordinates 
cal_geo <- cal_filt |>
  mutate(
    POINT_X = as.numeric(as.character(POINT_X)),
    POINT_Y = as.numeric(as.character(POINT_Y))) |>
  filter(!is.na(POINT_X), !is.na(POINT_Y))

# initialize using 4269 - NAD83 
cal_nad83 <- st_as_sf(cal_geo, coords = c("POINT_X", "POINT_Y"), 
                                crs = 4269)

# transform to 3310 - California Albers
cal_albers <- st_transform(cal_nad83, 3310)

saveRDS(cal_albers, "initial-analysis/data-clean/02_TIMS_Geocoded.rds")
