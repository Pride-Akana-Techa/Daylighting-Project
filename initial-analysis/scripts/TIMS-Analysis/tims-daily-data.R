### 
library(tidyverse)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(leaflet)
library(leaflet.extras)
library(lubridate)
library(here)
library(readr)

california_crashes <- read_csv(here("initial-analysis", "data-raw", "TIMS_Crashes_California.csv"))
#saveRDS(california_crashes, "TIMS_California_Crashes.rds")

###### Dates #######
year_start <- as.Date(c("2014-01-01", "2015-01-01", "2016-01-01", "2017-01-01", "2018-01-01",
                        "2019-01-01", "2020-01-01", "2021-01-01", "2022-01-01", "2023-01-01",
                        "2024-01-01", "2025-01-01", "2026-01-01"))
daylight_warn_date <- as.Date(c("2024-01-01"))
daylight_enforc_date <- as.Date(c("2025-01-01"))

###### Data Filtering ####


cal_filt <- california_crashes |>
  select(-OFFICER_ID, -REPORTING_DISTRICT, -CITY_DIVISION_LAPD, -BEAT_NUMBER,
         -DIRECTION, -CALTRANS_COUNTY, -CALTRANS_DISTRICT,
         -STATE_ROUTE, -ROUTE_SUFFIX, -POSTMILE_PREFIX, -POSTMILE, -LOCATION_TYPE,
         -RAMP_INTERSECTION, -SIDE_OF_HWY, -TOW_AWAY,-PRIMARY_COLL_FACTOR, -PCF_CODE_OF_VIOL, -PCF_VIOLATION,
         -PCF_VIOL_SUBSECTION, -HIT_AND_RUN, -TYPE_OF_COLLISION, -CHP_SHIFT,
         -MVIW, -LATITUDE, -LONGITUDE, -ROAD_COND_1, -ROAD_COND_2, -CONTROL_DEVICE,
         -CHP_ROAD_TYPE, -MOTORCYCLE_ACCIDENT,-TRUCK_ACCIDENT, -NOT_PRIVATE_PROPERTY,
         -STWD_VEHTYPE_AT_FAULT, -CHP_VEHTYPE_AT_FAULT, -COUNT_MC_KILLED,
         -COUNT_MC_INJURED, -PRIMARY_RAMP, -SECONDARY_RAMP, -JURIS,
         -PROC_DATE, -CNTY_CITY_LOC, -BEAT_TYPE, -CHP_BEAT_TYPE, -SPECIAL_COND,
         -CHP_BEAT_CLASS, -WEATHER_2) |>
  distinct(CASE_ID, .keep_all = TRUE)

# filling column values that are NA with N or - #
cal_filt$PEDESTRIAN_ACCIDENT[is.na(cal_filt$PEDESTRIAN_ACCIDENT)] <- "N"
cal_filt$BICYCLE_ACCIDENT[is.na(cal_filt$BICYCLE_ACCIDENT)] <- "N"
cal_filt$ALCOHOL_INVOLVED[is.na(cal_filt$ALCOHOL_INVOLVED)] <- "N"

cal_filt$SECONDARY_RD[is.na(cal_filt$SECONDARY_RD)] <- "-"
cal_filt$PARTY_COUNT[is.na(cal_filt$PARTY_COUNT)] <- "-"
cal_filt$PED_ACTION[is.na(cal_filt$PED_ACTION)] <- "-"
cal_filt$LIGHTING[is.na(cal_filt$LIGHTING)] <- "-"


#saveRDS(cal_filt, "TIMS_Filtered.rds")


###### Deaths by Day ######
daily_data <- cal_filt %>%
  mutate(day = floor_date(COLLISION_DATE, "day")) %>%
  group_by(day, day_of_week = DAY_OF_WEEK) %>%
  summarize(
    ped_killed = sum(COUNT_PED_KILLED, na.rm = TRUE),
    ped_injured = sum(COUNT_PED_INJURED, na.rm = TRUE),
    bike_killed = sum(COUNT_BICYCLIST_KILLED, na.rm = TRUE),
    bike_injured = sum(COUNT_BICYCLIST_INJURED, na.rm = TRUE),
    killed = sum(COUNT_PED_KILLED, na.rm = TRUE) +
      sum(COUNT_BICYCLIST_KILLED, na.rm = TRUE),
    injured = sum(COUNT_BICYCLIST_INJURED, na.rm = TRUE) +
      sum(COUNT_PED_INJURED, na.rm = TRUE),
    
    ped_killed_int = sum(COUNT_PED_KILLED[INTERSECTION == "Y"], na.rm = TRUE),
    ped_injured_int = sum(COUNT_PED_INJURED[INTERSECTION == "Y"], na.rm = TRUE),
    bike_killed_int = sum(COUNT_BICYCLIST_KILLED[INTERSECTION == "Y"], na.rm = TRUE),
    bike_injured_int = sum(COUNT_BICYCLIST_INJURED[INTERSECTION == "Y"], na.rm = TRUE),
    
    killed_int = sum(COUNT_PED_KILLED[INTERSECTION == "Y"], na.rm = TRUE) +
      sum(COUNT_BICYCLIST_KILLED[INTERSECTION == "Y"], na.rm = TRUE),
    
    injured_int = sum(COUNT_PED_INJURED[INTERSECTION == "Y"], na.rm = TRUE) +
      sum(COUNT_BICYCLIST_INJURED[INTERSECTION == "Y"], na.rm = TRUE),
    
    ped_killed_nonint = sum(COUNT_PED_KILLED[INTERSECTION == "N"], na.rm = TRUE),
    ped_injured_nonint = sum(COUNT_PED_INJURED[INTERSECTION == "N"], na.rm = TRUE),
    bike_killed_nonint = sum(COUNT_BICYCLIST_KILLED[INTERSECTION == "N"], na.rm = TRUE),
    bike_injured_nonint = sum(COUNT_BICYCLIST_INJURED[INTERSECTION == "N"], na.rm = TRUE),
    
    killed_nonint = sum(COUNT_PED_KILLED[INTERSECTION == "N"], na.rm = TRUE) +
      sum(COUNT_BICYCLIST_KILLED[INTERSECTION == "N"], na.rm = TRUE),
    
    injured_nonint = sum(COUNT_PED_INJURED[INTERSECTION == "N"], na.rm = TRUE) +
      sum(COUNT_BICYCLIST_INJURED[INTERSECTION == "N"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(day)

ggplot(daily_data, aes(x = day, y = ped_killed)) +
  stat_summary(
    aes(x = floor_date(day, "month")),
    fun = sum, geom = "line", group = 1) + 
  # stat_summary(aes(x = floor_date(day, "month"), y = ped_killed_int), 
  #              fun = sum, geom = "line", group = 1, color = "blue") +
  # stat_summary(aes(x = floor_date(day, "month"), y = ped_killed_nonint), 
  #              fun = sum, geom = "line", group = 1, color = "red") +
  geom_vline(xintercept = year_start,
             linetype = "dashed", color = "grey", size = .5) +
  geom_vline(xintercept = daylight_warn_date,
             color = "blue", size = 2, alpha = .5) +
  geom_vline(xintercept = daylight_enforc_date,
             color = "green", size = 2, alpha = .5) +
  annotate("rect",
           xmin = as.Date("2024-01-01"),
           xmax = as.Date("2025-01-01"),
           ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "lightblue") +
  annotate("rect",
           xmin = as.Date("2025-01-01"),
           xmax = as.Date("2026-01-01"),
           ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "lightgreen") +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b %Y") +
  theme_minimal()


monthly_table <- daily_data %>%
  mutate(month = floor_date(day, "month")) %>%
  group_by(month) %>%
  summarize(
    killed = sum(killed, na.rm = TRUE),
    ped_killed = sum(ped_killed, na.rm = TRUE),
    ped_injured = sum(ped_injured, na.rm = TRUE),
    bike_killed = sum(bike_killed, na.rm = TRUE),
    bike_injured = sum(bike_injured, na.rm = TRUE),
    injured = sum(injured, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(month)

monthly_table


dow_data <- cal_filt %>%
  group_by(ACCIDENT_YEAR, day_of_week = DAY_OF_WEEK) %>%
  summarize(
    ped_killed = sum(COUNT_PED_KILLED, na.rm = TRUE),
    bike_killed = sum(COUNT_BICYCLIST_KILLED, na.rm = TRUE)
  )


dow_data <- dow_data %>%
  mutate(day_of_week_name = factor(
    day_of_week, 
    levels = 1:7, 
    labels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  ))


# ggplot(dow_data, aes(fill = ACCIDENT_YEAR, x = day_of_week_name, 
#                      y = ped_killed, 
#                      group = factor(ACCIDENT_YEAR))) +
#   geom_bar(position="dodge", stat="identity")

