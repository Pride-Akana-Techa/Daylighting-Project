### Combining csv files
library(tidyverse)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(leaflet)
library(leaflet.extras)


# combined_data <- list.files(path = "C:/Users/kylek/OneDrive/Desktop/TIMS_Crash_Data", 
#                             pattern = "\\.csv$", full.names = TRUE) |> 
#   map_df(~read_csv(.x, col_types = cols(.default = "c"))) |>
#   
#   write_csv(file.path("data/Crashes_California.csv"))

california_crashes <- read_csv("data/Crashes_California.csv")

california_crashes

colSums(is.na(california_crashes))

cal_filt <- california_crashes |>
  select(-OFFICER_ID, -REPORTING_DISTRICT, -CITY_DIVISION_LAPD, -BEAT_NUMBER,
         -DIRECTION, -STATE_HWY_IND, -CALTRANS_COUNTY, -CALTRANS_DISTRICT,
         -STATE_ROUTE, -ROUTE_SUFFIX, -POSTMILE_PREFIX, -POSTMILE, -LOCATION_TYPE,
         -RAMP_INTERSECTION, -SIDE_OF_HWY, -TOW_AWAY, -PCF_CODE_OF_VIOL, -PCF_VIOLATION,
         -PCF_VIOL_SUBSECTION, -LATITUDE, -LONGITUDE)

colSums(is.na(cal_filt))


# filling column values that are NA with N or -
# cal_filt$PEDESTRIAN_ACCIDENT <- cal_filt$PEDESTRIAN_ACCIDENT[is.na(cal_filt$PEDESTRIAN_ACCIDENT)] <- "N"
# cal_filt$BICYCLE_ACCIDENT <- cal_filt$BICYCLE_ACCIDENT[is.na(cal_filt$BICYCLE_ACCIDENT)] <- "N"
# cal_filt$MOTORCYCLE_ACCIDENT <- cal_filt$MOTORCYCLE_ACCIDENT[is.na(cal_filt$MOTORCYCLE_ACCIDENT)] <- "N"
# cal_filt$TRUCK_ACCIDENT <- cal_filt$TRUCK_ACCIDENT[is.na(cal_filt$TRUCK_ACCIDENT)] <- "N"
# cal_filt$ALCOHOL_INVOLVED <- cal_filt$ALCOHOL_INVOLVED[is.na(cal_filt$ALCOHOL_INVOLVED)] <- "N"
# cal_filt$CHP_VEHTYPE_AT_FAULT <- cal_filt$CHP_VEHTYPE_AT_FAULT[is.na(cal_filt$CHP_VEHTYPE_AT_FAULT)] <- "-"
# cal_filt$SECONDARY_RD <- cal_filt$SECONDARY_RD[is.na(cal_filt$SECONDARY_RD)] <- "-"
# cal_filt$PARTY_COUNT <- cal_filt$PARTY_COUNT[is.na(cal_filt$PARTY_COUNT)] <- "-"
# cal_filt$PED_ACTION <- cal_filt$PED_ACTION[is.na(cal_filt$PED_ACTION)] <- "-"
# cal_filt$TYPE_OF_COLLISION <- cal_filt$TYPE_OF_COLLISION[is.na(cal_filt$TYPE_OF_COLLISION)] <- "-"
# cal_filt$LIGHTING <- cal_filt$LIGHTING[is.na(cal_filt$LIGHTING)] <- "-"
# 
# cal_filt$POINT_X <- cal_filt$POINT_X[is.na(cal_filt$POINT_X)] <- "-"
# cal_filt$POINT_Y <- cal_filt$POINT_Y[is.na(cal_filt$POINT_Y)] <- "-"


cal_geo <- cal_filt %>%
  mutate(
    POINT_X = as.numeric(as.character(POINT_X)),
    POINT_Y = as.numeric(as.character(POINT_Y))
  ) %>%
  filter(!is.na(POINT_X), !is.na(POINT_Y))

cal_sf <- st_as_sf(cal_geo, coords = c("POINT_X", "POINT_Y"), crs = 26911)

cal_wgs84 <- st_transform(cal_sf, 4326)

# leaflet(cal_wgs84) %>%
#   addTiles() %>%
#   addCircleMarkers(
#     radius = 5,
#     color = "blue",
#     stroke = FALSE,
#     fillOpacity = 0.6,
#     clusterOptions = markerClusterOptions()
#   )

