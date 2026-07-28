library(sf)
library(here)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tigris)
library(tidyr)

day_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
sf_citation_data <- st_read(here("initial-analysis", "data-clean", "sf_citation_data","sf_citation_data3.shp"))
sd_citation_data1 <- read.csv(here("initial-analysis", "data-raw", "parking_citations_2025_part1_datasd.csv"))
sd_citation_data2 <- read.csv(here("initial-analysis", "data-raw", "parking_citations_2025_part2_datasd.csv"))
sd_citation_data3 <- read.csv(here("initial-analysis", "data-raw", "parking_citations_2026_part1_datasd (1).csv"))

sd_citation_data1 <- sd_citation_data1 |> 
  mutate(citation_id = as.character(citation_id))
sd_citation_data2 <- sd_citation_data2 |> 
  mutate(citation_id = as.character(citation_id))
sd_citation_data3 <- sd_citation_data3 |> 
  mutate(citation_id = as.character(citation_id))

sd_citation_data <- bind_rows(sd_citation_data1, sd_citation_data2, sd_citation_data3) |>
  filter(vio_code == "CVC 22500(n)")

# San Francisco County (same as citation data)
# City is the same geography as county for San Fran
sf_boundary <- counties(state = "CA", cb = TRUE, class = "sf") |> 
  dplyr::filter(NAME == "San Francisco")

# San Diego City (same as citation data)
sd_boundary <- places(state = "CA", cb = TRUE) |> 
  subset(NAME == "San Diego")

cal_crashes <- readRDS(here("initial-analysis", "data-clean", "02_TIMS_Geocoded.rds")) |>
  filter((PEDESTRIAN_ACCIDENT == "Y") & INTERSECTION == "Y")

cal_crashes <- st_transform(cal_crashes, 3310)
sf_boundary <- st_transform(sf_boundary, 3310)
sd_boundary <- st_transform(sd_boundary, 3310)


sf_crashes <- cal_crashes |> 
  st_filter(sf_boundary)

sd_crashes <- cal_crashes |>
  st_filter(sd_boundary)

# san francisco  ----------------------------------------------------------
# monthly
sf_citation_sum <- sf_citation_data |>
  mutate(
    date = ymd_hms(USER_Date_),
    day = day(date), 
    month = as.Date(floor_date(date, "month")), 
    year = year(date),
    dayofweek = weekdays(date)
  ) |>
  st_drop_geometry() |> 
  group_by(month) |>
  summarize(total_citations = n()) |>
  mutate(type = "AB-413 Citations")

sf_crash_sum <- sf_crashes |>
  mutate(
    date = ymd(COLLISION_DATE),
    month = as.Date(floor_date(date, "month"))
  ) |>
  st_drop_geometry() |>   
  group_by(month) |>
  summarize(total_crashes = n())|>
  mutate(type = "Pedestrian/Bike Crashes")


month_data <- left_join(sf_citation_sum, sf_crash_sum, by = "month")


scale_factor <- max(month_data$total_crashes, na.rm = TRUE) /
  max(month_data$total_citations, na.rm = TRUE)

ggplot(month_data, aes(month)) +
  geom_col(aes(y = total_crashes),
           fill = "steelblue") +
  geom_line(aes(y = total_citations * scale_factor),
            color = "red",
            linewidth = 1.2) +
  scale_y_continuous(
    name = "Pedestrian Intersection Crashes (Blue)",
    sec.axis = sec_axis(~ . / scale_factor,
                        name = "AB-413 Citations (Red)")
  ) +
  labs(
    title = "San Francisco Pedestrian Crashes and Citations",
    x = NULL
  )
  theme_minimal()


# daily
sf_citation_sum <- sf_citation_data |>
  mutate(
    date = ymd_hms(USER_Date_),
    day = as.Date(floor_date(date, "day"), 
                  month = as.Date(floor_date(date, "month")), 
                  year = year(date))) |>
  group_by(day) |>
  summarize(total_citations = n())


ggplot(sf_citation_sum, aes(x = day, y = total_citations)) + 
  geom_col(fill = "steelblue") + 
  scale_x_date(date_breaks = "3 month", date_labels = "%b %Y") + 
  labs(
    title = "San Francisco Daily AB-413 Ticket Citations",
    y = "Number of Citations",
    x = NULL
  ) + 
  theme_minimal()

# day of week
sf_citation_sum <- sf_citation_data |>
  group_by(dayofweek) |>
  summarize(total_citations = n()) |>
  mutate(dayofweek = factor(dayofweek, levels = day_order)) |>
  arrange(dayofweek)


ggplot(sf_citation_sum, aes(x = dayofweek, y = total_citations)) + 
  geom_col(fill = "steelblue") + 
  labs(
    title = "San Francisco AB-413 Ticket Citations by Day",
    y = "Number of Citations",
    x = NULL
  ) + 
  theme_minimal()

# san diego ---------------------------------------------------------------
# monthly
# sd_citation_data <- sd_citation_data |>
#   mutate(
#     date = ymd(date_issue),
#     day = day(date), 
#     month = floor_date(date, "month"), 
#     year = year(date),
#     dayofweek = weekdays(date))
# 
# sd_citation_sum <- sd_citation_data |>
#   group_by(month) |>
#   summarize(total_citations = n())

sd_citation_sum <- sd_citation_data |>
  mutate(
    date = ymd(date_issue),
    day = day(date), 
    month = as.Date(floor_date(date, "month")), 
    year = year(date),
    dayofweek = weekdays(date)
  ) |>
  st_drop_geometry() |> 
  group_by(month) |>
  summarize(total_citations = n()) |>
  mutate(type = "AB-413 Citations")

sd_crash_sum <- sd_crashes |>
  mutate(
    date = ymd(COLLISION_DATE),
    month = as.Date(floor_date(date, "month"))
  ) |>
  st_drop_geometry() |>   
  group_by(month) |>
  summarize(total_crashes = n())|>
  mutate(type = "Pedestrian/Bike Crashes")


month_data <- left_join(sd_citation_sum, sd_crash_sum, by = "month")

scale_factor <- max(month_data$total_crashes, na.rm = TRUE) /
  max(month_data$total_citations, na.rm = TRUE)

ggplot(month_data, aes(month)) +
  geom_col(aes(y = total_crashes),
           fill = "steelblue") +
  geom_line(aes(y = total_citations * scale_factor),
            color = "red",
            linewidth = 1.2) +
  scale_y_continuous(
    name = "Pedestrian Intersection Crashes (Blue)",
    sec.axis = sec_axis(~ . / scale_factor,
                        name = "AB-413 Citations (Red)")
  ) +
  labs(
    title = "San Diego Pedestrian Crashes and Citations",
    x = NULL
  )
theme_minimal()


# daily
sd_citation_sum <- sd_citation_data |>
  mutate(
    date = ymd(date_issue),
    day = floor_date(date, "day"), 
    month = floor_date(date, "month"), 
    year = year(date)) |>
  group_by(day) |>
  summarize(total_citations = n())



ggplot(sd_citation_sum, aes(x = day, y = total_citations)) + 
  geom_col(fill = "steelblue") + 
  scale_x_date(date_breaks = "3 month", date_labels = "%b %Y") + 
  labs(
    title = "San Diego Daily AB-413 Ticket Citations",
    y = "Number of Citations",
    x = NULL
  ) + 
  theme_minimal()

# day of week
sd_citation_sum <- sd_citation_data |>
  group_by(dayofweek) |>
  summarize(total_citations = n()) |>
  mutate(dayofweek = factor(dayofweek, levels = day_order)) |>
  arrange(dayofweek)


ggplot(sd_citation_sum, aes(x = dayofweek, y = total_citations)) + 
  geom_col(fill = "steelblue") + 
  labs(
    title = "San Diego AB-413 Ticket Citations by Day",
    y = "Number of Citations",
    x = NULL
  ) + 
  theme_minimal()

library(leaflet)
leaflet(data = sf_citation_data) |>
  addProviderTiles("CartoDB.Positron") |> 
  addCircleMarkers(
    lng = ~X,
    lat = ~Y,
    radius = 1,
    fillOpacity = .3 
  )
