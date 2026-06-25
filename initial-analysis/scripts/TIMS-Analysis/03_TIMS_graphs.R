# -------------------------------------------------------------------------
# 03_TIMS_graphs
# graphs of pedestrian and bicyclist injuries and fatalities  
# -------------------------------------------------------------------------
# load libraries
library(tidyverse)
library(lubridate)
library(scales)
library(patchwork)

# -------------------------------------------------------------------------
# Graphs formating and colors 

CLR_PED      <- "#C0392B"   
CLR_BIKE     <- "#2980B9"   
CLR_START   <- "#E67E22"  
CLR_ENFORCE  <- "#27AE60"   


daylight_warn_date <- as.Date(c("2024-01-01"))
daylight_enforce_date <- as.Date(c("2025-01-01"))

policy_poly <- function() {
  list(
    annotate("rect",
             xmin = as.Date("2024-01-01"), as.Date("2025-01-01"),
             ymin = -Inf, ymax = Inf,
             fill = "#FEF9E7", alpha = 1),
    annotate("rect",
             xmin = as.Date("2025-01-01"), as.Date("2026-01-01"),
             ymin = -Inf, ymax = Inf,
             fill = "#EAFAF1", alpha = 1)
  )
}

policy_vlines <- function(warn_date, enforce_date) {
  list(
    geom_vline(xintercept = warn_date,
               color = CLR_START, linewidth = 1, linetype = "dashed"),
    geom_vline(xintercept = enforce_date,
               color = CLR_ENFORCE, linewidth = 1, linetype = "dashed"),
    annotate("text", x = warn_date + days(5), y = Inf,
             label = "Warning\nStart\nDate", vjust = 1.3, hjust = 0,
             size = 4, color = CLR_START, fontface = "bold"),
    annotate("text", x = enforce_date + days(5), y = Inf,
             label = "Ticketing\nStart\nDate", vjust = 1.3, hjust = 0,
             size = 4, color = CLR_ENFORCE, fontface = "bold")
  )
}

# graphs theme
theme_safety <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title        = element_text(face = "bold", size = base_size + 2, hjust = 0),
      plot.subtitle     = element_text(color = "grey40", size = base_size - 1, hjust = 0,
                                       margin = margin(b = 8)),
      plot.caption      = element_text(color = "grey55", size = 9, hjust = 0,
                                       margin = margin(t = 8)),
      axis.title        = element_text(size = base_size - 1, color = "grey30"),
      axis.text.x       = element_text(angle = 45, hjust = 1, size = 9),
      axis.text.y       = element_text(size = 10),
      panel.grid.major  = element_line(color = "grey92", linewidth = 0.4),
      panel.grid.minor  = element_blank(),
      legend.position   = "bottom",
      legend.title      = element_blank(),
      legend.text       = element_text(size = 10),
      strip.text        = element_text(face = "bold", size = base_size),
      strip.background  = element_rect(fill = "grey96", color = NA),
      plot.background   = element_rect(fill = "white", color = NA),
      panel.background  = element_rect(fill = "white", color = NA),
      plot.margin       = margin(16, 16, 12, 16)
    )
}

# -------------------------------------------------------------------------
# Function to aggregate data by month 
monthly <- function(data, ...) {
  vars <- enquos(...)
  data %>%
    mutate(month = floor_date(day, "month")) %>%
    group_by(month) %>%
    summarise(across(c(!!!vars), sum, .names = "{.col}"), .groups = "drop")
}


monthly_deaths   <- monthly(daily_data, ped_killed, bike_killed)
monthly_injuries <- monthly(daily_data, ped_injured, bike_injured)


annual_data <- daily_data %>%
  mutate(year_start = floor_date(day, "year")) %>%
  group_by(year_start) %>%
  summarise(annual_avg_ped = sum(ped_killed, na.rm = TRUE) / 12,
            annual_avg_bike = sum(bike_killed, na.rm = TRUE) / 12,
            .groups = "drop")

# -------------------------------------------------------------------------
# pedestrian and bicyclist deaths by month
ggplot(monthly_deaths, aes(x = month)) +
  phase_rects() +
  policy_vlines(daylight_warn_date, daylight_enforc_date) +
  geom_line(aes(y = ped_killed,  color = "Pedestrian"), linewidth = 1.1) +
  geom_point(aes(y = ped_killed, color = "Pedestrian"), size = 2) +
  geom_line(aes(y = bike_killed, color = "Bicycle"),    linewidth = 1.1) +
  geom_point(aes(y = bike_killed, color = "Bicycle"),   size = 2) +
  scale_color_manual(values = c("Pedestrian" = CLR_PED, "Bicycle" = CLR_BIKE)) +
  scale_x_date(limits = as.Date(c("2020-01-01", "2026-01-01")),
               date_breaks = "1 year", date_labels = "%b '%y") +
  labs(title = "Fatalities by Month",
       caption = "Source: UC Berkley Transportation Injury Mapping System (TIMS)",
       x = NULL, y = "Fatalities") +
  theme_safety()

# -------------------------------------------------------------------------
# pedestrian and bicyclist injuries by month

ggplot(monthly_injuries, aes(x = month)) +
  phase_rects() +
  policy_vlines(daylight_warn_date, daylight_enforc_date) +
  geom_line(aes(y = ped_injured,  color = "Pedestrian"), linewidth = 1.1) +
  geom_point(aes(y = ped_injured, color = "Pedestrian"), size = 2) +
  geom_line(aes(y = bike_injured, color = "Bicycle"),    linewidth = 1.1) +
  geom_point(aes(y = bike_injured, color = "Bicycle"),   size = 2) +
  scale_color_manual(values = c("Pedestrian" = CLR_PED, "Bicycle" = CLR_BIKE)) +
  scale_x_date(limits = as.Date(c("2020-01-01", "2026-01-01")), 
               date_breaks = "1 year", date_labels = "%b '%y") +
  labs(title = "Injuries by Month",
       caption = "Source: UC Berkley Transportation Injury Mapping System (TIMS)",
       x = NULL, y = "Injuries") +
  theme_safety()

# -------------------------------------------------------------------------
# pedestrian deaths by month with yearly month avg
ggplot() +
  phase_rects() +
  geom_col(data = annual_data,
           aes(x = year_start + months(6), y = annual_avg_ped),
           fill = CLR_PED, alpha = 0.35, width = 340) +
  geom_line(data = monthly_deaths,
            aes(x = month, y = ped_killed),
            color = CLR_PED, linewidth = 1.2) +
  geom_point(data = monthly_deaths,
             aes(x = month, y = ped_killed),
             color = CLR_PED, size = 2) +
  policy_vlines(daylight_warn_date, daylight_enforc_date) +
  scale_x_date(limits = as.Date(c("2020-01-01", "2026-01-01")),
               date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Pedestrian Fatalities", x = NULL, y = "Monthly Deaths",
       caption = "Source: UC Berkley Transportation Injury Mapping System (TIMS)") +
  theme_safety()

# -------------------------------------------------------------------------
# bicyclist deaths by month with yearly month avg
ggplot() +
  phase_rects() +
  geom_col(data = annual_data,
           aes(x = year_start + months(6), y = annual_avg_bike),
           fill = CLR_BIKE, alpha = 0.35, width = 340) +
  geom_line(data = monthly_deaths,
            aes(x = month, y = bike_killed),
            color = CLR_BIKE, linewidth = 1.2) +
  geom_point(data = monthly_deaths,
             aes(x = month, y = bike_killed),
             color = CLR_BIKE, size = 2) +
  policy_vlines(daylight_warn_date, daylight_enforc_date) +
  scale_x_date(limits = as.Date(c("2020-01-01", "2026-01-01")),
               date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Bicycle Fatalities", x = NULL, y = "Monthly Deaths",
       caption = "Source: UC Berkley Transportation Injury Mapping System (TIMS)") +
  theme_safety()