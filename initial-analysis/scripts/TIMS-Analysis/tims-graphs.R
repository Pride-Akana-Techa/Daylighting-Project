library(tidyverse)
library(lubridate)
library(scales)
library(patchwork)

CLR_PED      <- "#C0392B"   # red
CLR_BIKE     <- "#2980B9"   # blue
CLR_COMBINED <- "#8E44AD"   # purple
CLR_START   <- "#E67E22"   # orange 
CLR_ENFORCE  <- "#27AE60"   # green  


PHASE_WARNING  <- list(xmin = as.Date("2024-01-01"), xmax = as.Date("2025-01-01"),
                       fill = "#FEF9E7", label = "Warning Phase")
PHASE_ENFORCE  <- list(xmin = as.Date("2025-01-01"), xmax = as.Date("2026-01-01"),
                       fill = "#EAFAF1", label = "Enforcement Phase")

phase_rects <- function() {
  list(
    annotate("rect",
             xmin = PHASE_WARNING$xmin, xmax = PHASE_WARNING$xmax,
             ymin = -Inf, ymax = Inf,
             fill = PHASE_WARNING$fill, alpha = 1),
    annotate("rect",
             xmin = PHASE_ENFORCE$xmin, xmax = PHASE_ENFORCE$xmax,
             ymin = -Inf, ymax = Inf,
             fill = PHASE_ENFORCE$fill, alpha = 1)
  )
}

phase_labels <- function(y_pos) {
  list(
    annotate("text", x = PHASE_WARNING$xmin + days(10), y = y_pos,
             label = "Warning Phase", hjust = 0, size = 3,
             color = "#B7770D", fontface = "italic"),
    annotate("text", x = PHASE_ENFORCE$xmin + days(10), y = y_pos,
             label = "Enforcement Phase", hjust = 0, size = 3,
             color = "#1E8449", fontface = "italic")
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

monthly <- function(data, ...) {
  vars <- enquos(...)
  data %>%
    mutate(month = floor_date(day, "month")) %>%
    group_by(month) %>%
    summarise(across(c(!!!vars), sum, .names = "{.col}"), .groups = "drop")
}

daylight_warn_date   <- as.Date("2024-01-01")  
daylight_enforc_date <- as.Date("2025-01-01")   



monthly_deaths   <- monthly(daily_data, ped_killed, bike_killed)
monthly_injuries <- monthly(daily_data, ped_injured, bike_injured)


p1_deaths <- ggplot(monthly_deaths, aes(x = month)) +
  phase_rects() +
  policy_vlines(daylight_warn_date, daylight_enforc_date) +
  geom_line(aes(y = ped_killed,  color = "Pedestrian"), linewidth = 1.1) +
  geom_point(aes(y = ped_killed, color = "Pedestrian"), size = 2) +
  geom_line(aes(y = bike_killed, color = "Bicycle"),    linewidth = 1.1) +
  geom_point(aes(y = bike_killed, color = "Bicycle"),   size = 2) +
  scale_color_manual(values = c("Pedestrian" = CLR_PED, "Bicycle" = CLR_BIKE)) +
  scale_x_date(limits = as.Date(c("2020-01-01", "2026-01-01")),
               date_breaks = "1 year", date_labels = "%b '%y") +
  labs(title = "Traffic Fatalities by Month",
       caption = "Source: UC Berkley Transportation Injury Mapping System (TIMS)",
       x = NULL, y = "Deaths") +
  theme_safety()
p1_deaths

p1_injuries <- ggplot(monthly_injuries, aes(x = month)) +
  phase_rects() +
  policy_vlines(daylight_warn_date, daylight_enforc_date) +
  geom_line(aes(y = ped_injured,  color = "Pedestrian"), linewidth = 1.1) +
  geom_point(aes(y = ped_injured, color = "Pedestrian"), size = 2) +
  geom_line(aes(y = bike_injured, color = "Bicycle"),    linewidth = 1.1) +
  geom_point(aes(y = bike_injured, color = "Bicycle"),   size = 2) +
  scale_color_manual(values = c("Pedestrian" = CLR_PED, "Bicycle" = CLR_BIKE)) +
  scale_x_date(limits = as.Date(c("2020-01-01", "2026-01-01")), 
               date_breaks = "1 year", date_labels = "%b '%y") +
  labs(title = "Traffic Injuries by Month",
       caption = "Source: UC Berkley Transportation Injury Mapping System (TIMS)",
       x = NULL, y = "Injuries") +
  theme_safety()
p1_injuries





make_bar_line_chart <- function(data, col, title, subtitle, y_label,
                                bar_color, line_color) {
  monthly_df <- data %>%
    mutate(month = ceiling_date(day, "month") - days(1)) %>%
    group_by(month) %>%
    summarise(monthly_total = sum(.data[[col]], na.rm = TRUE), .groups = "drop")
  
  annual_df <- data %>%
    mutate(year_start = floor_date(day, "year")) %>%
    group_by(year_start) %>%
    summarise(annual_avg = sum(.data[[col]], na.rm = TRUE) / 12, .groups = "drop")
  
  ggplot() +
    phase_rects() +
    geom_col(data = annual_df,
             aes(x = year_start + months(6), y = annual_avg),
             fill = bar_color, alpha = 0.35, width = 340) +
    geom_line(data = monthly_df,
              aes(x = month, y = monthly_total),
              color = line_color, linewidth = 1.2) +
    geom_point(data = monthly_df,
               aes(x = month, y = monthly_total),
               color = line_color, size = 2) +
    policy_vlines(daylight_warn_date, daylight_enforc_date) +
    scale_x_date(limits = as.Date(c("2020-01-01", "2026-01-01")),
                 date_breaks = "1 year", date_labels = "%Y") +
    labs(title = title, x = NULL, y = "Monthly Deaths",
         caption = "Source: UC Berkley Transportation Injury Mapping System (TIMS)") +
    theme_safety()
}

chart4_ped <- make_bar_line_chart(
  daily_data, "ped_killed",
  title     = "Pedestrian Fatalities",
  y_label   = "Deaths",
  bar_color = CLR_PED,
  line_color = CLR_PED
)

chart4_bike <- make_bar_line_chart(
  daily_data, "bike_killed",
  title     = "Bicycle Fatalities",
  y_label   = "Deaths",
  bar_color = CLR_BIKE,
  line_color = CLR_BIKE
)

chart4_ped
chart4_bike
