# -------------------------------------------------------------------------
## This script categorizes pedestrian crashes into school zones and non school    zone areas, and visualizes the crash trends in these two categories.

# -------------------------------------------------------------------------



# Set Up ------------------------------------------------------------------

# Load packages
library(sf)
library(here)
library(ggplot2)
library(dplyr)
library(leaflet)
library(scales)
library(lubridate)

gdb_path <- "initial-analysis/data-raw/CaliforniaSchools/CSCD_2025.gdb"

public_schools <- st_read(dsn = gdb_path, layer = "Schools_Current_Stacked") |>
  st_transform(crs = 3310)

crashes <- readRDS(here("initial-analysis", "data-clean", "02_TIMS_Geocoded.rds")) |>
#  filter(INTERSECTION == "Y") |>
  st_transform(crs = 3310)

buffer_ft <- 500 
buffer <- buffer_ft * 0.3048 

school_zones <- st_buffer(public_schools, dist = buffer)
school_zones_union <- st_union(school_zones)


# Group crashes into school zones and non-school zones
crashes_analyzed <- crashes |>
  mutate(
    crash_date = as.Date(COLLISION_DATE), 
    in_school_zone = lengths(st_intersects(geometry, school_zones_union)) > 0,
    policy_period = case_when(
      crash_date < as.Date("2024-01-01") ~ "Pre-Policy (Before 2024)",
      crash_date >= as.Date("2024-01-01") & crash_date < as.Date("2025-01-01") ~ "Warnings (1/1/2024)",
      crash_date >= as.Date("2025-01-01") ~ "Citations (1/1/2025)",
    )
  )

# Save the dataset
saveRDS(crashes_analyzed, "initial-analysis/data-clean/school_data.rds")

# aggregate by month
monthly_trends <- crashes_analyzed |>
  st_drop_geometry() |>
  mutate(month_date = floor_date(as.Date(COLLISION_DATE), "month")) |>
  filter(year(month_date) >= 2021 & year(month_date) <= 2025) |> 
  group_by(month_date, in_school_zone) |>
  summarise(crashes = n(), .groups = "drop") |>
  mutate(
    zone_label = if_else(in_school_zone, "School Zone (Within 500ft)", "Control (Outside School Zone)")
  )

# crash value for normalizing
dec_2023_values <- monthly_trends |>
  filter(month_date == as.Date("2023-12-01")) |>
  select(in_school_zone, dec_2023_crashes = crashes)


plot_indexed_data <- monthly_trends |>
  left_join(dec_2023_values, by = "in_school_zone") |>
  mutate(
    # divide every month's raw crash count by the Jan 2024 crash count
    indexed_value = (crashes - dec_2023_crashes) / dec_2023_crashes
  )


ggplot(plot_indexed_data, aes(x = month_date, y = indexed_value, group = zone_label, color = zone_label)) +
  # Baseline represents December 2023 (0% change)
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.7) +
  
  # AB-413 Phase Lines
  geom_vline(xintercept = as.numeric(as.Date("2024-01-01")), linetype = "dotted", linewidth = 0.9) +
  geom_vline(xintercept = as.numeric(as.Date("2025-01-01")), linetype = "dotted", linewidth = 0.9) +
  
  # Raw data tracks (low opacity to prevent visual clutter)
  geom_line(alpha = 0.25, linewidth = 0.5) +
  geom_smooth(method = "loess", span = 0.3, se = FALSE, linewidth = 1.3) + 
  
  # Policy Phase Annotations (Adjusted Y values to align with percent scales)
  annotate("text", x = as.Date("2024-01-01"), y = 0.2, label = "Warnings\n(Jan 1, 2024)", 
           hjust = 1.1, size = 3.5) +
  annotate("text", x = as.Date("2025-01-01"), y = 0.2, label = "Citations\n(Jan 1, 2025)", 
           hjust = 1.1, size = 3.5) +
  
  # Formatting
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) + 
  scale_color_manual(values = c("School Zone (Within 500ft)" = "orange", 
                                "Control (Outside School Zone)" = "darkblue")) +
  labs(
    title = "Relative Crash Trends Indexed to AB-413 Warning Start",
#    subtitle = "Intersection Accident",
    x = NULL,
    y = "% Change Relative to December 2023 Baseline",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )



plot_rolling_data <- monthly_trends |>
  arrange(in_school_zone, month_date) |>
  
  group_by(in_school_zone) |>
  
  mutate(
    prior_month_crashes = lag(crashes),
    mom_rate_of_change = ((crashes - prior_month_crashes) / prior_month_crashes) * 100
  ) |>
  
  ungroup()

ggplot(plot_rolling_data, aes(x = month_date, y = mom_rate_of_change, group = zone_label, color = zone_label)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.7) +
  # AB-413 Phase Lines
  geom_vline(xintercept = as.numeric(as.Date("2024-01-01")), linetype = "dotted",  size = 0.9) +
  geom_vline(xintercept = as.numeric(as.Date("2025-01-01")), linetype = "dotted",  size = 0.9) +
  geom_line(alpha = 0.3, size = 0.6) +
  geom_smooth(method = "loess", span = 0.3, se = FALSE, size = 1.3) + 
  
  # policy
  # annotate("text", x = as.Date("2024-01-01"), y = 1.25, label = "Warnings\n(Jan 1, 2024)", 
  #          hjust = 1.1, size = 3.5) +
  # annotate("text", x = as.Date("2025-01-01"), y = 1.25, label = "Citations\n(Jan 1, 2025)", 
  #          hjust = 1.1, size = 3.5) +
  # 
  # Formatting
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) + # Formats 1.0 as 100%
  scale_color_manual(values = c("School Zone (Within 500ft)" = "orange", 
                                "Control (Outside School Zone)" = "darkblue")) +
  labs(
    title = "Relative Crash Change to AB-413 Warning Start",
    subtitle = "Pedestrian Crashes",
    x = NULL,
    y = "% Change Relative to Dec 2023",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )