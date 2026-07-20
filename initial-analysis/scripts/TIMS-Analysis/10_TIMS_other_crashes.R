
# -------------------------------------------------------------------------

## This script evaluates other crash trends, to determine how they change after the policy.
## Inputs: initial-analysis/data-clean/updated_tims.rds
##         initial-analysis/data/Crashes_California.csv

# -------------------------------------------------------------------------



# Set Up ------------------------------------------------------------------

# Load packages and dataset
library(tidyverse)
library(rdrobust)
library(patchwork)

tims_crashes <- readRDS("initial-analysis/data-clean/updated_tims.rds")
crashes <- read_csv("initial-analysis/data/Crashes_California.csv")

# Filter required crashes
crashes <- crashes |> 
  select(-OFFICER_ID, -REPORTING_DISTRICT, -CITY_DIVISION_LAPD, 
         -BEAT_NUMBER, -DIRECTION, -CALTRANS_COUNTY, -CALTRANS_DISTRICT,
         -STATE_ROUTE, -ROUTE_SUFFIX, -POSTMILE_PREFIX, -POSTMILE, 
         -LOCATION_TYPE, -RAMP_INTERSECTION, -SIDE_OF_HWY, -TOW_AWAY,
         -PRIMARY_COLL_FACTOR, -PCF_CODE_OF_VIOL, -PCF_VIOLATION,
         -PCF_VIOL_SUBSECTION, -HIT_AND_RUN, -TYPE_OF_COLLISION, 
         -CHP_SHIFT, -MVIW, -LATITUDE, -LONGITUDE, -ROAD_COND_1, 
         -ROAD_COND_2, -CONTROL_DEVICE,-CHP_ROAD_TYPE,
         -NOT_PRIVATE_PROPERTY, -STWD_VEHTYPE_AT_FAULT, 
         -CHP_VEHTYPE_AT_FAULT, -COUNT_MC_KILLED, -COUNT_MC_INJURED, 
         -PRIMARY_RAMP, -SECONDARY_RAMP, -JURIS, -PROC_DATE, 
         -CNTY_CITY_LOC, -BEAT_TYPE, -CHP_BEAT_TYPE, -SPECIAL_COND,
         -CHP_BEAT_CLASS, -WEATHER_2) |>
  distinct(CASE_ID, .keep_all = TRUE) 

# Save the data
saveRDS(crashes, "initial-analysis/data-clean/tims_crashes.rds")

tims_crashes2 <- readRDS("initial-analysis/data-clean/tims_crashes.rds")

# Bicycle Intersection Crashes ----------------------------------------------

# Prepare data
bike_data <- tims_crashes2 |>
  filter(ACCIDENT_YEAR %in% c("2023", "2024", "2025")) |> 
  filter(INTERSECTION == "Y" & BICYCLE_ACCIDENT == "Y") 


# Aggregate to monthly counts
bike_monthly <- bike_data |>
  mutate(crash_date = as.Date(COLLISION_DATE),
         year_month = floor_date(crash_date, "month")) |>
  count(year_month, name = "crash_count")

# Plot
ggplot(bike_monthly, aes(x = year_month, y = crash_count)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue") +
  labs(title = "Monthly Bicycle Crashes at Intersections",
       x = "Month",
       y = "Crash Count") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  scale_y_continuous(breaks = seq(100, 600, by = 100)) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"))



## Jan 2024 Cutoff ##

# Prepare model data
bike_rdit <- bike_data |> 
  filter(ACCIDENT_YEAR %in% c("2023", "2024")) |>
  mutate(MONTH = floor_date(ymd(COLLISION_DATE), "month"),
         Time = interval(as.Date("2024-01-01"), MONTH) %/% months(1),
         Post = ifelse(Time >= 0, 1, 0),
         Season_factor = factor(
           case_when(month(MONTH) %in% c(12, 1, 2) ~ "Winter",
                     month(MONTH) %in% c(3, 4, 5) ~ "Spring",
                     month(MONTH) %in% c(6, 7, 8) ~ "Summer",
                     month(MONTH) %in% c(9, 10, 11) ~ "Fall"),
           levels = c("Winter", "Spring", "Summer", "Fall"))) |> 
  
  group_by(Time, Post, Season_factor) |> 
  summarise(Total_crashes = n(),
            .groups = "drop")


# Run the model
bike_model <- rdrobust(y = bike_rdit$Total_crashes,
                         x = bike_rdit$Time,
                         covs = model.matrix(~ Season_factor, bike_rdit)[, -1],
                         c = 0,
                         p = 1,
                         h = 12,
                         kernel = "triangular")

summary(bike_model)

  
# Adjust for Seasonality
bike_seasonal_model <- lm(Total_crashes ~ Season_factor,
                     data = bike_rdit)

bike_rdit$Crash_adj <- resid(bike_seasonal_model) + mean(bike_rdit$Total_crashes)

# plot
bike_rd_out <- rdplot(y = bike_rdit$Crash_adj,
                        x = bike_rdit$Time,
                        c = 0,
                        p = 1,
                        h = 12,
                        kernel = "triangular",
                        nbins = c(12, 12))

bike_rd_out$rdplot +
  labs(title = "RDiT Model (Bicycle Intersection Crashes)",
       y = "Number of Crashes",
       x = "Months Relative to Jan 2024") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )


## Jan 2025 Cutoff
# Prepare data for the model

bike_rdit2 <- bike_data |> 
  filter(ACCIDENT_YEAR %in% c("2024", "2025")) |>
  mutate(MONTH = floor_date(ymd(COLLISION_DATE), "month"),
         Time = interval(as.Date("2025-01-01"), MONTH) %/% months(1),
         Post = ifelse(Time >= 0, 1, 0),
         Season_factor = factor(
           case_when(month(MONTH) %in% c(12, 1, 2) ~ "Winter",
                     month(MONTH) %in% c(3, 4, 5) ~ "Spring",
                     month(MONTH) %in% c(6, 7, 8) ~ "Summer",
                     month(MONTH) %in% c(9, 10, 11) ~ "Fall"),
           levels = c("Winter", "Spring", "Summer", "Fall"))) |> 
  
  group_by(Time, Post, Season_factor) |> 
  summarise(Total_crashes = n(),
            .groups = "drop")


# Run the model
bike_model2 <- rdrobust(y = bike_rdit2$Total_crashes,
                       x = bike_rdit2$Time,
                       covs = model.matrix(~ Season_factor, bike_rdit2)[, -1],
                       c = 0,
                       p = 1,
                       h = 12,
                       kernel = "triangular")

summary(bike_model2)


# Adjust for Seasonality
bike_seasonal_model2 <- lm(Total_crashes ~ Season_factor,
                          data = bike_rdit2)

bike_rdit2$Crash_adj <- resid(bike_seasonal_model2) + mean(bike_rdit2$Total_crashes)

# plot
bike_rd_out2 <- rdplot(y = bike_rdit2$Crash_adj,
                      x = bike_rdit2$Time,
                      c = 0,
                      p = 1,
                      h = 12,
                      kernel = "triangular",
                      nbins = c(12, 12))

bike_rd_out2$rdplot +
  labs(title = "RDiT Model (Bicycle Intersection Crashes)",
       y = "Number of Crashes",
       x = "Months Relative to Jan 2025") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )


## Combined Model ##


h <- 12
nudge_days <- 5

make_ci_band <- function(data, y_var, side_filter, xseq) {
  d <- data |> filter(side_filter(Time))
  d$w <- (1 - abs(d$Time / h)) * (abs(d$Time / h) <= 1)
  fit <- lm(reformulate("Time", response = y_var), data = d, weights = w)
  pred <- predict(fit, newdata = data.frame(Time = xseq), se.fit = TRUE)
  data.frame(
    Time = xseq,
    fit = pred$fit,
    lwr = pred$fit - qt(0.975, fit$df.residual) * pred$se.fit,
    upr = pred$fit + qt(0.975, fit$df.residual) * pred$se.fit
  )
}

xseq_left  <- seq(-12, 0, length.out = 100)
xseq_right <- seq(0, 12, length.out = 100)

ci_2024 <- bind_rows(
  make_ci_band(bike_rdit, "Crash_adj", function(t) t < 0, xseq_left),
  make_ci_band(bike_rdit, "Crash_adj", function(t) t >= 0, xseq_right)
) |> mutate(Date = as.Date("2024-01-01") + Time * 30.4368 - nudge_days,
            Model = "Jan 2024 Cutoff")

ci_2025 <- bind_rows(
  make_ci_band(bike_rdit2, "Crash_adj", function(t) t < 0, xseq_left),
  make_ci_band(bike_rdit2, "Crash_adj", function(t) t >= 0, xseq_right)
) |> mutate(Date = as.Date("2025-01-01") + Time * 30.4368 + nudge_days,
            Model = "Jan 2025 Cutoff")


# combined plot, but scopes each panel to its own cutoff and window.
make_cutoff_plot <- function(ci_data, cutoff_date, model_label, line_color,
                             event_label, x_limits, x_breaks,
                             y_breaks = seq(100, 500, by = 50)) {
  ggplot(ci_data) +
    geom_ribbon(aes(x = Date, ymin = lwr, ymax = upr), fill = line_color, alpha = 0.2) +
    geom_line(data = filter(ci_data, Time < 0), aes(Date, fit), color = line_color, linewidth = 1) +
    geom_line(data = filter(ci_data, Time > 0), aes(Date, fit), color = line_color, linewidth = 1) +
    geom_vline(xintercept = cutoff_date, linetype = "dashed", color = "black") +
    annotate("text", x = cutoff_date, y = Inf, label = event_label,
             vjust = 1.5, fontface = "bold", size = 4) +
    scale_y_continuous(breaks = y_breaks) +
    scale_x_date(date_labels = "%b %Y",
                 limits = x_limits,
                 breaks = x_breaks,
                 expand = c(0.02, 0)) +
    theme_minimal(base_size = 13) +
    labs(title = model_label, x = "Month", y = "Crash Count") +
    theme(plot.title = element_text(size = 16, face = "bold"),
          axis.title.x = element_text(size = 13, face = "bold"),
          axis.title.y = element_text(size = 13, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1))
}

p_2024 <- make_cutoff_plot(
  ci_2024,
  cutoff_date = as.Date("2024-01-01"),
  model_label = "Bicycle Intersection RDiT, Jan 2024 Cutoff",
  line_color  = "#0072B2",
  event_label = "Warning Begins",
  x_limits = c(as.Date("2023-01-01"), as.Date("2024-12-01")),
  x_breaks = seq(as.Date("2023-01-01"), as.Date("2024-12-01"), by = "2 months")
)

p_2025 <- make_cutoff_plot(
  ci_2025,
  cutoff_date = as.Date("2025-01-01"),
  model_label = "Bicycle Intersection RDiT, Jan 2025 Cutoff",
  line_color  = "#D55E00",
  event_label = "Enforcement Begins",
  x_limits = c(as.Date("2024-01-01"), as.Date("2025-12-01")),
  x_breaks = seq(as.Date("2024-01-01"), as.Date("2025-12-01"), by = "2 months")
)

# Side by side 
p_2024 + p_2025


ggsave(filename = "initial-analysis/figs/bike_int_rdit.png",
       height = 10,
       width = 20)


# Bicycle Non-Intersection  -----------------------------------------------

bike_data2 <- tims_crashes2 |>
  filter(ACCIDENT_YEAR %in% c("2023", "2024", "2025")) |> 
  filter(INTERSECTION == "N" & BICYCLE_ACCIDENT == "Y") 


## Jan 2024 ##
bike_rdit3 <- bike_data2 |> 
  filter(ACCIDENT_YEAR %in% c("2023", "2024")) |>
  mutate(MONTH = floor_date(ymd(COLLISION_DATE), "month"),
         Time = interval(as.Date("2024-01-01"), MONTH) %/% months(1),
         Post = ifelse(Time >= 0, 1, 0),
         Season_factor = factor(
           case_when(month(MONTH) %in% c(12, 1, 2) ~ "Winter",
                     month(MONTH) %in% c(3, 4, 5) ~ "Spring",
                     month(MONTH) %in% c(6, 7, 8) ~ "Summer",
                     month(MONTH) %in% c(9, 10, 11) ~ "Fall"),
           levels = c("Winter", "Spring", "Summer", "Fall"))) |> 
  
  group_by(Time, Post, Season_factor) |> 
  summarise(Total_crashes = n(),
            .groups = "drop")


# Run the model
bike_model3 <- rdrobust(y = bike_rdit3$Total_crashes,
                       x = bike_rdit3$Time,
                       covs = model.matrix(~ Season_factor, bike_rdit3)[, -1],
                       c = 0,
                       p = 1,
                       h = 12,
                       kernel = "triangular")

summary(bike_model3)


# Adjust for Seasonality
bike_seasonal_model3 <- lm(Total_crashes ~ Season_factor,
                          data = bike_rdit3)

bike_rdit3$Crash_adj <- resid(bike_seasonal_model3) + mean(bike_rdit3$Total_crashes)

# plot
bike_rd_out3 <- rdplot(y = bike_rdit3$Crash_adj,
                      x = bike_rdit3$Time,
                      c = 0,
                      p = 1,
                      h = 12,
                      kernel = "triangular",
                      nbins = c(12, 12))

bike_rd_out3$rdplot +
  labs(title = "RDiT Model (Bicycle Non-intersection Crashes)",
       y = "Number of Crashes",
       x = "Months Relative to Jan 2024") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )

## Jan 2025 Cutoff ##
bike_rdit4 <- bike_data2 |> 
  filter(ACCIDENT_YEAR %in% c("2024", "2025")) |>
  mutate(MONTH = floor_date(ymd(COLLISION_DATE), "month"),
         Time = interval(as.Date("2025-01-01"), MONTH) %/% months(1),
         Post = ifelse(Time >= 0, 1, 0),
         Season_factor = factor(
           case_when(month(MONTH) %in% c(12, 1, 2) ~ "Winter",
                     month(MONTH) %in% c(3, 4, 5) ~ "Spring",
                     month(MONTH) %in% c(6, 7, 8) ~ "Summer",
                     month(MONTH) %in% c(9, 10, 11) ~ "Fall"),
           levels = c("Winter", "Spring", "Summer", "Fall"))) |> 
  
  group_by(Time, Post, Season_factor) |> 
  summarise(Total_crashes = n(),
            .groups = "drop")


# Run the model
bike_model4 <- rdrobust(y = bike_rdit4$Total_crashes,
                       x = bike_rdit4$Time,
                       covs = model.matrix(~ Season_factor, bike_rdit4)[, -1],
                       c = 0,
                       p = 1,
                       h = 12,
                       kernel = "triangular")

summary(bike_model4)


# Adjust for Seasonality
bike_seasonal_model4 <- lm(Total_crashes ~ Season_factor,
                          data = bike_rdit4)

bike_rdit4$Crash_adj <- resid(bike_seasonal_model4) + mean(bike_rdit4$Total_crashes)

# plot
bike_rd_out4 <- rdplot(y = bike_rdit4$Crash_adj,
                      x = bike_rdit4$Time,
                      c = 0,
                      p = 1,
                      h = 12,
                      kernel = "triangular",
                      nbins = c(12, 12))

bike_rd_out4$rdplot +
  labs(title = "RDiT Model (Bicycle Non-intersection Crashes)",
       y = "Number of Crashes",
       x = "Months Relative to Jan 2025") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )



## Combine Plots ##

h <- 12
nudge_days <- 5

make_ci_band <- function(data, y_var, side_filter, xseq) {
  d <- data |> filter(side_filter(Time))
  d$w <- (1 - abs(d$Time / h)) * (abs(d$Time / h) <= 1)
  fit <- lm(reformulate("Time", response = y_var), data = d, weights = w)
  pred <- predict(fit, newdata = data.frame(Time = xseq), se.fit = TRUE)
  data.frame(
    Time = xseq,
    fit = pred$fit,
    lwr = pred$fit - qt(0.975, fit$df.residual) * pred$se.fit,
    upr = pred$fit + qt(0.975, fit$df.residual) * pred$se.fit
  )
}

xseq_left  <- seq(-12, 0, length.out = 100)
xseq_right <- seq(0, 12, length.out = 100)

ci_2024 <- bind_rows(
  make_ci_band(bike_rdit3, "Crash_adj", function(t) t < 0, xseq_left),
  make_ci_band(bike_rdit3, "Crash_adj", function(t) t >= 0, xseq_right)
) |> mutate(Date = as.Date("2024-01-01") + Time * 30.4368 - nudge_days,
            Model = "Jan 2024 Cutoff")

ci_2025 <- bind_rows(
  make_ci_band(bike_rdit4, "Crash_adj", function(t) t < 0, xseq_left),
  make_ci_band(bike_rdit4, "Crash_adj", function(t) t >= 0, xseq_right)
) |> mutate(Date = as.Date("2025-01-01") + Time * 30.4368 + nudge_days,
            Model = "Jan 2025 Cutoff")


# combined plot, but scopes each panel to its own cutoff and window.
make_cutoff_plot <- function(ci_data, cutoff_date, model_label, line_color,
                             event_label, x_limits, x_breaks,
                             y_breaks = seq(200, 600, by = 50)) {
  ggplot(ci_data) +
    geom_ribbon(aes(x = Date, ymin = lwr, ymax = upr), fill = line_color, alpha = 0.2) +
    geom_line(data = filter(ci_data, Time < 0), aes(Date, fit), color = line_color, linewidth = 1) +
    geom_line(data = filter(ci_data, Time > 0), aes(Date, fit), color = line_color, linewidth = 1) +
    geom_vline(xintercept = cutoff_date, linetype = "dashed", color = "black") +
    annotate("text", x = cutoff_date, y = Inf, label = event_label,
             vjust = 1.5, fontface = "bold", size = 4) +
    scale_y_continuous(breaks = y_breaks) +
    scale_x_date(date_labels = "%b %Y",
                 limits = x_limits,
                 breaks = x_breaks,
                 expand = c(0.02, 0)) +
    theme_minimal(base_size = 13) +
    labs(title = model_label, x = "Month", y = "Crash Count") +
    theme(plot.title = element_text(size = 16, face = "bold"),
          axis.title.x = element_text(size = 13, face = "bold"),
          axis.title.y = element_text(size = 13, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1))
}

p_2024 <- make_cutoff_plot(
  ci_2024,
  cutoff_date = as.Date("2024-01-01"),
  model_label = "Bicycle Non-intersection RDiT, Jan 2024 Cutoff",
  line_color  = "#0072B2",
  event_label = "Warning Begins",
  x_limits = c(as.Date("2023-01-01"), as.Date("2024-12-01")),
  x_breaks = seq(as.Date("2023-01-01"), as.Date("2024-12-01"), by = "2 months")
)

p_2025 <- make_cutoff_plot(
  ci_2025,
  cutoff_date = as.Date("2025-01-01"),
  model_label = "Bicycle Non-intersection RDiT, Jan 2025 Cutoff",
  line_color  = "#D55E00",
  event_label = "Enforcement Begins",
  x_limits = c(as.Date("2024-01-01"), as.Date("2025-12-01")),
  x_breaks = seq(as.Date("2024-01-01"), as.Date("2025-12-01"), by = "2 months")
)

# Side by side 
p_2024 + p_2025

ggsave(filename = "initial-analysis/figs/bike_int.png",
       height = 10,
       width = 20)


# Non Pedestrians Intersections ---------------------------------------------

## Jan 2024 Cutoff
# Prepare model data
no_ped_data <- tims_crashes |> 
  filter(ACCIDENT_YEAR %in% c("2023", "2024")) |> 
  filter(PED_ACTION == "A" &
         INTERSECTION == "Y") |>
  mutate(MONTH = floor_date(ymd(COLLISION_DATE), "month"),
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
  group_by(Time, Post, Season_factor) |> 
  summarise(Total_crashes = n(),
            .groups = "drop")

## Using rdrobust ##
no_ped_model <- rdrobust(y = no_ped_data$Total_crashes,
                      x = no_ped_data$Time,
                      covs = model.matrix(~ Season_factor, no_ped_data)[, -1],
                      c = 0,
                      p = 1,
                      h = 12,
                      kernel = "triangular")

summary(no_ped_model)

# Adjusting for seasonality before plotting
seasonal_model <- lm(Total_crashes ~ Season_factor,
                    data = no_ped_data)

no_ped_data$Crash_adj <- resid(seasonal_model) + mean(no_ped_data$Total_crashes)

# plot
no_ped_rd_out <- rdplot(y = no_ped_data$Crash_adj,
                 x = no_ped_data$Time,
                 c = 0,
                 p = 1,
                 h = 12,
                 kernel = "triangular",
                 nbins = c(12, 12))

no_ped_rd_out$rdplot +
  labs(title = "RDiT Model(Crashes with No Pedestrians)",
       y = "Number of Crashes",
       x = "Months Relative to Jan 2024") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )


## 2. Jan 2025 Cutoff
# Prepare data for the model
no_ped_data2 <- tims_crashes |> 
  filter(ACCIDENT_YEAR %in% c("2024", "2025")) |> 
  filter(PED_ACTION == "A" &
           INTERSECTION == "Y") |>
  mutate(MONTH = floor_date(ymd(COLLISION_DATE), "month"),
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
  group_by(Time, Post, Season_factor) |> 
  summarise(Total_crashes = n(),
            .groups = "drop")

# Run the model
no_ped_model2 <- rdrobust(y = no_ped_data2$Total_crashes,
                         x = no_ped_data2$Time,
                         covs = model.matrix(~ Season_factor, no_ped_data2)[, -1],
                         c = 0,
                         p = 1,
                         h = 12,
                         kernel = "triangular")

summary(no_ped_model2)

# Adjusting for seasonality before plotting
seasonal_model2 <- lm(Total_crashes ~ Season_factor,
                     data = no_ped_data2)

no_ped_data2$Crash_adj <- resid(seasonal_model2) + mean(no_ped_data2$Total_crashes)

# plot
no_ped_rd_out2 <- rdplot(y = no_ped_data2$Crash_adj,
                        x = no_ped_data2$Time,
                        c = 0,
                        p = 1,
                        h = 12,
                        kernel = "triangular",
                        nbins = c(12, 12))

no_ped_rd_out2$rdplot +
  labs(title = "RDiT Model (Crashes with No Pedestrians)",
       y = "Number of Crashes",
       x = "Months Relative to Jan 2025") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )


## Combined Model

h <- 12
nudge_days <- 5

make_ci_band <- function(data, y_var, side_filter, xseq) {
  d <- data |> filter(side_filter(Time))
  d$w <- (1 - abs(d$Time / h)) * (abs(d$Time / h) <= 1)
  fit <- lm(reformulate("Time", response = y_var), data = d, weights = w)
  pred <- predict(fit, newdata = data.frame(Time = xseq), se.fit = TRUE)
  data.frame(
    Time = xseq,
    fit = pred$fit,
    lwr = pred$fit - qt(0.975, fit$df.residual) * pred$se.fit,
    upr = pred$fit + qt(0.975, fit$df.residual) * pred$se.fit
  )
}

xseq_left  <- seq(-12, 0, length.out = 100)
xseq_right <- seq(0, 12, length.out = 100)

ci_2024 <- bind_rows(
  make_ci_band(no_ped_data, "Crash_adj", function(t) t < 0, xseq_left),
  make_ci_band(no_ped_data, "Crash_adj", function(t) t >= 0, xseq_right)
) |> mutate(Date = as.Date("2024-01-01") + Time * 30.4368 - nudge_days,
            Model = "Jan 2024 Cutoff")

ci_2025 <- bind_rows(
  make_ci_band(no_ped_data2, "Crash_adj", function(t) t < 0, xseq_left),
  make_ci_band(no_ped_data2, "Crash_adj", function(t) t >= 0, xseq_right)
) |> mutate(Date = as.Date("2025-01-01") + Time * 30.4368 + nudge_days,
            Model = "Jan 2025 Cutoff")


# combined plot, but scopes each panel to its own cutoff and window.
make_cutoff_plot <- function(ci_data, cutoff_date, model_label, line_color,
                             event_label, x_limits, x_breaks,
                             y_breaks = seq(3000, 5000, by = 250)) {
  ggplot(ci_data) +
    geom_ribbon(aes(x = Date, ymin = lwr, ymax = upr), fill = line_color, alpha = 0.2) +
    geom_line(data = filter(ci_data, Time < 0), aes(Date, fit), color = line_color, linewidth = 1) +
    geom_line(data = filter(ci_data, Time > 0), aes(Date, fit), color = line_color, linewidth = 1) +
    geom_vline(xintercept = cutoff_date, linetype = "dashed", color = "black") +
    annotate("text", x = cutoff_date, y = Inf, label = event_label,
             vjust = 1.5, fontface = "bold", size = 4) +
    scale_y_continuous(breaks = y_breaks) +
    scale_x_date(date_labels = "%b %Y",
                 limits = x_limits,
                 breaks = x_breaks,
                 expand = c(0.02, 0)) +
    theme_minimal(base_size = 13) +
    labs(title = model_label, x = "Month", y = "Crash Count") +
    theme(plot.title = element_text(size = 16, face = "bold"),
          axis.title.x = element_text(size = 13, face = "bold"),
          axis.title.y = element_text(size = 13, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1))
}

p_2024 <- make_cutoff_plot(
  ci_2024,
  cutoff_date = as.Date("2024-01-01"),
  model_label = "Non-pedestrian Intersection RDiT, Jan 2024 Cutoff",
  line_color  = "#0072B2",
  event_label = "Warning Begins",
  x_limits = c(as.Date("2023-01-01"), as.Date("2024-12-01")),
  x_breaks = seq(as.Date("2023-01-01"), as.Date("2024-12-01"), by = "2 months")
)

p_2025 <- make_cutoff_plot(
  ci_2025,
  cutoff_date = as.Date("2025-01-01"),
  model_label = "Non-pedestrian Intersection RDiT, Jan 2025 Cutoff",
  line_color  = "#D55E00",
  event_label = "Enforcement Begins",
  x_limits = c(as.Date("2024-01-01"), as.Date("2025-12-01")),
  x_breaks = seq(as.Date("2024-01-01"), as.Date("2025-12-01"), by = "2 months")
)

# Side by side 
p_2024 + p_2025

ggsave(filename = "initial-analysis/figs/no_ped.png",
       height = 10,
       width = 20)

#  Other Pedestrian Related Action ----------------------------------------
## This includes:
## - Crossing not in crosswalk
## - In Road and Shoulder Area
## - Pedestrian not in Road

## 1. Jan 2024 Cutoff
# Prepare model data
other_ped_data <- tims_crashes |> 
  filter(ACCIDENT_YEAR %in% c("2023", "2024")) |> 
  filter(PED_ACTION %in% c("D", "E", "F")) |>
  mutate(MONTH = floor_date(ymd(COLLISION_DATE), "month"),
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
  group_by(Time, Post, Season_factor) |> 
  summarise(Total_crashes = n(),
            .groups = "drop")

# Run the model
other_ped_model <- rdrobust(y = other_ped_data$Total_crashes,
                         x = other_ped_data$Time,
                         covs = model.matrix(~ Season_factor, other_ped_data)[, -1],
                         c = 0,
                         p = 1,
                         h = 12,
                         kernel = "triangular")

summary(other_ped_model)

# Adjusting for seasonality before plotting
seasonal_model2 <- lm(Total_crashes ~ Season_factor,
                     data = other_ped_data)

other_ped_data$Crash_adj <- resid(seasonal_model2) + mean(other_ped_data$Total_crashes)

# plot
other_ped_rd_out <- rdplot(y = other_ped_data$Crash_adj,
                        x = other_ped_data$Time,
                        c = 0,
                        p = 1,
                        h = 12,
                        kernel = "triangular",
                        nbins = c(12, 12))

other_ped_rd_out$rdplot +
  labs(title = "RDiT Model(Pedestrian Non-intersection Crashes)",
       y = "Number of Crashes",
       x = "Months Relative to Jan 2024") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )


## 2. Jan 2025 Cutoff
# Prepare data for the model
other_ped_data2 <- tims_crashes |> 
  filter(ACCIDENT_YEAR %in% c("2024", "2025")) |> 
  filter(PED_ACTION %in% c("D", "E", "F")) |>
  mutate(MONTH = floor_date(ymd(COLLISION_DATE), "month"),
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
  group_by(Time, Post, Season_factor) |> 
  summarise(Total_crashes = n(),
            .groups = "drop")

# Run the model
other_ped_model2 <- rdrobust(y = other_ped_data2$Total_crashes,
                          x = other_ped_data2$Time,
                          covs = model.matrix(~ Season_factor, other_ped_data2)[, -1],
                          c = 0,
                          p = 1,
                          h = 12,
                          kernel = "triangular")

summary(other_ped_model2)

# Adjusting for seasonality before plotting
seasonal_model2 <- lm(Total_crashes ~ Season_factor,
                      data = other_ped_data2)

other_ped_data2$Crash_adj <- resid(seasonal_model2) + mean(other_ped_data2$Total_crashes)

# plot
other_ped_rd_out2 <- rdplot(y = other_ped_data2$Crash_adj,
                         x = other_ped_data2$Time,
                         c = 0,
                         p = 1,
                         h = 12,
                         kernel = "triangular",
                         nbins = c(12, 12))

other_ped_rd_out2$rdplot +
  labs(title = "RDiT Model (Pedestrian Non-intersection Crashes)",
       y = "Number of Crashes",
       x = "Months Relative to Jan 2025") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )


# Merged models
h <- 12
nudge_days <- 5

make_ci_band <- function(data, y_var, side_filter, xseq) {
  d <- data |> filter(side_filter(Time))
  d$w <- (1 - abs(d$Time / h)) * (abs(d$Time / h) <= 1)
  fit <- lm(reformulate("Time", response = y_var), data = d, weights = w)
  pred <- predict(fit, newdata = data.frame(Time = xseq), se.fit = TRUE)
  data.frame(
    Time = xseq,
    fit = pred$fit,
    lwr = pred$fit - qt(0.975, fit$df.residual) * pred$se.fit,
    upr = pred$fit + qt(0.975, fit$df.residual) * pred$se.fit
  )
}

xseq_left  <- seq(-12, 0, length.out = 100)
xseq_right <- seq(0, 12, length.out = 100)

ci_2024 <- bind_rows(
  make_ci_band(other_ped_data, "Crash_adj", function(t) t < 0, xseq_left),
  make_ci_band(other_ped_data, "Crash_adj", function(t) t >= 0, xseq_right)
) |> mutate(Date = as.Date("2024-01-01") + Time * 30.4368 - nudge_days,
            Model = "Jan 2024 Cutoff")

ci_2025 <- bind_rows(
  make_ci_band(other_ped_data2, "Crash_adj", function(t) t < 0, xseq_left),
  make_ci_band(other_ped_data2, "Crash_adj", function(t) t >= 0, xseq_right)
) |> mutate(Date = as.Date("2025-01-01") + Time * 30.4368 + nudge_days,
            Model = "Jan 2025 Cutoff")


# combined plot, but scopes each panel to its own cutoff and window.
make_cutoff_plot <- function(ci_data, cutoff_date, model_label, line_color,
                             event_label, x_limits, x_breaks,
                             y_breaks = seq(300, 600, by = 50)) {
  ggplot(ci_data) +
    geom_ribbon(aes(x = Date, ymin = lwr, ymax = upr), fill = line_color, alpha = 0.2) +
    geom_line(data = filter(ci_data, Time < 0), aes(Date, fit), color = line_color, linewidth = 1) +
    geom_line(data = filter(ci_data, Time > 0), aes(Date, fit), color = line_color, linewidth = 1) +
    geom_vline(xintercept = cutoff_date, linetype = "dashed", color = "black") +
    annotate("text", x = cutoff_date, y = Inf, label = event_label,
             vjust = 1.5, fontface = "bold", size = 4) +
    scale_y_continuous(breaks = y_breaks) +
    scale_x_date(date_labels = "%b %Y",
                 limits = x_limits,
                 breaks = x_breaks,
                 expand = c(0.02, 0)) +
    theme_minimal(base_size = 13) +
    labs(title = model_label, x = "Month", y = "Crash Count") +
    theme(plot.title = element_text(size = 16, face = "bold"),
          axis.title.x = element_text(size = 13, face = "bold"),
          axis.title.y = element_text(size = 13, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1))
}

p_2024 <- make_cutoff_plot(
  ci_2024,
  cutoff_date = as.Date("2024-01-01"),
  model_label = "Other Pedestrian-related RDiT, Jan 2024 Cutoff",
  line_color  = "#0072B2",
  event_label = "Warning Begins",
  x_limits = c(as.Date("2023-01-01"), as.Date("2024-12-01")),
  x_breaks = seq(as.Date("2023-01-01"), as.Date("2024-12-01"), by = "2 months")
)

p_2025 <- make_cutoff_plot(
  ci_2025,
  cutoff_date = as.Date("2025-01-01"),
  model_label = "Other Pedestrian-related RDiT, Jan 2025 Cutoff",
  line_color  = "#D55E00",
  event_label = "Enforcement Begins",
  x_limits = c(as.Date("2024-01-01"), as.Date("2025-12-01")),
  x_breaks = seq(as.Date("2024-01-01"), as.Date("2025-12-01"), by = "2 months")
)

# Side by side 
p_2024 + p_2025




