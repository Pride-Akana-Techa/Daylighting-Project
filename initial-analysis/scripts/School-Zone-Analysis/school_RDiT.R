
# -------------------------------------------------------------------------

## This scripts uses an RDiT model to observe any change in crashes in school     zones around the cutoff dates

# -------------------------------------------------------------------------


# Set Up ------------------------------------------------------------------

# Load packages

library(tidyverse)
library(rdrobust)
library(patchwork)
library(sf)

# Load dataset and select required variables

school_data <- readRDS("initial-analysis/data-clean/school_data.rds")


# Pedestrian Crashes Within School Zones -----------------------------------

school_ped <- school_data |> 
  filter(ACCIDENT_YEAR %in% c(2023, 2024, 2025)) |> 
  filter(in_school_zone == "TRUE")


## Jan 2024 Cutoff ##

# Prepare data for model
school_rdit <- school_ped |> 
  filter(ACCIDENT_YEAR %in% c(2023, 2024)) |> 
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


# Standardizing crash outcomes
pre_mean <- school_rdit |>
  filter(Post == 0) |>
  summarise(mean = mean(Total_crashes)) |>
  pull(mean)

pre_sd <- school_rdit |>
  filter(Post == 0) |>
  summarise(sd = sd(Total_crashes)) |>
  pull(sd)

school_rdit <- school_rdit |>
  mutate(Crash_std = (Total_crashes - pre_mean) / pre_sd)


# Run model
school_zone_model <- rdrobust(y = school_rdit$Crash_std,
                      x = school_rdit$Time,
                      covs = model.matrix(~ Season_factor, school_rdit)[, -1],
                      c = 0,
                      p = 1,
                      h = 12,
                      kernel = "triangular")

summary(school_zone_model)



# Adjusting for seasonality before plotting
season_model <- lm(Crash_std ~ Season_factor,
                    data = school_rdit)

school_rdit$Crash_adj <- resid(season_model) + mean(school_rdit$Crash_std)


## Jan 2025 Cutoff ##

# Prepare data for model
school_rdit2 <- school_ped |> 
  filter(ACCIDENT_YEAR %in% c(2024, 2025)) |> 
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


# Standardizing crash outcomes
pre_mean <- school_rdit2 |>
  filter(Post == 0) |>
  summarise(mean = mean(Total_crashes)) |>
  pull(mean)

pre_sd <- school_rdit2 |>
  filter(Post == 0) |>
  summarise(sd = sd(Total_crashes)) |>
  pull(sd)

school_rdit2 <- school_rdit2 |>
  mutate(Crash_std = (Total_crashes - pre_mean) / pre_sd)


# Run model
school_zone_model2 <- rdrobust(y = school_rdit2$Crash_std,
                              x = school_rdit2$Time,
                              covs = model.matrix(~ Season_factor, school_rdit2)[, -1],
                              c = 0,
                              p = 1,
                              h = 12,
                              kernel = "triangular")

summary(school_zone_model2)


# Adjusting for seasonality before plotting
season_model2 <- lm(Crash_std ~ Season_factor,
                   data = school_rdit2)

school_rdit2$Crash_adj <- resid(season_model2) + mean(school_rdit2$Crash_std)


# Combined Plots

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
  make_ci_band(school_rdit, "Crash_adj", function(t) t < 0, xseq_left),
  make_ci_band(school_rdit, "Crash_adj", function(t) t >= 0, xseq_right)
) |> mutate(Date = as.Date("2024-01-01") + Time * 30.4368 - nudge_days,
            Model = "Jan 2024 Cutoff")

ci_2025 <- bind_rows(
  make_ci_band(school_rdit2, "Crash_adj", function(t) t < 0, xseq_left),
  make_ci_band(school_rdit2, "Crash_adj", function(t) t >= 0, xseq_right)
) |> mutate(Date = as.Date("2025-01-01") + Time * 30.4368 + nudge_days,
            Model = "Jan 2025 Cutoff")


# combined plot, but scopes each panel to its own cutoff and window.
make_cutoff_plot <- function(ci_data, cutoff_date, model_label, line_color,
                             event_label, x_limits, x_breaks,
                             y_breaks = seq(-1.5, 1.5, by = 0.5)) {
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
    labs(title = model_label, x = "Month", 
         y = "Standardized Crash Count (z-score)") +
    theme(plot.title = element_text(size = 16, face = "bold"),
          axis.title.x = element_text(size = 13, face = "bold"),
          axis.title.y = element_text(size = 13, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1))
}

p_2024 <- make_cutoff_plot(
  ci_2024,
  cutoff_date = as.Date("2024-01-01"),
  model_label = "Pedestrian Crashes Within School Zones, Jan 2024 Cutoff",
  line_color  = "#0072B2",
  event_label = "Warning Begins",
  x_limits = c(as.Date("2023-01-01"), as.Date("2024-12-01")),
  x_breaks = seq(as.Date("2023-01-01"), as.Date("2024-12-01"), by = "2 months")
)

p_2025 <- make_cutoff_plot(
  ci_2025,
  cutoff_date = as.Date("2025-01-01"),
  model_label = "      Jan 2025 Cutoff",
  line_color  = "#D55E00",
  event_label = "Enforcement Begins",
  x_limits = c(as.Date("2024-01-01"), as.Date("2025-12-01")),
  x_breaks = seq(as.Date("2024-01-01"), as.Date("2025-12-01"), by = "2 months")
)

# Side by side 
p_2024 + p_2025


# Pedestrian Crashes Not Within School Zones ---------------------------------

school_ped2 <- school_data |> 
  filter(ACCIDENT_YEAR %in% c(2023, 2024, 2025)) |> 
  filter(in_school_zone == "FALSE")


## Jan 2024 Cutoff ##

# Prepare data for model
school_rdit3 <- school_ped2 |> 
  filter(ACCIDENT_YEAR %in% c(2023, 2024)) |> 
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


# Standardizing crash outcomes
pre_mean <- school_rdit3 |>
  filter(Post == 0) |>
  summarise(mean = mean(Total_crashes)) |>
  pull(mean)

pre_sd <- school_rdit3 |>
  filter(Post == 0) |>
  summarise(sd = sd(Total_crashes)) |>
  pull(sd)

school_rdit3 <- school_rdit3 |>
  mutate(Crash_std = (Total_crashes - pre_mean) / pre_sd)


# Run model
school_zone_model3 <- rdrobust(y = school_rdit3$Crash_std,
                              x = school_rdit3$Time,
                              covs = model.matrix(~ Season_factor, school_rdit3)[, -1],
                              c = 0,
                              p = 1,
                              h = 12,
                              kernel = "triangular")

summary(school_zone_model3)


# Adjusting for seasonality before plotting
season_model3 <- lm(Crash_std ~ Season_factor,
                   data = school_rdit3)

school_rdit3$Crash_adj <- resid(season_model3) + mean(school_rdit3$Crash_std)


## Jan 2025 Cutoff ##

# Prepare data for model
school_rdit4 <- school_ped2 |> 
  filter(ACCIDENT_YEAR %in% c(2024, 2025)) |> 
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


# Standardizing crash outcomes
pre_mean <- school_rdit4 |>
  filter(Post == 0) |>
  summarise(mean = mean(Total_crashes)) |>
  pull(mean)

pre_sd <- school_rdit4 |>
  filter(Post == 0) |>
  summarise(sd = sd(Total_crashes)) |>
  pull(sd)

school_rdit4 <- school_rdit4 |>
  mutate(Crash_std = (Total_crashes - pre_mean) / pre_sd)



# Run model
school_zone_model4 <- rdrobust(y = school_rdit4$Crash_std,
                               x = school_rdit4$Time,
                               covs = model.matrix(~ Season_factor, school_rdit4)[, -1],
                               c = 0,
                               p = 1,
                               h = 12,
                               kernel = "triangular")

summary(school_zone_model4)


# Adjusting for seasonality before plotting
season_model4 <- lm(Crash_std ~ Season_factor,
                    data = school_rdit4)

school_rdit4$Crash_adj <- resid(season_model4) + mean(school_rdit4$Crash_std)


# Combined Plots

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
  make_ci_band(school_rdit3, "Crash_adj", function(t) t < 0, xseq_left),
  make_ci_band(school_rdit3, "Crash_adj", function(t) t >= 0, xseq_right)
) |> mutate(Date = as.Date("2024-01-01") + Time * 30.4368 - nudge_days,
            Model = "Jan 2024 Cutoff")

ci_2025 <- bind_rows(
  make_ci_band(school_rdit4, "Crash_adj", function(t) t < 0, xseq_left),
  make_ci_band(school_rdit4, "Crash_adj", function(t) t >= 0, xseq_right)
) |> mutate(Date = as.Date("2025-01-01") + Time * 30.4368 + nudge_days,
            Model = "Jan 2025 Cutoff")


# combined plot, but scopes each panel to its own cutoff and window.
make_cutoff_plot <- function(ci_data, cutoff_date, model_label, line_color,
                             event_label, x_limits, x_breaks,
                             y_breaks = seq(-2, 2, by = 0.5)) {
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
    labs(title = model_label, x = "Month", 
         y = "Standardized Crash Count (z-score)") +
    theme(plot.title = element_text(size = 16, face = "bold"),
          axis.title.x = element_text(size = 13, face = "bold"),
          axis.title.y = element_text(size = 13, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1))
}

p_2024 <- make_cutoff_plot(
  ci_2024,
  cutoff_date = as.Date("2024-01-01"),
  model_label = "Pedestrian Crashes Not in School Zones, Jan 2024 Cutoff",
  line_color  = "#0072B2",
  event_label = "Warning Begins",
  x_limits = c(as.Date("2023-01-01"), as.Date("2024-12-01")),
  x_breaks = seq(as.Date("2023-01-01"), as.Date("2024-12-01"), by = "2 months")
)

p_2025 <- make_cutoff_plot(
  ci_2025,
  cutoff_date = as.Date("2025-01-01"),
  model_label = "      Jan 2025 Cutoff",
  line_color  = "#D55E00",
  event_label = "Enforcement Begins",
  x_limits = c(as.Date("2024-01-01"), as.Date("2025-12-01")),
  x_breaks = seq(as.Date("2024-01-01"), as.Date("2025-12-01"), by = "2 months")
)

# Side by side 
p_2024 + p_2025



# Pedestrian Intersection Crashes -----------------------------------------

## --- 1. Crashes Within School Zones --- ##

school_int <- school_data |> 
  filter(ACCIDENT_YEAR %in% c(2023, 2024, 2025)) |> 
  filter(in_school_zone == "TRUE" & INTERSECTION == "Y")


## A. Jan 2024 Cutoff ##

# Prepare data for model
school_int_rdit <- school_int |> 
  filter(ACCIDENT_YEAR %in% c(2023, 2024)) |> 
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


# Standardizing crash outcomes
pre_mean <- school_int_rdit |>
  filter(Post == 0) |>
  summarise(mean = mean(Total_crashes)) |>
  pull(mean)

pre_sd <- school_int_rdit |>
  filter(Post == 0) |>
  summarise(sd = sd(Total_crashes)) |>
  pull(sd)

school_int_rdit <- school_int_rdit |>
  mutate(Crash_std = (Total_crashes - pre_mean) / pre_sd)


# Run model
school_int_model <- rdrobust(y = school_int_rdit$Crash_std,
                              x = school_int_rdit$Time,
                              covs = model.matrix(~ Season_factor, school_int_rdit)[, -1],
                              c = 0,
                              p = 1,
                              h = 12,
                              kernel = "triangular")

summary(school_int_model)



# Adjusting for seasonality before plotting
season_model <- lm(Crash_std ~ Season_factor,
                   data = school_int_rdit)

school_int_rdit$Crash_adj <- resid(season_model) + mean(school_int_rdit$Crash_std)


## Jan 2025 Cutoff ##

# Prepare data for model
school_int_rdit2 <- school_int |> 
  filter(ACCIDENT_YEAR %in% c(2024, 2025)) |> 
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


# Standardizing crash outcomes
pre_mean <- school_int_rdit2 |>
  filter(Post == 0) |>
  summarise(mean = mean(Total_crashes)) |>
  pull(mean)

pre_sd <- school_int_rdit2 |>
  filter(Post == 0) |>
  summarise(sd = sd(Total_crashes)) |>
  pull(sd)

school_int_rdit2 <- school_int_rdit2 |>
  mutate(Crash_std = (Total_crashes - pre_mean) / pre_sd)


# Run model
school_int_model2 <- rdrobust(y = school_int_rdit2$Crash_std,
                               x = school_int_rdit2$Time,
                               covs = model.matrix(~ Season_factor, school_int_rdit2)[, -1],
                               c = 0,
                               p = 1,
                               h = 12,
                               kernel = "triangular")

summary(school_int_model2)


# Adjusting for seasonality before plotting
season_int_model2 <- lm(Crash_std ~ Season_factor,
                    data = school_int_rdit2)

school_int_rdit2$Crash_adj <- resid(season_int_model2) + mean(school_int_rdit2$Crash_std)


# Combined Plots

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
  make_ci_band(school_int_rdit, "Crash_adj", function(t) t < 0, xseq_left),
  make_ci_band(school_int_rdit, "Crash_adj", function(t) t >= 0, xseq_right)
) |> mutate(Date = as.Date("2024-01-01") + Time * 30.4368 - nudge_days,
            Model = "Jan 2024 Cutoff")

ci_2025 <- bind_rows(
  make_ci_band(school_int_rdit2, "Crash_adj", function(t) t < 0, xseq_left),
  make_ci_band(school_int_rdit2, "Crash_adj", function(t) t >= 0, xseq_right)
) |> mutate(Date = as.Date("2025-01-01") + Time * 30.4368 + nudge_days,
            Model = "Jan 2025 Cutoff")


# combined plot, but scopes each panel to its own cutoff and window.
make_cutoff_plot <- function(ci_data, cutoff_date, model_label, line_color,
                             event_label, x_limits, x_breaks,
                             y_breaks = seq(-1.5, 1.5, by = 0.5)) {
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
    labs(title = model_label, x = "Month", 
         y = "Standardized Crash Count (z-score)") +
    theme(plot.title = element_text(size = 16, face = "bold"),
          axis.title.x = element_text(size = 13, face = "bold"),
          axis.title.y = element_text(size = 13, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1))
}

p_2024 <- make_cutoff_plot(
  ci_2024,
  cutoff_date = as.Date("2024-01-01"),
  model_label = "Ped-Int Crashes Within School Zones, Jan 2024 Cutoff",
  line_color  = "#0072B2",
  event_label = "Warning Begins",
  x_limits = c(as.Date("2023-01-01"), as.Date("2024-12-01")),
  x_breaks = seq(as.Date("2023-01-01"), as.Date("2024-12-01"), by = "2 months")
)

p_2025 <- make_cutoff_plot(
  ci_2025,
  cutoff_date = as.Date("2025-01-01"),
  model_label = "      Jan 2025 Cutoff",
  line_color  = "#D55E00",
  event_label = "Enforcement Begins",
  x_limits = c(as.Date("2024-01-01"), as.Date("2025-12-01")),
  x_breaks = seq(as.Date("2024-01-01"), as.Date("2025-12-01"), by = "2 months")
)

# Side by side 
p_2024 + p_2025


## --- 2. Crashes out of School Zones --- ##
school_int2 <- school_data |> 
  filter(ACCIDENT_YEAR %in% c(2023, 2024, 2025)) |> 
  filter(in_school_zone == "FALSE" & INTERSECTION == "Y")


## Jan 2024 Cutoff ##

# Prepare data for model
school_int_rdit3 <- school_int2 |> 
  filter(ACCIDENT_YEAR %in% c(2023, 2024)) |> 
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


# Standardizing crash outcomes
pre_mean <- school_int_rdit3 |>
  filter(Post == 0) |>
  summarise(mean = mean(Total_crashes)) |>
  pull(mean)

pre_sd <- school_int_rdit3 |>
  filter(Post == 0) |>
  summarise(sd = sd(Total_crashes)) |>
  pull(sd)

school_int_rdit3 <- school_int_rdit3 |>
  mutate(Crash_std = (Total_crashes - pre_mean) / pre_sd)


# Run model
school_int_model3 <- rdrobust(y = school_int_rdit3$Crash_std,
                               x = school_int_rdit3$Time,
                               covs = model.matrix(~ Season_factor, school_int_rdit3)[, -1],
                               c = 0,
                               p = 1,
                               h = 12,
                               kernel = "triangular")

summary(school_int_model3)


# Adjusting for seasonality before plotting
season_int_model3 <- lm(Crash_std ~ Season_factor,
                    data = school_int_rdit3)

school_int_rdit3$Crash_adj <- resid(season_int_model3) + mean(school_int_rdit3$Crash_std)


## Jan 2025 Cutoff ##

# Prepare data for model
school_int_rdit4 <- school_int2 |> 
  filter(ACCIDENT_YEAR %in% c(2024, 2025)) |> 
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


# Standardizing crash outcomes
pre_mean <- school_int_rdit4 |>
  filter(Post == 0) |>
  summarise(mean = mean(Total_crashes)) |>
  pull(mean)

pre_sd <- school_int_rdit4 |>
  filter(Post == 0) |>
  summarise(sd = sd(Total_crashes)) |>
  pull(sd)

school_int_rdit4 <- school_int_rdit4 |>
  mutate(Crash_std = (Total_crashes - pre_mean) / pre_sd)



# Run model
school_int_model4 <- rdrobust(y = school_int_rdit4$Crash_std,
                               x = school_int_rdit4$Time,
                               covs = model.matrix(~ Season_factor, school_int_rdit4)[, -1],
                               c = 0,
                               p = 1,
                               h = 12,
                               kernel = "triangular")

summary(school_int_model4)


# Adjusting for seasonality before plotting
season_int_model4 <- lm(Crash_std ~ Season_factor,
                    data = school_int_rdit4)

school_int_rdit4$Crash_adj <- resid(season_int_model4) + mean(school_int_rdit4$Crash_std)


# Combined Plots

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
  make_ci_band(school_int_rdit3, "Crash_adj", function(t) t < 0, xseq_left),
  make_ci_band(school_int_rdit3, "Crash_adj", function(t) t >= 0, xseq_right)
) |> mutate(Date = as.Date("2024-01-01") + Time * 30.4368 - nudge_days,
            Model = "Jan 2024 Cutoff")

ci_2025 <- bind_rows(
  make_ci_band(school_int_rdit4, "Crash_adj", function(t) t < 0, xseq_left),
  make_ci_band(school_int_rdit4, "Crash_adj", function(t) t >= 0, xseq_right)
) |> mutate(Date = as.Date("2025-01-01") + Time * 30.4368 + nudge_days,
            Model = "Jan 2025 Cutoff")


# combined plot, but scopes each panel to its own cutoff and window.
make_cutoff_plot <- function(ci_data, cutoff_date, model_label, line_color,
                             event_label, x_limits, x_breaks,
                             y_breaks = seq(-2, 2, by = 0.5)) {
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
    labs(title = model_label, x = "Month", 
         y = "Standardized Crash Count (z-score)") +
    theme(plot.title = element_text(size = 16, face = "bold"),
          axis.title.x = element_text(size = 13, face = "bold"),
          axis.title.y = element_text(size = 13, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1))
}

p_2024 <- make_cutoff_plot(
  ci_2024,
  cutoff_date = as.Date("2024-01-01"),
  model_label = "Ped-Int Crashes Not in School Zones, Jan 2024 Cutoff",
  line_color  = "#0072B2",
  event_label = "Warning Begins",
  x_limits = c(as.Date("2023-01-01"), as.Date("2024-12-01")),
  x_breaks = seq(as.Date("2023-01-01"), as.Date("2024-12-01"), by = "2 months")
)

p_2025 <- make_cutoff_plot(
  ci_2025,
  cutoff_date = as.Date("2025-01-01"),
  model_label = "      Jan 2025 Cutoff",
  line_color  = "#D55E00",
  event_label = "Enforcement Begins",
  x_limits = c(as.Date("2024-01-01"), as.Date("2025-12-01")),
  x_breaks = seq(as.Date("2024-01-01"), as.Date("2025-12-01"), by = "2 months")
)

# Side by side 
p_2024 + p_2025





