
# ------------------------------------------------------------------------

## RDiT by crash severity
## Inputs: initial-analysis/data-clean/updated_tims.rds

# ------------------------------------------------------------------------


# Set Up -----------------------------------------------------------------

# load libraries and dataset
library(tidyverse)
library(rdrobust)
library(scales)

tims_data <-  readRDS("initial-analysis/data-clean/updated_tims.rds")


# Monthly Proportion
severity_monthly <- tims_data |> 
  filter(ACCIDENT_YEAR %in% c("2023", "2024", "2025"),
         PED_ACTION == "B",
         INTERSECTION == "Y") |> 
  filter(!is.na(COLLISION_SEVERITY)) |>
  mutate(MONTH_DATE = floor_date(COLLISION_DATE, "month"),
         COLLISION_SEVERITY = case_when(
           COLLISION_SEVERITY == 1 ~ "Fatal Injury",
           COLLISION_SEVERITY == 2 ~ "Suspected Serious Injury",
           COLLISION_SEVERITY == 3 ~ "Suspected Minor Injury",
           COLLISION_SEVERITY == 4 ~ "Possible Injury",
           COLLISION_SEVERITY == 0 ~ "No Injury (Property Damage)"
         )) |>   
  group_by(ACCIDENT_YEAR, MONTH_DATE, COLLISION_SEVERITY) |> 
  summarise(CRASHES = n(), .groups = "drop")



# Fatal  -----------------------------------------------------------

## Jan 2024 Cutoff ##
# Prepare data
fatal_data <- severity_monthly |> 
  filter(ACCIDENT_YEAR %in% c("2023", "2024")) |> 
  filter(COLLISION_SEVERITY == "Fatal Injury") |> 
  mutate(Time = interval(as.Date("2024-01-01"), MONTH_DATE) %/% months(1),
         Post = ifelse(Time >= 0, 1, 0),
         Season_factor = factor(case_when(
           month(MONTH_DATE) %in% c(12, 1, 2) ~ "Winter",
           month(MONTH_DATE) %in% c(3, 4, 5) ~ "Spring",
           month(MONTH_DATE) %in% c(6, 7, 8) ~ "Summer",
           month(MONTH_DATE) %in% c(9, 10, 11) ~ "Fall"
         ),
         levels = c("Winter", "Spring", "Summer", "Fall")
         ))


# Run the model
fatal_model <- rdrobust(y = fatal_data$CRASHES,
                           x = fatal_data$Time,
                           covs = model.matrix(~ Season_factor, fatal_data)[, -1],
                           c = 0,
                           p = 1,
                           h = 12,
                           kernel = "triangular")

summary(fatal_model)

# Adjusting for seasonality before plotting
season_fatal_model <- lm(CRASHES ~ Season_factor,
                            data = fatal_data)

fatal_data$Crash_adj <- resid(season_fatal_model) + 
  mean(fatal_data$CRASHES)



## Jan 2025 ##
# Prepare data
fatal_data2 <- severity_monthly |> 
  filter(ACCIDENT_YEAR %in% c("2024", "2025")) |> 
  filter(COLLISION_SEVERITY == "Fatal Injury") |> 
  mutate(Time = interval(as.Date("2025-01-01"), MONTH_DATE) %/% months(1),
         Post = ifelse(Time >= 0, 1, 0),
         Season_factor = factor(case_when(
           month(MONTH_DATE) %in% c(12, 1, 2) ~ "Winter",
           month(MONTH_DATE) %in% c(3, 4, 5) ~ "Spring",
           month(MONTH_DATE) %in% c(6, 7, 8) ~ "Summer",
           month(MONTH_DATE) %in% c(9, 10, 11) ~ "Fall"
         ),
         levels = c("Winter", "Spring", "Summer", "Fall")
         ))


# Run model
fatal_model2 <- rdrobust(y = fatal_data2$CRASHES,
                            x = fatal_data2$Time,
                            covs = model.matrix(~ Season_factor, fatal_data2)[, -1],
                            c = 0,
                            p = 1,
                            h = 12,
                            kernel = "triangular")

summary(fatal_model2)

# Adjusting for seasonality before plotting
season_fatal_model2 <- lm(CRASHES~ Season_factor,
                             data = fatal_data2)

fatal_data2$Crash_adj <- resid(season_fatal_model2) + 
  mean(fatal_data2$CRASHES)


# Combined plot with CIs

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
  make_ci_band(fatal_data, "Crash_adj", function(t) t < 0, xseq_left),
  make_ci_band(fatal_data, "Crash_adj", function(t) t >= 0, xseq_right)
) |> mutate(Date = as.Date("2024-01-01") + Time * 30.4368 - nudge_days,
            Model = "Jan 2024 Cutoff")

ci_2025 <- bind_rows(
  make_ci_band(fatal_data2, "Crash_adj", function(t) t < 0, xseq_left),
  make_ci_band(fatal_data2, "Crash_adj", function(t) t >= 0, xseq_right)
) |> mutate(Date = as.Date("2025-01-01") + Time * 30.4368 + nudge_days,
            Model = "Jan 2025 Cutoff")

ci_all <- bind_rows(ci_2024, ci_2025)


# --- Plot ---
ggplot() +
  geom_ribbon(data = ci_all, aes(x = Date, ymin = lwr, ymax = upr, fill = Model), alpha = 0.2) +
  geom_line(data = filter(ci_all, Time < 0),
            aes(Date, fit, color = Model),
            linewidth = 1) +
  
  geom_line(data = filter(ci_all, Time > 0),
            aes(Date, fit, color = Model),
            linewidth = 1) +
  
  geom_vline(xintercept = as.Date("2024-01-01"), linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.Date("2025-01-01"), linetype = "dashed", color = "black") +
  
  scale_y_continuous(breaks = seq(1, 15, by = 2)) +
  
  scale_x_date(limits = c(as.Date("2023-01-01"),
                          as.Date("2025-12-01")),
               breaks = seq(from = as.Date("2023-01-01"),
                            to   = as.Date("2025-11-01"),
                            by   = "2 months"),
               date_labels = "%b %Y",
               expand = c(0.01, 0)) +
  
  scale_color_manual(values = c("Jan 2024 Cutoff" = "#0072B2",
                                "Jan 2025 Cutoff" = "#D55E00")) +
  
  scale_fill_manual(values = c("Jan 2024 Cutoff" = "#0072B2",
                               "Jan 2025 Cutoff" = "#D55E00")) +
  
  theme_minimal(base_size = 13) +
  labs(title = "Fatal RDiT Models",
       x = "Month", y = "Crash Rate",
       color = "Cutoff", fill = "Cutoff") +
  
  annotate("text",
           x = as.Date("2024-01-01"),
           y = Inf,
           label = "Warning Begins",
           vjust = 1.5,
           fontface = "bold",
           size = 4) +
  
  annotate("text",
           x = as.Date("2025-01-01"),
           y = Inf,
           label = "Enforcement Begins",
           vjust = 1.5,
           fontface = "bold",
           size = 4) +
  
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 13, face = "bold"),
        axis.title.y = element_text(size = 13, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = "initial-analysis/figs/fatal_models.png",
       height = 10, 
       width = 20)


# Possible Injury ---------------------------------------------------------

## Jan 2024 Cutoff ##
# Prepare data
possible_data <- severity_monthly |> 
  filter(ACCIDENT_YEAR %in% c("2023", "2024")) |> 
  filter(COLLISION_SEVERITY == "Possible Injury") |> 
  mutate(Time = interval(as.Date("2024-01-01"), MONTH_DATE) %/% months(1),
         Post = ifelse(Time >= 0, 1, 0),
         Season_factor = factor(case_when(
           month(MONTH_DATE) %in% c(12, 1, 2) ~ "Winter",
           month(MONTH_DATE) %in% c(3, 4, 5) ~ "Spring",
           month(MONTH_DATE) %in% c(6, 7, 8) ~ "Summer",
           month(MONTH_DATE) %in% c(9, 10, 11) ~ "Fall"
         ),
         levels = c("Winter", "Spring", "Summer", "Fall")
         ))


# Run the model
possible_model <- rdrobust(y = possible_data$CRASHES,
                        x = possible_data$Time,
                        covs = model.matrix(~ Season_factor, possible_data)[, -1],
                        c = 0,
                        p = 1,
                        h = 12,
                        kernel = "triangular")

summary(possible_model)

# Adjusting for seasonality before plotting
season_possible_model <- lm(CRASHES ~ Season_factor,
                         data = possible_data)

possible_data$Crash_adj <- resid(season_possible_model) + 
  mean(possible_data$CRASHES)



## Jan 2025 ##
# Prepare data
possible_data2 <- severity_monthly |> 
  filter(ACCIDENT_YEAR %in% c("2024", "2025")) |> 
  filter(COLLISION_SEVERITY == "Possible Injury") |> 
  mutate(Time = interval(as.Date("2025-01-01"), MONTH_DATE) %/% months(1),
         Post = ifelse(Time >= 0, 1, 0),
         Season_factor = factor(case_when(
           month(MONTH_DATE) %in% c(12, 1, 2) ~ "Winter",
           month(MONTH_DATE) %in% c(3, 4, 5) ~ "Spring",
           month(MONTH_DATE) %in% c(6, 7, 8) ~ "Summer",
           month(MONTH_DATE) %in% c(9, 10, 11) ~ "Fall"
         ),
         levels = c("Winter", "Spring", "Summer", "Fall")
         ))


# Run model
possible_model2 <- rdrobust(y = possible_data2$CRASHES,
                         x = possible_data2$Time,
                         covs = model.matrix(~ Season_factor, possible_data2)[, -1],
                         c = 0,
                         p = 1,
                         h = 12,
                         kernel = "triangular")

summary(possible_model2)

# Adjusting for seasonality before plotting
season_possible_model2 <- lm(CRASHES ~ Season_factor,
                          data = possible_data2)

possible_data2$Crash_adj <- resid(season_possible_model2) + mean(possible_data2$CRASHES)


# Combined plot with CIs

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
  make_ci_band(possible_data, "Crash_adj", function(t) t < 0, xseq_left),
  make_ci_band(possible_data, "Crash_adj", function(t) t >= 0, xseq_right)
) |> mutate(Date = as.Date("2024-01-01") + Time * 30.4368 - nudge_days,
            Model = "Jan 2024 Cutoff")

ci_2025 <- bind_rows(
  make_ci_band(possible_data2, "Crash_adj", function(t) t < 0, xseq_left),
  make_ci_band(possible_data2, "Crash_adj", function(t) t >= 0, xseq_right)
) |> mutate(Date = as.Date("2025-01-01") + Time * 30.4368 + nudge_days,
            Model = "Jan 2025 Cutoff")

ci_all <- bind_rows(ci_2024, ci_2025)


# --- Plot ---
ggplot() +
  geom_ribbon(data = ci_all, aes(x = Date, ymin = lwr, ymax = upr, fill = Model), alpha = 0.2) +
  geom_line(data = filter(ci_all, Time < 0),
            aes(Date, fit, color = Model),
            linewidth = 1) +
  
  geom_line(data = filter(ci_all, Time > 0),
            aes(Date, fit, color = Model),
            linewidth = 1) +
  
  geom_vline(xintercept = as.Date("2024-01-01"), linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.Date("2025-01-01"), linetype = "dashed", color = "black") +
  
  scale_y_continuous(breaks = seq(60, 150, by = 20)) +
  
  scale_x_date(limits = c(as.Date("2023-01-01"),
                          as.Date("2025-12-01")),
               breaks = seq(from = as.Date("2023-01-01"),
                            to   = as.Date("2025-11-01"),
                            by   = "2 months"),
               date_labels = "%b %Y",
               expand = c(0.01, 0)) +
  
  scale_color_manual(values = c("Jan 2024 Cutoff" = "#0072B2",
                                "Jan 2025 Cutoff" = "#D55E00")) +
  
  scale_fill_manual(values = c("Jan 2024 Cutoff" = "#0072B2",
                               "Jan 2025 Cutoff" = "#D55E00")) +
  
  theme_minimal(base_size = 13) +
  labs(title = "Possible Injury (Complaint of Pain) RDiT Models",
       x = "Month", y = "Crash Count",
       color = "Cutoff", fill = "Cutoff") +
  
  annotate("text",
           x = as.Date("2024-01-01"),
           y = Inf,
           label = "Warning Begins",
           vjust = 1.5,
           fontface = "bold",
           size = 4) +
  
  annotate("text",
           x = as.Date("2025-01-01"),
           y = Inf,
           label = "Enforcement Begins",
           vjust = 1.5,
           fontface = "bold",
           size = 4) +
  
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 13, face = "bold"),
        axis.title.y = element_text(size = 13, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = "initial-analysis/figs/possible_inj_models.png",
       height = 10, 
       width = 20)


# Minor Injury  ----------------------------------------------------------

## Jan 2024 Cutoff ##
# Prepare data
minor_data <- severity_monthly |> 
  filter(ACCIDENT_YEAR %in% c("2023", "2024")) |> 
  filter(COLLISION_SEVERITY == "Suspected Minor Injury") |> 
  mutate(Time = interval(as.Date("2024-01-01"), MONTH_DATE) %/% months(1),
         Post = ifelse(Time >= 0, 1, 0),
         Season_factor = factor(case_when(
           month(MONTH_DATE) %in% c(12, 1, 2) ~ "Winter",
           month(MONTH_DATE) %in% c(3, 4, 5) ~ "Spring",
           month(MONTH_DATE) %in% c(6, 7, 8) ~ "Summer",
           month(MONTH_DATE) %in% c(9, 10, 11) ~ "Fall"
         ),
         levels = c("Winter", "Spring", "Summer", "Fall")
         ))


# Run the model
minor_model <- rdrobust(y = minor_data$CRASHES,
                        x = minor_data$Time,
                        covs = model.matrix(~ Season_factor, minor_data)[, -1],
                        c = 0,
                        p = 1,
                        h = 12,
                        kernel = "triangular")

summary(minor_model)

# Adjusting for seasonality before plotting
season_minor_model <- lm(CRASHES~ Season_factor,
                         data = minor_data)

minor_data$Crash_adj <- resid(season_minor_model) + 
  mean(minor_data$CRASHES)



## Jan 2025 ##
# Prepare data
minor_data2 <- severity_monthly |> 
  filter(ACCIDENT_YEAR %in% c("2024", "2025")) |> 
  filter(COLLISION_SEVERITY == "Suspected Minor Injury") |> 
  mutate(Time = interval(as.Date("2025-01-01"), MONTH_DATE) %/% months(1),
         Post = ifelse(Time >= 0, 1, 0),
         Season_factor = factor(case_when(
           month(MONTH_DATE) %in% c(12, 1, 2) ~ "Winter",
           month(MONTH_DATE) %in% c(3, 4, 5) ~ "Spring",
           month(MONTH_DATE) %in% c(6, 7, 8) ~ "Summer",
           month(MONTH_DATE) %in% c(9, 10, 11) ~ "Fall"
         ),
         levels = c("Winter", "Spring", "Summer", "Fall")
         ))


# Run model
minor_model2 <- rdrobust(y = minor_data2$CRASHES,
                         x = minor_data2$Time,
                         covs = model.matrix(~ Season_factor, minor_data2)[, -1],
                         c = 0,
                         p = 1,
                         h = 12,
                         kernel = "triangular")

summary(minor_model2)

# Adjusting for seasonality before plotting
season_minor_model2 <- lm(CRASHES~ Season_factor,
                          data = minor_data2)

minor_data2$Crash_adj <- resid(season_minor_model2) + 
  mean(minor_data2$CRASHES)


# Combined plot with CIs

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
  make_ci_band(minor_data, "Crash_adj", function(t) t < 0, xseq_left),
  make_ci_band(minor_data, "Crash_adj", function(t) t >= 0, xseq_right)
) |> mutate(Date = as.Date("2024-01-01") + Time * 30.4368 - nudge_days,
            Model = "Jan 2024 Cutoff")

ci_2025 <- bind_rows(
  make_ci_band(minor_data2, "Crash_adj", function(t) t < 0, xseq_left),
  make_ci_band(minor_data2, "Crash_adj", function(t) t >= 0, xseq_right)
) |> mutate(Date = as.Date("2025-01-01") + Time * 30.4368 + nudge_days,
            Model = "Jan 2025 Cutoff")

ci_all <- bind_rows(ci_2024, ci_2025)


# --- Plot ---
ggplot() +
  geom_ribbon(data = ci_all, aes(x = Date, ymin = lwr, ymax = upr, fill = Model), alpha = 0.2) +
  geom_line(data = filter(ci_all, Time < 0),
            aes(Date, fit, color = Model),
            linewidth = 1) +
  
  geom_line(data = filter(ci_all, Time > 0),
            aes(Date, fit, color = Model),
            linewidth = 1) +
  
  geom_vline(xintercept = as.Date("2024-01-01"), linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.Date("2025-01-01"), linetype = "dashed", color = "black") +
  
  scale_y_continuous(breaks = seq(60, 140, by = 20)) +
  
  scale_x_date(limits = c(as.Date("2023-01-01"),
                          as.Date("2025-12-01")),
               breaks = seq(from = as.Date("2023-01-01"),
                            to   = as.Date("2025-11-01"),
                            by   = "2 months"),
               date_labels = "%b %Y",
               expand = c(0.01, 0)) +
  
  scale_color_manual(values = c("Jan 2024 Cutoff" = "#0072B2",
                                "Jan 2025 Cutoff" = "#D55E00")) +
  
  scale_fill_manual(values = c("Jan 2024 Cutoff" = "#0072B2",
                               "Jan 2025 Cutoff" = "#D55E00")) +
  
  theme_minimal(base_size = 13) +
  labs(title = "Suspected Minor Injury RDiT Models",
       x = "Month", y = "Crash Count",
       color = "Cutoff", fill = "Cutoff") +
  
  annotate("text",
           x = as.Date("2024-01-01"),
           y = Inf,
           label = "Warning Begins",
           vjust = 1.5,
           fontface = "bold",
           size = 4) +
  
  annotate("text",
           x = as.Date("2025-01-01"),
           y = Inf,
           label = "Enforcement Begins",
           vjust = 1.5,
           fontface = "bold",
           size = 4) +
  
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 13, face = "bold"),
        axis.title.y = element_text(size = 13, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = "initial-analysis/figs/minor_inj_models.png",
       height = 10, 
       width = 20)


# Severe Injury  ---------------------------------------------------------

## Jan 2024 Cutoff ##
# Prepare data
severe_data <- severity_monthly |> 
  filter(ACCIDENT_YEAR %in% c("2023", "2024")) |> 
  filter(COLLISION_SEVERITY == "Suspected Serious Injury") |> 
  mutate(Time = interval(as.Date("2024-01-01"), MONTH_DATE) %/% months(1),
         Post = ifelse(Time >= 0, 1, 0),
         Season_factor = factor(case_when(
           month(MONTH_DATE) %in% c(12, 1, 2) ~ "Winter",
           month(MONTH_DATE) %in% c(3, 4, 5) ~ "Spring",
           month(MONTH_DATE) %in% c(6, 7, 8) ~ "Summer",
           month(MONTH_DATE) %in% c(9, 10, 11) ~ "Fall"
         ),
         levels = c("Winter", "Spring", "Summer", "Fall")
         ))


# Run the model
severe_model <- rdrobust(y = severe_data$CRASHES,
                        x = severe_data$Time,
                        covs = model.matrix(~ Season_factor, severe_data)[, -1],
                        c = 0,
                        p = 1,
                        h = 12,
                        kernel = "triangular")

summary(severe_model)

# Adjusting for seasonality before plotting
season_severe_model <- lm(CRASHES ~ Season_factor,
                         data = severe_data)

severe_data$Crash_adj <- resid(season_severe_model) + 
  mean(severe_data$CRASHES)



## Jan 2025 ##
# Prepare data
severe_data2 <- severity_monthly |> 
  filter(ACCIDENT_YEAR %in% c("2024", "2025")) |> 
  filter(COLLISION_SEVERITY == "Suspected Serious Injury") |> 
  mutate(Time = interval(as.Date("2025-01-01"), MONTH_DATE) %/% months(1),
         Post = ifelse(Time >= 0, 1, 0),
         Season_factor = factor(case_when(
           month(MONTH_DATE) %in% c(12, 1, 2) ~ "Winter",
           month(MONTH_DATE) %in% c(3, 4, 5) ~ "Spring",
           month(MONTH_DATE) %in% c(6, 7, 8) ~ "Summer",
           month(MONTH_DATE) %in% c(9, 10, 11) ~ "Fall"
         ),
         levels = c("Winter", "Spring", "Summer", "Fall")
         ))



# Run model
severe_model2 <- rdrobust(y = severe_data2$CRASHES,
                         x = severe_data2$Time,
                         covs = model.matrix(~ Season_factor, severe_data2)[, -1],
                         c = 0,
                         p = 1,
                         h = 12,
                         kernel = "triangular")

summary(severe_model2)

# Adjusting for seasonality before plotting
season_severe_model2 <- lm(CRASHES ~ Season_factor,
                          data = severe_data2)

severe_data2$Crash_adj <- resid(season_severe_model2) + mean(severe_data2$CRASHES)


# Combined plot with CIs

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
  make_ci_band(severe_data, "Crash_adj", function(t) t < 0, xseq_left),
  make_ci_band(severe_data, "Crash_adj", function(t) t >= 0, xseq_right)
) |> mutate(Date = as.Date("2024-01-01") + Time * 30.4368 - nudge_days,
            Model = "Jan 2024 Cutoff")

ci_2025 <- bind_rows(
  make_ci_band(severe_data2, "Crash_adj", function(t) t < 0, xseq_left),
  make_ci_band(severe_data2, "Crash_adj", function(t) t >= 0, xseq_right)
) |> mutate(Date = as.Date("2025-01-01") + Time * 30.4368 + nudge_days,
            Model = "Jan 2025 Cutoff")

ci_all <- bind_rows(ci_2024, ci_2025)


# --- Plot ---
ggplot() +
  geom_ribbon(data = ci_all, aes(x = Date, ymin = lwr, ymax = upr, fill = Model), alpha = 0.2) +
  geom_line(data = filter(ci_all, Time < 0),
            aes(Date, fit, color = Model),
            linewidth = 1) +
  
  geom_line(data = filter(ci_all, Time > 0),
            aes(Date, fit, color = Model),
            linewidth = 1) +
  
  geom_vline(xintercept = as.Date("2024-01-01"), linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.Date("2025-01-01"), linetype = "dashed", color = "black") +
  
  scale_y_continuous(breaks = seq(20, 70, by = 10)) +
  
  scale_x_date(limits = c(as.Date("2023-01-01"),
                          as.Date("2025-12-01")),
               breaks = seq(from = as.Date("2023-01-01"),
                            to   = as.Date("2025-11-01"),
                            by   = "2 months"),
               date_labels = "%b %Y",
               expand = c(0.01, 0)) +
  
  scale_color_manual(values = c("Jan 2024 Cutoff" = "#0072B2",
                                "Jan 2025 Cutoff" = "#D55E00")) +
  
  scale_fill_manual(values = c("Jan 2024 Cutoff" = "#0072B2",
                               "Jan 2025 Cutoff" = "#D55E00")) +
  
  theme_minimal(base_size = 13) +
  labs(title = "Suspected Serious Injury RDiT Models",
       x = "Month", y = "Crash Count",
       color = "Cutoff", fill = "Cutoff") +
  
  annotate("text",
           x = as.Date("2024-01-01"),
           y = Inf,
           label = "Warning Begins",
           vjust = 1.5,
           fontface = "bold",
           size = 4) +
  
  annotate("text",
           x = as.Date("2025-01-01"),
           y = Inf,
           label = "Enforcement Begins",
           vjust = 1.5,
           fontface = "bold",
           size = 4) +
  
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 13, face = "bold"),
        axis.title.y = element_text(size = 13, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = "initial-analysis/figs/severe_inj_models.png",
       height = 10, 
       width = 20)




# BAR PLOT ---------------------------------------------------------------

# color palette
sunflower <- c(
  "#F2C94C",
  "#1E4E8C",
  "#D4A04A",
  "#8A9B5B",
  "#F7F4E7"
)

# Extract coefficients from rdrobust
extract_rd <- function(model, severity, cutoff){
  
  tibble(
    Severity = severity,
    Cutoff = cutoff,
    Effect = model$coef[3,1],
    SE = model$se[3,1],
    P_value = model$pv[3,1],
    CI_lower = model$ci[3,1],
    CI_upper = model$ci[3,2]
  )
  
}

# Build datframes for each category
severity_effects <- bind_rows(
  
  extract_rd(fatal_model,      "Fatal", "Jan 2024"),
  extract_rd(fatal_model2,     "Fatal", "Jan 2025"),
  
  extract_rd(severe_model,     "Serious", "Jan 2024"),
  extract_rd(severe_model2,    "Serious", "Jan 2025"),
  
  extract_rd(minor_model,      "Minor", "Jan 2024"),
  extract_rd(minor_model2,     "Minor", "Jan 2025"),
  
  extract_rd(possible_model,   "Possible", "Jan 2024"),
  extract_rd(possible_model2,  "Possible", "Jan 2025")
  
) |>
  
  mutate(Severity = factor(Severity,
                           levels = c("Fatal", "Serious", "Minor", "Possible")),
         Cutoff = factor(Cutoff, 
                         levels = c("Jan 2024","Jan 2025")),
         Sig = case_when(
           P_value < 0.01 ~ "***",
           P_value < 0.05 ~ "**",
           P_value < 0.10 ~ "*",
           TRUE ~ ""
         ), 
         Label = round(Effect, 2))

# Plot
ggplot(severity_effects,
       aes(Severity, Effect, fill = Cutoff)) +
  
  geom_col(position = position_dodge(width = 0.6),
           width = .55) +
  
  geom_text(aes(label = Label,
                vjust = ifelse(Effect < 0, 1.15, -0.35),
                color = ifelse(Sig != "" & !is.na(Sig), "#800000", "black")),
            position = position_dodge(width = .65),
            fontface = "bold",
            size = 4.5) +
  
  geom_text(aes(label = Sig,
                y = ifelse(Effect < 0, Effect - 2, Effect + 2),
                vjust = ifelse(Effect < 0, 0, -0.7), 
                hjust = -1.5,
                color = "#800000"), 
            position = position_dodge(width = 0.65), 
            size = 4, 
            fontface = "bold") +
  
  scale_color_identity() +  
  
  scale_x_discrete(expand = expansion(mult = c(0.1, 0.1))) +
  
  geom_hline(yintercept = 0,
             linewidth = .5) +
  
  scale_fill_manual(values = sunflower) +
  
  labs(title = "RD Effect by Crash Severity",
       subtitle = "Comparison of January 2024 and January 2025 Cutoffs",
       x = NULL,
       y = "RD Effect",
       fill = NULL,
       caption = "* Significant at the 10% level; ** Significant at the 5% level; *** Significant at the 1% level") +
  
  theme_minimal(base_size = 13) +
  
  theme(legend.position = "bottom",
        plot.caption = element_text(hjust = 0.5, 
                                    face = "italic", 
                                    size = 10,
                                    color = "#800000"),  
        axis.text.x = element_text(face = "bold", size = 13),
        plot.title = element_text(face = "bold", size = 17),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

# save
ggsave(filename = "initial-analysis/figs/severity-analysis.png",
       width = 10,
       height = 7.5)
