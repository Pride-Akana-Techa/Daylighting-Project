
# -------------------------------------------------------------------------
## Investigating the relationship between lighting condition and pedestrian      crashes at intersections at the cutoff dates
## Inputs: initial-analysis/data-clean/updated_tims.rds

# -------------------------------------------------------------------------

# load libraries and dataset
library(tidyverse)
library(rdrobust)
library(scales)
library(patchwork)

tims_data <-  readRDS("initial-analysis/data-clean/updated_tims.rds")

# Check yearly lighting condition
lighting_distribution <- tims_data |> 
  filter(ACCIDENT_YEAR >= "2022" &
           PED_ACTION == "B" &
           INTERSECTION == "Y") |> 
  filter_out(is.na(LIGHTING)) |> 
  group_by(ACCIDENT_YEAR, MONTH, LIGHTING)|> 
  summarise(CRASHES = n(),
            .groups = "drop")


# Monthly Proportion
lighting_monthly <- tims_data |> 
  filter(ACCIDENT_YEAR %in% c("2023", "2024", "2025"),
         PED_ACTION == "B",
         INTERSECTION == "Y") |> 
  filter(!is.na(LIGHTING)) |>
  mutate(MONTH_DATE = floor_date(COLLISION_DATE, "month")) |>   
  group_by(ACCIDENT_YEAR, MONTH_DATE, LIGHTING) |> 
  summarise(CRASHES = n(), .groups = "drop")



# Daylight -----------------------------------------------------------

## Jan 2024 Cutoff ##
# Prepare data
daylight_data <- lighting_monthly |> 
  filter(ACCIDENT_YEAR %in% c("2023", "2024")) |> 
  filter(LIGHTING == "Daylight") |> 
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


daylight_model <- rdrobust(y = daylight_data$CRASHES,
                        x = daylight_data$Time,
                        covs = model.matrix(~ Season_factor, daylight_data)[, -1],
                        c = 0,
                        p = 1,
                        h = 12,
                        kernel = "triangular")

summary(daylight_model)

# Adjusting for seasonality before plotting
season_daylight_model <- lm(CRASHES ~ Season_factor,
                         data = daylight_data)

daylight_data$Crash_adj <- resid(season_daylight_model) + mean(daylight_data$CRASHES)

# plot
daylight_rd_out <- rdplot(y = daylight_data$Crash_adj,
                 x = daylight_data$Time,
                 c = 0,
                 p = 1,
                 h = 12,
                 kernel = "triangular",
                 nbins = c(12, 12))

daylight_rd_out$rdplot +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(y = "Crash Rate",
       title = "Daylight RDiT",
       x = "Months Relative to Jan 2024") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )

ggsave(filename = "initial-analysis/figs/daylight_rdit.png",
       height = 10,
       width = 20)


## Jan 2025 ##
# Prepare data
daylight_data2 <- lighting_monthly |> 
  filter(ACCIDENT_YEAR %in% c("2024", "2025")) |> 
  filter(LIGHTING == "Daylight") |> 
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


daylight_model2 <- rdrobust(y = daylight_data2$CRASHES,
                           x = daylight_data2$Time,
                           covs = model.matrix(~ Season_factor, daylight_data2)[, -1],
                           c = 0,
                           p = 1,
                           h = 12,
                           kernel = "triangular")

summary(daylight_model2)


# Adjusting for seasonality before plotting
season_daylight_model2 <- lm(CRASHES ~ Season_factor,
                            data = daylight_data2)

daylight_data2$Crash_adj <- resid(season_daylight_model2) + mean(daylight_data2$CRASHES)

# plot
daylight_rd_out2 <- rdplot(y = daylight_data2$Crash_adj,
                          x = daylight_data2$Time,
                          c = 0,
                          p = 1,
                          h = 12,
                          kernel = "triangular",
                          nbins = c(12, 12))

daylight_rd_out2$rdplot +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(y = "Crash Rate",
       title = "Daylight RDiT",
       x = "Months Relative to Jan 2025") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )


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
  make_ci_band(daylight_data, "Crash_adj", function(t) t < 0, xseq_left),
  make_ci_band(daylight_data, "Crash_adj", function(t) t >= 0, xseq_right)
) |> mutate(Date = as.Date("2024-01-01") + Time * 30.4368 - nudge_days,
            Model = "Jan 2024 Cutoff")

ci_2025 <- bind_rows(
  make_ci_band(daylight_data2, "Crash_adj", function(t) t < 0, xseq_left),
  make_ci_band(daylight_data2, "Crash_adj", function(t) t >= 0, xseq_right)
) |> mutate(Date = as.Date("2025-01-01") + Time * 30.4368 + nudge_days,
            Model = "Jan 2025 Cutoff")


# combined plot, but scopes each panel to its own cutoff and window.
make_cutoff_plot <- function(ci_data, cutoff_date, model_label, line_color,
                             event_label, x_limits, x_breaks,
                             y_breaks = seq(120, 200, by = 20)) {
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
  model_label = "Daylight RDiT, Jan 2024 Cutoff",
  line_color  = "#0072B2",
  event_label = "Warning Begins",
  x_limits = c(as.Date("2023-01-01"), as.Date("2024-12-01")),
  x_breaks = seq(as.Date("2023-01-01"), as.Date("2024-12-01"), by = "2 months")
)

p_2025 <- make_cutoff_plot(
  ci_2025,
  cutoff_date = as.Date("2025-01-01"),
  model_label = "Daylight RDiT, Jan 2025 Cutoff",
  line_color  = "#D55E00",
  event_label = "Enforcement Begins",
  x_limits = c(as.Date("2024-01-01"), as.Date("2025-12-01")),
  x_breaks = seq(as.Date("2024-01-01"), as.Date("2025-12-01"), by = "2 months")
)

# Side by side 
p_2024 + p_2025



ggsave(filename = "initial-analysis/figs/daylight_models.png",
       height = 10, 
       width = 20)



# Dark ----------------------------------------------------------------------
## Jan 2024 Cutoff ##
# Prepare data
dark_data <- lighting_monthly |> 
  filter(ACCIDENT_YEAR %in% c("2023", "2024")) |> 
  filter(LIGHTING == "Dark") |> 
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


dark_model <- rdrobust(y = dark_data$CRASHES,
                           x = dark_data$Time,
                           covs = model.matrix(~ Season_factor, dark_data)[, -1],
                           c = 0,
                           p = 1,
                           h = 12,
                           kernel = "triangular")

summary(dark_model)

# Adjusting for seasonality before plotting
season_dark_model <- lm(CRASHES ~ Season_factor,
                            data = dark_data)

dark_data$Crash_adj <- resid(season_dark_model) + mean(dark_data$CRASHES)

# plot
dark_rd_out <- rdplot(y = dark_data$Crash_adj,
                          x = dark_data$Time,
                          c = 0,
                          p = 1,
                          h = 12,
                          kernel = "triangular",
                          nbins = c(12, 12))

dark_rd_out$rdplot +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(y = "Crash Rate",
       title = "Dark RDiT",
       x = "Months Relative to Jan 2024") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )

ggsave(filename = "initial-analysis/figs/dark_rdit.png",
       height = 10,
       width = 20)


## Jan 2025 Cutoff ##
# Prepare data
dark_data2 <- lighting_monthly |> 
  filter(ACCIDENT_YEAR %in% c("2024", "2025")) |> 
  filter(LIGHTING == "Dark") |> 
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


dark_model2 <- rdrobust(y = dark_data2$CRASHES,
                       x = dark_data2$Time,
                       covs = model.matrix(~ Season_factor, dark_data2)[, -1],
                       c = 0,
                       p = 1,
                       h = 12,
                       kernel = "triangular")

summary(dark_model2)

# Adjusting for seasonality before plotting
season_dark_model2 <- lm(CRASHES ~ Season_factor,
                        data = dark_data2)

dark_data2$Crash_adj <- resid(season_dark_model2) + 
  mean(dark_data2$CRASHES)

# plot
dark_rd_out2 <- rdplot(y = dark_data2$Crash_adj,
                      x = dark_data2$Time,
                      c = 0,
                      p = 1,
                      h = 12,
                      kernel = "triangular",
                      nbins = c(12, 12))

dark_rd_out2$rdplot +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(y = "Crash Rate",
       title = "Dark RDiT",
       x = "Months Relative to Jan 2025") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )


## Combined Crashes with CIs ##

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
  make_ci_band(dark_data, "Crash_adj", function(t) t < 0, xseq_left),
  make_ci_band(dark_data, "Crash_adj", function(t) t >= 0, xseq_right)
) |> mutate(Date = as.Date("2024-01-01") + Time * 30.4368 - nudge_days,
            Model = "Jan 2024 Cutoff")

ci_2025 <- bind_rows(
  make_ci_band(dark_data2, "Crash_adj", function(t) t < 0, xseq_left),
  make_ci_band(dark_data2, "Crash_adj", function(t) t >= 0, xseq_right)
) |> mutate(Date = as.Date("2025-01-01") + Time * 30.4368 + nudge_days,
            Model = "Jan 2025 Cutoff")

## Plot ##
make_cutoff_plot <- function(ci_data, cutoff_date, model_label, line_color,
                             event_label, x_limits, x_breaks,
                             y_breaks = seq(30, 150, by = 20)) {
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
  model_label = "Dark Time RDiT, Jan 2024 Cutoff",
  line_color  = "#0072B2",
  event_label = "Warning Begins",
  x_limits = c(as.Date("2023-01-01"), as.Date("2024-12-01")),
  x_breaks = seq(as.Date("2023-01-01"), as.Date("2024-12-01"), by = "2 months")
)

p_2025 <- make_cutoff_plot(
  ci_2025,
  cutoff_date = as.Date("2025-01-01"),
  model_label = "Dark Time RDiT, Jan 2025 Cutoff",
  line_color  = "#D55E00",
  event_label = "Enforcement Begins",
  x_limits = c(as.Date("2024-01-01"), as.Date("2025-12-01")),
  x_breaks = seq(as.Date("2024-01-01"), as.Date("2025-12-01"), by = "2 months")
)

# Side by side 
p_2024 + p_2025


# RD Effect by City ------------------------------------------------------

# color palette
sunflower <- c(
  "#F2C94C",
  "#1E4E8C",
  "#D4A04A",
  "#8A9B5B",
  "#F7F4E7"
)

# Extract coefficients from rdrobust
extract_rd <- function(model, lighting, cutoff){
  
  tibble(
    Lighting = lighting,
    Effect = model$coef[3,1],
    Cutoff = cutoff,
    SE = model$se[3,1],
    P_value = model$pv[3,1],
    CI_lower = model$ci[3,1],
    CI_upper = model$ci[3,2]
  )
  
}

# Build dataframes for each category
lighting_analysis <- bind_rows(extract_rd(daylight_model, "Daylight", "2024"),
                           extract_rd(daylight_model2, "Daylight", "2025"),
                           extract_rd(dark_model, "Dark time", "2024"),
                           extract_rd(dark_model2, "Dark time", "2025")) |>
  
  mutate(Lighting = factor(Lighting,
                       levels = c("Daylight", "Dark time")),
         Cutoff = factor(Cutoff,
                         levels = c("2024","2025")),
         Sig = case_when(
           P_value < 0.01 ~ "***",
           P_value < 0.05 ~ "**",
           P_value < 0.10 ~ "*",
           TRUE ~ ""
         ),
         Label = round(Effect, 2))


# Plot
ggplot(lighting_analysis,
       aes(Lighting, Effect, fill = Cutoff)) +
  
  geom_col(position = position_dodge(width = 0.6), width = 0.55) +
  
  geom_text(aes(label = Label,
                vjust = ifelse(Effect < 0,1.15,-0.35),
                color = ifelse(Sig != "" & !is.na(Sig), "#800000", "black"),
                group = Cutoff),
            position = position_dodge(width = 0.65),
            fontface = "bold",
            size = 4.5) +
  
  geom_text(aes(label = Sig,
                y = ifelse(Effect < 0, Effect - 2, Effect + 2),
                vjust = ifelse(Effect < 0, -0.9, 1.5), 
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
  
  labs(title = "RD Effect by Major Cities",
       subtitle = "Comparison of San Diego, San Francisco, & Los Angeles",
       x = NULL,
       y = "RD Effect",
       fill = NULL,
       caption = "* Significant at the 10% level; ** Significant at the 5% level; *** Significant at the 1% level") +
  
  theme_minimal(base_size = 14) +
  
  theme(legend.position="bottom",
        plot.caption = element_text(hjust = 0.5, 
                                    face = "italic", 
                                    size = 10,
                                    color = "#800000"),
        axis.text.x=element_text(face="bold", size=13),
        plot.title=element_text(face="bold", size=17),
        panel.grid.major.x=element_blank(),
        panel.grid.minor=element_blank())


ggsave(filename = "initial-analysis/figs/lighting-analysis.png",
       width = 10,
       height = 7.5)
