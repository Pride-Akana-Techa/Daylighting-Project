
# -------------------------------------------------------------------------

## Examining the implications of the law in specific cities at their
## enforcement dates


# -------------------------------------------------------------------------


# Load packages and data
library(tidyverse)
library(rdrobust)

tims_crashes <- readRDS("initial-analysis/data-clean/01_TIMS_Cleaned.rds") 


# San Diego --------------------------------------------------------------

## Jan 2024 cutoff ##
san_diego <- tims_crashes |> 
  filter(CITY == "SAN DIEGO" & ACCIDENT_YEAR %in% c(2023, 2024)) |> 
  filter(PED_ACTION == "B" & INTERSECTION == "Y") |> 
  mutate(MONTH = floor_date(ymd(COLLISION_DATE), "month")) |>
  mutate(Time = interval(as.Date("2024-01-01"), MONTH) %/% months(1),
         Post = ifelse(Time >= 0, 1, 0), 
         Season_factor = factor(
           case_when(month(MONTH) %in% c(12, 1, 2) ~ "Winter",
                     month(MONTH) %in% c(3, 4, 5) ~ "Spring",
                     month(MONTH) %in% c(6, 7, 8) ~ "Summer",
                     month(MONTH) %in% c(9, 10, 11) ~ "Fall"),
           levels = c("Winter", "Spring", "Summer", "Fall"))
  ) |> 
  group_by(CITY, Time, Post, Season_factor) |> 
  summarise(Total_crashes = n(), .groups = "drop")


# Run the model
san_diego_model <- rdrobust(y = san_diego$Total_crashes,
                            x = san_diego$Time,
                            covs = model.matrix(~ Season_factor, san_diego)[, -1],
                            c = 0, 
                            p = 1,
                            h = 10,
                            kernel = "triangular")

summary(san_diego_model)

# Ajust for seasonality
san_diego_season <- lm(Total_crashes ~ Season_factor,
                        data = san_diego)

san_diego$Crash_adj <- resid(san_diego_season) + mean(san_diego$Total_crashes)


## March 2025 Cutoff ##
san_diego1 <- tims_crashes |> 
  filter(CITY == "SAN DIEGO" & COLLISION_DATE >= "2024-05-01") |> 
  filter(PED_ACTION == "B" & INTERSECTION == "Y") |> 
  mutate(MONTH = floor_date(ymd(COLLISION_DATE), "month")) |>
  mutate(Time = interval(as.Date("2025-03-01"), MONTH) %/% months(1),
         Post = ifelse(Time >= 0, 1, 0), 
         Season_factor = factor(
           case_when(month(MONTH) %in% c(12, 1, 2) ~ "Winter",
           month(MONTH) %in% c(3, 4, 5) ~ "Spring",
           month(MONTH) %in% c(6, 7, 8) ~ "Summer",
           month(MONTH) %in% c(9, 10, 11) ~ "Fall"),
           levels = c("Winter", "Spring", "Summer", "Fall"))
  ) |> 
  group_by(CITY, Time, Post, Season_factor) |> 
  summarise(Total_crashes = n(), .groups = "drop")


# Run the model
san_diego1_model <- rdrobust(y = san_diego1$Total_crashes,
                       x = san_diego1$Time,
                       covs = model.matrix(~ Season_factor, san_diego1)[, -1],
                       c = 0, 
                       p = 1,
                       h = 10,
                       kernel = "triangular")

summary(san_diego1_model)


# Ajust for seasonality
san_diego1_season <- lm(Total_crashes ~ Season_factor,
                    data = san_diego1)

san_diego1$Crash_adj <- resid(san_diego1_season) + mean(san_diego1$Total_crashes)


# Combined plots
## Plot ##

h <- 12

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
  make_ci_band(san_diego, "Crash_adj", function(t) t < 0, xseq_left),
  make_ci_band(san_diego, "Crash_adj", function(t) t >= 0, xseq_right)
) |> mutate(Date = as.Date("2024-01-01") + Time * 30.4368,
            Model = "Jan 2024 Cutoff")

ci_2025 <- bind_rows(
  make_ci_band(san_diego1, "Crash_adj", function(t) t < 0, xseq_left),
  make_ci_band(san_diego1, "Crash_adj", function(t) t >= 0, xseq_right)
) |> mutate(Date = as.Date("2025-01-01") + Time * 30.4368,
            Model = "Jan 2025 Cutoff")


# combined plot, but scopes each panel to its own cutoff and window.
make_cutoff_plot <- function(ci_data, cutoff_date, model_label, line_color,
                             event_label, x_limits, x_breaks,
                             y_breaks = seq(10, 50, by = 5)) {
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
  model_label = "LA RDiT, Jan 2024 Cutoff",
  line_color  = "#0072B2",
  event_label = "Warning Begins",
  x_limits = c(as.Date("2023-01-01"), as.Date("2024-12-01")),
  x_breaks = seq(as.Date("2023-01-01"), as.Date("2024-12-01"), by = "2 months")
)

p_2025 <- make_cutoff_plot(
  ci_2025,
  cutoff_date = as.Date("2025-01-01"),
  model_label = "LA RDiT, Jan 2025 Cutoff",
  line_color  = "#D55E00",
  event_label = "Enforcement Begins",
  x_limits = c(as.Date("2024-01-01"), as.Date("2025-12-01")),
  x_breaks = seq(as.Date("2024-01-01"), as.Date("2025-12-01"), by = "2 months")
)

# Side by side 
p_2024 + p_2025


# San Francisco ----------------------------------------------------------

## 2024 Cutoff ##
san_francisco <- tims_crashes |> 
  filter(CITY == "SAN FRANCISCO" & ACCIDENT_YEAR %in% c(2023, 2024)) |> 
  filter(PED_ACTION == "B" & INTERSECTION == "Y") |> 
  mutate(MONTH = floor_date(ymd(COLLISION_DATE), "month")) |>
  mutate(Time = interval(as.Date("2024-01-01"), MONTH) %/% months(1),
         Post = ifelse(Time >= 0, 1, 0), 
         Season_factor = factor(
           case_when(month(MONTH) %in% c(12, 1, 2) ~ "Winter",
                     month(MONTH) %in% c(3, 4, 5) ~ "Spring",
                     month(MONTH) %in% c(6, 7, 8) ~ "Summer",
                     month(MONTH) %in% c(9, 10, 11) ~ "Fall"),
           levels = c("Winter", "Spring", "Summer", "Fall"))
  ) |> 
  group_by(CITY, Time, Post, Season_factor) |> 
  summarise(Total_crashes = n(), .groups = "drop")

# Run the model
san_francisco_model <- rdrobust(y = san_francisco$Total_crashes,
                                x = san_francisco$Time,
                                covs = model.matrix(~ Season_factor, 
                                                    san_francisco)[, -1],
                                c = 0, 
                                p = 1,
                                h = 12,
                                kernel = "triangular")

summary(san_francisco_model)


# Plot the model
san_francisco_season <- lm(Total_crashes ~ Season_factor,
                           data = san_francisco)

san_francisco$Crash_adj <- resid(san_francisco_season) + mean(san_francisco$Total_crashes)


## 2025 Cutoff ##
san_francisco1 <- tims_crashes |> 
  filter(CITY == "SAN FRANCISCO" & ACCIDENT_YEAR %in% c(2024, 2025)) |> 
  filter(PED_ACTION == "B" & INTERSECTION == "Y") |> 
  mutate(MONTH = floor_date(ymd(COLLISION_DATE), "month")) |>
  mutate(Time = interval(as.Date("2025-01-01"), MONTH) %/% months(1),
         Post = ifelse(Time >= 0, 1, 0), 
         Season_factor = factor(
           case_when(month(MONTH) %in% c(12, 1, 2) ~ "Winter",
                     month(MONTH) %in% c(3, 4, 5) ~ "Spring",
                     month(MONTH) %in% c(6, 7, 8) ~ "Summer",
                     month(MONTH) %in% c(9, 10, 11) ~ "Fall"),
           levels = c("Winter", "Spring", "Summer", "Fall"))
  ) |> 
  group_by(CITY, Time, Post, Season_factor) |> 
  summarise(Total_crashes = n(), .groups = "drop")

# Run the model
san_francisco1_model <- rdrobust(y = san_francisco1$Total_crashes,
                            x = san_francisco1$Time,
                            covs = model.matrix(~ Season_factor, 
                                                san_francisco1)[, -1],
                            c = 0, 
                            p = 1,
                            h = 12,
                            kernel = "triangular")

summary(san_francisco1_model)


# Adjust for seasonality before plotting
san_francisco1_season <- lm(Total_crashes ~ Season_factor,
                       data = san_francisco1)

san_francisco1$Crash_adj <- resid(san_francisco1_season) + mean(san_francisco1$Total_crashes)


## Plot ##

h <- 12

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
  make_ci_band(san_francisco, "Crash_adj", function(t) t < 0, xseq_left),
  make_ci_band(san_francisco, "Crash_adj", function(t) t >= 0, xseq_right)
) |> mutate(Date = as.Date("2024-01-01") + Time * 30.4368,
            Model = "Jan 2024 Cutoff")

ci_2025 <- bind_rows(
  make_ci_band(san_francisco1, "Crash_adj", function(t) t < 0, xseq_left),
  make_ci_band(san_francisco1, "Crash_adj", function(t) t >= 0, xseq_right)
) |> mutate(Date = as.Date("2025-01-01") + Time * 30.4368,
            Model = "Jan 2025 Cutoff")


# combined plot, but scopes each panel to its own cutoff and window.
make_cutoff_plot <- function(ci_data, cutoff_date, model_label, line_color,
                             event_label, x_limits, x_breaks,
                             y_breaks = seq(10, 50, by = 5)) {
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
  model_label = "RDiT Model, Jan 2024 Cutoff",
  line_color  = "#0072B2",
  event_label = "Warning Begins",
  x_limits = c(as.Date("2023-01-01"), as.Date("2024-12-01")),
  x_breaks = seq(as.Date("2023-01-01"), as.Date("2024-12-01"), by = "2 months")
)

p_2025 <- make_cutoff_plot(
  ci_2025,
  cutoff_date = as.Date("2025-01-01"),
  model_label = "RDiT Model, Jan 2025 Cutoff",
  line_color  = "#D55E00",
  event_label = "Enforcement Begins",
  x_limits = c(as.Date("2024-01-01"), as.Date("2025-12-01")),
  x_breaks = seq(as.Date("2024-01-01"), as.Date("2025-12-01"), by = "2 months")
)

# Side by side 
p_2024 + p_2025



# LA -----------------------------------------------------------
## Jan 2024 ##

la <- tims_crashes |> 
  filter(CITY == "LOS ANGELES" & ACCIDENT_YEAR %in% c(2023, 2024)) |> 
  filter(PED_ACTION == "B" & INTERSECTION == "Y") |> 
  mutate(MONTH = floor_date(ymd(COLLISION_DATE), "month")) |>
  mutate(Time = interval(as.Date("2024-01-01"), MONTH) %/% months(1),
         Post = ifelse(Time >= 0, 1, 0), 
         Season_factor = factor(
           case_when(month(MONTH) %in% c(12, 1, 2) ~ "Winter",
                     month(MONTH) %in% c(3, 4, 5) ~ "Spring",
                     month(MONTH) %in% c(6, 7, 8) ~ "Summer",
                     month(MONTH) %in% c(9, 10, 11) ~ "Fall"),
           levels = c("Winter", "Spring", "Summer", "Fall"))
  ) |> 
  group_by(CITY, Time, Post, Season_factor) |> 
  summarise(Total_crashes = n(), .groups = "drop")

# Run the model
la_model <- rdrobust(y = la$Total_crashes,
                     x = la$Time,
                     covs = model.matrix(~ Season_factor, la)[, -1],
                     c = 0, 
                     p = 1,
                     h = 12,
                     kernel = "triangular")

summary(la_model)


# Adjust for seasonality before plotting
la_season <- lm(Total_crashes ~ Season_factor,
                data = la)

la$Crash_adj <- resid(la_season) + mean(la$Total_crashes)


## Jan 2025 ##

la1 <- tims_crashes |> 
  filter(CITY == "LOS ANGELES" & ACCIDENT_YEAR %in% c(2024, 2025)) |> 
  filter(PED_ACTION == "B" & INTERSECTION == "Y") |> 
  mutate(MONTH = floor_date(ymd(COLLISION_DATE), "month")) |>
  mutate(Time = interval(as.Date("2025-01-01"), MONTH) %/% months(1),
         Post = ifelse(Time >= 0, 1, 0), 
         Season_factor = factor(
           case_when(month(MONTH) %in% c(12, 1, 2) ~ "Winter",
                     month(MONTH) %in% c(3, 4, 5) ~ "Spring",
                     month(MONTH) %in% c(6, 7, 8) ~ "Summer",
                     month(MONTH) %in% c(9, 10, 11) ~ "Fall"),
           levels = c("Winter", "Spring", "Summer", "Fall"))
  ) |> 
  group_by(CITY, Time, Post, Season_factor) |> 
  summarise(Total_crashes = n(), .groups = "drop")

# Run the model
la1_model <- rdrobust(y = la1$Total_crashes,
                     x = la1$Time,
                     covs = model.matrix(~ Season_factor, la1)[, -1],
                     c = 0, 
                     p = 1,
                     h = 12,
                     kernel = "triangular")

summary(la1_model)


# Adjust for seasonality before plotting
la1_season <- lm(Total_crashes ~ Season_factor,
                data = la1)

la1$Crash_adj <- resid(la1_season) + mean(la1$Total_crashes)


## Plot ##

h <- 12

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
  make_ci_band(la, "Crash_adj", function(t) t < 0, xseq_left),
  make_ci_band(la, "Crash_adj", function(t) t >= 0, xseq_right)
) |> mutate(Date = as.Date("2024-01-01") + Time * 30.4368,
            Model = "Jan 2024 Cutoff")

ci_2025 <- bind_rows(
  make_ci_band(la1, "Crash_adj", function(t) t < 0, xseq_left),
  make_ci_band(la1, "Crash_adj", function(t) t >= 0, xseq_right)
) |> mutate(Date = as.Date("2025-01-01") + Time * 30.4368,
            Model = "Jan 2025 Cutoff")


# combined plot, but scopes each panel to its own cutoff and window.
make_cutoff_plot <- function(ci_data, cutoff_date, model_label, line_color,
                             event_label, x_limits, x_breaks,
                             y_breaks = seq(10, 50, by = 5)) {
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
  model_label = "LA RDiT, Jan 2024 Cutoff",
  line_color  = "#0072B2",
  event_label = "Warning Begins",
  x_limits = c(as.Date("2023-01-01"), as.Date("2024-12-01")),
  x_breaks = seq(as.Date("2023-01-01"), as.Date("2024-12-01"), by = "2 months")
)

p_2025 <- make_cutoff_plot(
  ci_2025,
  cutoff_date = as.Date("2025-01-01"),
  model_label = "LA RDiT, Jan 2025 Cutoff",
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
extract_rd <- function(model, city, cutoff){
  
  tibble(
    City = city,
    Effect = model$coef[3,1],
    Cutoff = cutoff,
    SE = model$se[3,1],
    P_value = model$pv[3,1],
    CI_lower = model$ci[3,1],
    CI_upper = model$ci[3,2]
  )
  
}

# Build dataframes for each category
city_analysis <- bind_rows(extract_rd(san_diego_model, "San Diego", "2024"),
                           extract_rd(san_diego1_model, "San Diego", "2025"),
                           extract_rd(san_francisco_model, "San Francisco", "2024"),
                           extract_rd(san_francisco1_model, "San Francisco", "2025"),
                           extract_rd(la_model, "Los Angeles", "2024"),
                           extract_rd(la1_model, "Los Angeles", "2025")) |>
  
  mutate(City = factor(City,
                       levels = c("San Diego", "San Francisco", "Los Angeles")),
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
ggplot(city_analysis,
       aes(City, Effect, fill = Cutoff)) +
  
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


ggsave(filename = "initial-analysis/figs/city-analysis.png",
       width = 10,
       height = 7.5)
