# -------------------------------------------------------------------------
# Apply the RDiT technique for causal inference (covariate: seasonal factor)
# - Use simple linear model for preliminary analysis 
# - Use the rdrobust package for more sophisticated analysis
#
# Inputs: initial-analysis/data
# -------------------------------------------------------------------------


# Set Up ------------------------------------------------------------------

# Load packages
library(tidyverse)
library(rdrobust) # for RDiT analysis and plots
library(ggh4x)
library(nprobust)
library(patchwork)

# Read datasets
tims_crashes <-  readRDS("initial-analysis/data/TIMS_Filtered.rds")


# Controlling for Monthly Differences ---------------------------------------

## Create the data for the model controlling for Months ##
rdit_data <- tims_crashes |> 
  filter(ACCIDENT_YEAR >= 2022) |> 
  filter(PED_ACTION == "B" & INTERSECTION == "Y") |>
  mutate(MONTH = floor_date(ymd(COLLISION_DATE), "month"),
         Time = interval(as.Date("2024-01-01"), MONTH) %/% months(1),
         Post = ifelse(Time >= 0, 1, 0),
         Month_factor = factor((Time %% 12 + 12) %% 12)) |> 
  group_by(Time, Post, Month_factor) |> 
  summarise(Total_crashes = n(),
            .groups = "drop")



## Using rdrobust ##

rd_model <- rdrobust(y = rdit_data$Total_crashes,
                     x = rdit_data$Time,
                     covs = model.matrix(~ Month_factor, rdit_data)[, -1],
                     c = 0,
                     p = 1,
                     h = 24,
                     kernel = "uniform")

summary(rd_model)

# Adjusting for seasonality before plotting
season_model <- lm(Total_crashes ~ Month_factor,
                   data = rdit_data)

rdit_data$Crash_adj <- resid(season_model) + mean(rdit_data$Total_crashes)

# plot
rdplot(y = rdit_data$Crash_adj,
       x = rdit_data$Time,
       c = 0,
       p = 1,
       h = 24,
       kernel = "uniform",
       nbins = c(24, 24),
       title = "RDiT with Controlled Months",
       x.label = "Months Relative to AB 413 Implementation",
       y.label = "Number of Crashes",
       
       x.lim = c(-24, 24))


# Controlling for Seasonal Differences ------------------------------------

## Create the data for the model controlling for Seasons ##
rdit_data2 <- tims_crashes |> 
  filter(ACCIDENT_YEAR >= 2022) |> 
  filter(PED_ACTION == "B" & INTERSECTION == "Y") |>
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
rd_model2 <- rdrobust(y = rdit_data2$Total_crashes,
                      x = rdit_data2$Time,
                      covs = model.matrix(~ Season_factor, rdit_data2)[, -1],
                      c = 0,
                      p = 1,
                      h = 24,
                      kernel = "uniform")

summary(rd_model2)

# Adjusting for seasonality before plotting
season_model2 <- lm(Total_crashes ~ Season_factor,
                    data = rdit_data2)

rdit_data2$Crash_adj2 <- resid(season_model2) + mean(rdit_data2$Total_crashes)

# plot
rd_out <- rdplot(y = rdit_data2$Crash_adj2,
                 x = rdit_data2$Time,
                 c = 0,
                 p = 1,
                 h = 24,
                 kernel = "uniform",
                 nbins = c(24, 24))

rd_out$rdplot +
  labs(title = "RDiT Model",
       y = "Number of Crashes",
       x = "Months Relative to Jan 2024") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )

ggsave(filename = "initial-analysis/figs/season_rdit.png",
       height = 10,
       width = 20)



# Jan 2024 Cutoff (2-year span) -------------------------------------------
two_year_rdit <- tims_crashes |> 
  filter(ACCIDENT_YEAR %in% c("2023", "2024")) |> 
  filter(PED_ACTION == "B" & INTERSECTION == "Y") |>
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

## Run the model
two_year_model <- rdrobust(y = two_year_rdit$Total_crashes,
                      x = two_year_rdit$Time,
                      covs = model.matrix(~ Season_factor, two_year_rdit)[, -1],
                      c = 0,
                      p = 1,
                      h = 12,
                      kernel = "triangular")

summary(two_year_model)

# Adjusting for seasonality before plotting
seasonal_model <- lm(Total_crashes ~ Season_factor,
                    data = two_year_rdit)

two_year_rdit$Crash_adj <- resid(seasonal_model) + mean(two_year_rdit$Total_crashes)

# plot
rd_out <- rdplot(y = two_year_rdit$Crash_adj,
                 x = two_year_rdit$Time,
                 c = 0,
                 p = 1,
                 h = 12,
                 kernel = "triangular",
                 nbins = c(12, 12))

rd_out$rdplot +
  labs(title = "RDiT Model",
       y = "Number of Crashes",
       x = "Months Relative to Jan 2024") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )


# Robustness Checks 1 -----------------------------------------------------

## 1. Using different bandwidths ##

# Using a bandwith of 18 (36 months)
rd_model_bw18 <- rdrobust(y = rdit_data2$Total_crashes,
                      x = rdit_data2$Time,
                      covs = model.matrix(~ Season_factor, rdit_data2)[, -1],
                      c = 0,
                      p = 1,
                      h = 18,
                      kernel = "uniform")

summary(rd_model_bw18)


# Using a bandwidth of 12 (24 months)
rd_model_bw12 <- rdrobust(y = rdit_data2$Total_crashes,
                          x = rdit_data2$Time,
                          covs = model.matrix(~ Season_factor, rdit_data2)[, -1],
                          c = 0,
                          p = 1,
                          h = 12,
                          kernel = "uniform")

summary(rd_model_bw12)


## Optimal bandwidth selection ##
bw_result <- rdbwselect(y = rdit_data2$Total_crashes,     
                        x = rdit_data2$Time,
                        covs = model.matrix(~ Season_factor, rdit_data2)[, -1],
                        c = 0)                           
 

summary(bw_result)
# =======================================================
#  BW est. (h)    BW bias (b)
# Left of c Right of c  Left of c Right of c
# =======================================================
#  mserd     4.393      4.393      7.011      7.011
# =======================================================
 
# The optimal bandwidth is so narrow. how does it affect our analysis?


## 2. Using different polynomial regressions ##
rd_model_p2 <- rdrobust(y = rdit_data2$Total_crashes,
                          x = rdit_data2$Time,
                          covs = model.matrix(~ Season_factor, rdit_data2)[, -1],
                          c = 0,
                          p = 2,
                          h = 24,
                          kernel = "uniform")

summary(rd_model_p2)


## 3. Triangular weighting, No bandwidth selection ##
## 1. Linear

rd_model_p1_tri <- rdrobust(y = rdit_data2$Total_crashes,
                        x = rdit_data2$Time,
                        covs = model.matrix(~ Season_factor, rdit_data2)[, -1],
                        c = 0,
                        p = 1,
                        kernel = "triangular")

summary(rd_model_p1_tri)

# plot the model
linear_data <- rdit_data2

linear_model <- lm(Total_crashes ~ Season_factor,
                 data = linear_data)

linear_data$Crash_adj2 <- resid(linear_model) + 
  mean(linear_data$Total_crashes)


linear_rd_out <- rdplot(y = linear_data$Crash_adj2,
                      x = linear_data$Time,
                      c = 0,
                      p = 1,
                      kernel = "triangular",
                      nbins = c(24, 24))

linear_rd_out$rdplot +
  labs(title = "RDiT Model",
       y = "Number of Crashes",
       x = "Months Relative to Jan 2024") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )


## 2. Quadratic
rd_model_tri <- rdrobust(y = rdit_data2$Total_crashes,
                            x = rdit_data2$Time,
                            covs = model.matrix(~ Season_factor, rdit_data2)[, -1],
                            c = 0,
                            p = 2,
                            h = 24,
                            kernel = "triangular")

summary(rd_model_tri)

# plot the model
quad_data <- rdit_data2

quad_model <- lm(Total_crashes ~ Season_factor,
                 data = quad_data)

quad_data$Crash_adj2 <- resid(quad_model) + mean(quad_data$Total_crashes)


quad_rd_out <- rdplot(y = quad_data$Crash_adj2,
                      x = quad_data$Time,
                      c = 0,
                      p = 2,
                      h = 24,
                      kernel = "triangular",
                      nbins = c(24, 24))

quad_rd_out$rdplot +
  labs(title = "RDiT Model",
       y = "Number of Crashes",
       x = "Months Relative to Jan 2024") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )


## 4. Just Triangular Weighting

rd_model_tri <- rdrobust(y = rdit_data2$Total_crashes,
                            x = rdit_data2$Time,
                            covs = model.matrix(~ Season_factor, rdit_data2)[, -1],
                            c = 0,
                            p = 1,
                            h = 24,
                            kernel = "triangular")

summary(rd_model_tri)


# Check RDiT results
rd_model_tri$coef
rd_model_tri$se
rd_model_tri$ci


# plot the model
linear_data <- rdit_data2

linear_model <- lm(Total_crashes ~ Season_factor,
                   data = linear_data)

linear_data$Crash_adj2 <- resid(linear_model) + 
  mean(linear_data$Total_crashes)


linear_rd_out <- rdplot(y = linear_data$Crash_adj2,
                        x = linear_data$Time,
                        c = 0,
                        p = 1,
                        h = 24,
                        kernel = "triangular",
                        nbins = c(24, 24))

linear_rd_out$rdplot +
  labs(title = "RDiT Model",
       y = "Number of Crashes",
       x = "Months Relative to Jan 2024") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )



# Jan 2025 Cutoff ---------------------------------------------------------

## Using Jan 2025
rdit_data3 <- tims_crashes |> 
  filter(ACCIDENT_YEAR %in% c("2024", "2025")) |> 
  filter(PED_ACTION == "B" & INTERSECTION == "Y") |>
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

# Run model
rd_model3 <- rdrobust(y = rdit_data3$Total_crashes,
                      x = rdit_data3$Time,
                      covs = model.matrix(~ Season_factor, rdit_data3)[, -1],
                      c = 0,
                      p = 1,
                      h = 12,
                      kernel = "triangular")

summary(rd_model3)

# Adjusting for seasonality before plotting
season_model3 <- lm(Total_crashes ~ Season_factor,
                    data = rdit_data3)

rdit_data3$Crash_adj3 <- resid(season_model3) + mean(rdit_data3$Total_crashes)

# plot
rd3_out <- rdplot(y = rdit_data3$Crash_adj3,
                  x = rdit_data3$Time,
                  c = 0,
                  p = 1,
                  h = 12,
                  kernel = "triangular",
                  nbins = c(12, 12))

rd3_out$rdplot +
  labs(title = "RDiT Model",
       y = "Number of Crashes",
       x = "Months Relative to Jan 2025") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )

ggsave(filename = "initial-analysis/figs/Jan25_rdit.png",
       height = 10,
       width = 20)



# Bandwidth Sensitivity Check ---------------------------------------------

# --- Function to run rdrobust across a grid of bandwidths ---
bw_sensitivity <- function(data, y_var, x_var, covs_var, cutoff, bw_grid, kernel = "triangular") {
  
  y <- data[[y_var]]
  x <- data[[x_var]]
  
  # Expand factor covariate into dummy columns (drop intercept column)
  covs <- model.matrix(~ data[[covs_var]])[, -1, drop = FALSE]
  
  results <- lapply(bw_grid, function(h) {
    fit <- tryCatch(
      rdrobust(y = y, x = x, c = cutoff, h = h, kernel = kernel, covs = covs),
      error = function(e) NULL
    )
    
    if (is.null(fit)) {
      return(data.frame(bw = h, coef = NA, se = NA, ci_lower = NA, ci_upper = NA))
    }
    
    data.frame(
      bw       = h,
      coef     = fit$coef["Conventional", ],
      se       = fit$se["Conventional", ],
      ci_lower = fit$ci["Conventional", "CI Lower"],
      ci_upper = fit$ci["Conventional", "CI Upper"]
    )
  })
  
  bind_rows(results)
}

# --- Define bandwidth grid (in months) ---
bw_grid_2024 <- seq(4, 24, by = 2)
bw_grid_2025 <- seq(4, 12, by = 1)

# --- Run for Jan 2024 cutoff ---
sens_2024 <- bw_sensitivity(
  data      = rdit_data2,
  y_var     = "Total_crashes",       
  x_var     = "Time",
  covs_var  = "Season_factor",
  cutoff    = 0,
  bw_grid   = bw_grid_2024
)
sens_2024$cutoff_label <- "January 2024 (Warning)"

# --- Run for Jan 2025 cutoff (rdit_data3) ---
sens_2025 <- bw_sensitivity(
  data      = rdit_data3,
  y_var     = "Total_crashes",
  x_var     = "Time",
  covs_var  = "Season_factor",
  cutoff    = 0,
  bw_grid   = bw_grid_2025
)
sens_2025$cutoff_label <- "January 2025 (Enforcement)"

# --- Combine and plot ---
sens_all <- bind_rows(sens_2024, sens_2025)

ggplot(sens_all, aes(x = bw, y = coef)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "steelblue", alpha = 0.2) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_point(color = "steelblue", size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~ cutoff_label, scales = "free") +
  facetted_pos_scales(
    x = list(
      cutoff_label == "January 2024 (Warning)"    ~ scale_x_continuous(limits = c(4, 24),  breaks = seq(4, 24, by = 2)),
      cutoff_label == "January 2025 (Enforcement)" ~ scale_x_continuous(limits = c(4, 12),  breaks = seq(4, 12, by = 1))
    ),
    y = list(
      cutoff_label == "January 2024 (Passage)"    ~ scale_y_continuous(limits = c(-400, 250),  breaks = seq(-300, 250, by = 100)),
      cutoff_label == "January 2025 (Enforcement)" ~ scale_y_continuous(limits = c(-400, 250),  breaks = seq(-300, 250, by = 100))
    )
  ) +
  labs(
    x = "Bandwidth (months)",
    y = "RD Estimate",
    title = "Bandwidth Sensitivity of RDiT Estimates",
    subtitle = "Shaded band = 95% confidence interval"
  ) +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(face = "bold"))

ggsave(filename = "initial-analysis/figs/bw_sensitivity.png",
       height = 10,
       width = 20)




# Combined Model ----------------------------------------------------------

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
  make_ci_band(two_year_rdit, "Crash_adj", function(t) t < 0, xseq_left),
  make_ci_band(two_year_rdit, "Crash_adj", function(t) t >= 0, xseq_right)
) |> mutate(Date = as.Date("2024-01-01") + Time * 30.4368 - nudge_days,
            Model = "Jan 2024 Cutoff")

ci_2025 <- bind_rows(
  make_ci_band(rdit_data3, "Crash_adj3", function(t) t < 0, xseq_left),
  make_ci_band(rdit_data3, "Crash_adj3", function(t) t >= 0, xseq_right)
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


ggsave(filename = "initial-analysis/figs/rdit2425.png",
       height = 10,
       width = 20)
