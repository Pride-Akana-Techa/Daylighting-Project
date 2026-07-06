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
library(rddensity) 
library(lmtest)
library(car)

# Read datasets
tims_crashes <-  readRDS("initial-analysis/data/TIMS_Filtered.rds")



# Seasonal Effects by Months ------------------------------------------------

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


## 1. Linear Model ##

# Run the model
c <- 0  # setting the cutoff
rdit_model <- lm(Total_crashes ~ Post + I(Time - c) + Post * I(Time - c) +                    Month_factor,
                 data = rdit_data)

summary(rdit_model)

# Sensitivity Check

tibble(Model = c("Narrow (±9mo)", "Full (±24mo)", "Wide (±18mo)"),
       Estimate = c(coef(lm(Total_crashes ~ Post + I(Time - c) + Post * I(Time - c) + Month_factor,
                            data = filter(rdit_data, Time >= -9 & Time <= 9)))["Post:I(Time - c)"],
                    coef(rdit_model)["Post:I(Time - c)"],
                    coef(lm(Total_crashes ~ Post + I(Time - c) + Post * I(Time - c) + Month_factor,
                            data = filter(rdit_data, Time >= -18 & Time <= 18)))["Post:I(Time - c)"]),
       P_value = c(summary(lm(Total_crashes ~ Post + I(Time - c) + Post * I(Time - c) + Month_factor,
                              data = filter(rdit_data, Time >= -9 & Time <= 9)))$coefficients["Post:I(Time - c)", 4],
                   summary(rdit_model)$coefficients["Post:I(Time - c)", 4],
                   summary(lm(Total_crashes ~ Post + I(Time - c) + Post * I(Time - c) + Month_factor,
                              data = filter(rdit_data, Time >= -18 & Time <= 18)))$coefficients["Post:I(Time - c)", 4]))


### Seasonally Adjusted Plot ###

# Fit a model with only month to capture seasonal effects
seasonal_model <- lm(Total_crashes ~ Month_factor, data = rdit_data)
summary(seasonal_model)

# Remove seasonal component, add mean to residual
rdit_data$crashes_sa <- residuals(seasonal_model) + mean(rdit_data$Total_crashes)

# Plot seasonally adjusted data
ggplot(rdit_data, aes(Time, crashes_sa)) +
  geom_point(size = 2) +
  
  geom_smooth(data = subset(rdit_data, Post == 0),
              method = "lm",
              se = TRUE) +
  
  geom_smooth(data = subset(rdit_data, Post == 1),
              method = "lm",
              se = TRUE) +
  
  geom_vline(xintercept = -0.5,
             linetype = "dashed") +
  
  theme_minimal(base_size = 13) +
  scale_x_continuous(breaks = seq(-24, 23, by = 1)) +
  
  labs(title = "Regression Discontinuity in Time: AB 413 (Seasonally Adjusted)",
       x = "Months Relative to January 2024",
       y = "Monthly Pedestrian Crashes (Seasonally Adjusted)")

# Save plot
ggsave(filename = "initial-analysis/figs/rdit_sa.png",
       height = 10,
       width = 20)



## 2. Using rdrobust ##

rd_model <- rdrobust(y = rdit_data$Total_crashes,
                     x = rdit_data$Time,
                     covs = model.matrix(~ Month_factor, rdit_data)[, -1],
                     c = 0,
                     p = 1,
                     h = 24,
                     kernel = "uniform",
                     bwselect = "mserd")

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





# Quadratic Model ---------------------------------------------------------

c <- 0

rdit_model_quad <- lm(Total_crashes ~ Post + 
                        I(Time - c) + I((Time - c)^2) +          
                        Post * I(Time - c) + Post * I((Time - c)^2) + 
                        Month_factor,
                      data = rdit_data)
summary(rdit_model_quad)

# Sensitivity Check
tibble(Model = c("Narrow (±9mo)", "Full (±24mo)", "Wide (±18mo)"),
       Estimate = c(
         coef(lm(Total_crashes ~ Post + I(Time - c) + I((Time - c)^2) +
                   Post * I(Time - c) + Post * I((Time - c)^2) + Month_factor,
                 data = filter(rdit_data, Time >= -9 & Time <= 9)))["Post:I(Time - c)"],
         coef(rdit_model_quad)["Post:I(Time - c)"],
         coef(lm(Total_crashes ~ Post + I(Time - c) + I((Time - c)^2) +
                   Post * I(Time - c) + Post * I((Time - c)^2) + Month_factor,
                 data = filter(rdit_data, Time >= -18 & Time <= 18)))["Post:I(Time - c)"]
       ),
       P_value = c(
         summary(lm(Total_crashes ~ Post + I(Time - c) + I((Time - c)^2) +
                      Post * I(Time - c) + Post * I((Time - c)^2) + Month_factor,
                    data = filter(rdit_data, Time >= -9 & Time <= 9)))$coefficients["Post:I(Time - c)", 4],
         summary(rdit_model_quad)$coefficients["Post:I(Time - c)", 4],
         summary(lm(Total_crashes ~ Post + I(Time - c) + I((Time - c)^2) +
                      Post * I(Time - c) + Post * I((Time - c)^2) + Month_factor,
                    data = filter(rdit_data, Time >= -18 & Time <= 18)))$coefficients["Post:I(Time - c)", 4]
       ))

AIC(rdit_model, rdit_model_quad)
anova(rdit_model, rdit_model_quad)

# Formal functional form test
# RESET test - tests whether higher order terms improve fit
resettest(rdit_model, power = 2:3, type = "regressor")

# Check for multicolinearity in quadratic model
vif(rdit_model_quad)



# Seasonal Effects by Seasons ----------------------------------------------
## A.Create the data for the model controlling for Seasons ##
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


### 1. Linear Model ###
# Run the model
c <- 0  # setting the cutoff
rdit_model2 <- lm(Total_crashes ~ Post + I(Time - c) + Post * I(Time - c) +                    Season_factor,
                  data = rdit_data2)

summary(rdit_model2)

# Fit a model with capture seasonal effects
seasonal_model2 <- lm(Total_crashes ~ Season_factor, data = rdit_data2)
summary(seasonal_model2)

# Remove seasonal component, add mean to residual
rdit_data2$crashes_sa2 <- residuals(seasonal_model2) + mean(rdit_data$Total_crashes)


# Plot Seasonally adjusted model
ggplot(rdit_data2,
       aes(Time, crashes_sa2)) +
  geom_point(size = 2) +
  geom_smooth(data = subset(rdit_data2, Time < 0),
              method = "lm",
              se = TRUE) +
  geom_smooth(data = subset(rdit_data2, Time >= 0),
              method = "lm",
              se = TRUE) +
  geom_vline(xintercept = -0.5,
             linetype = "dashed") +
  scale_x_continuous(breaks = seq(-24, 23, by = 1)) +
  
  labs(title = "Seasonally Adjusted RDiT",
       y = "Pedestrian Crashes",
       x = "Months Relative to Jan 2024") +
  theme_minimal(base_size = 13)

# Save plot
ggsave(filename = "initial-analysis/figs/season_rdit.png",
       height = 10,
       width = 20)


### 2. Using rdrobust ###
rd_model2 <- rdrobust(y = rdit_data2$Total_crashes,
                      x = rdit_data2$Time,
                      covs = model.matrix(~ Season_factor, rdit_data2)[, -1],
                      c = 0,
                      p = 1,
                      h = 24,
                      kernel = "uniform",
                      bwselect = "mserd")

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



# Jan 2025 Cutoff ---------------------------------------------------------

## Using Jan 2025
rdit_data3 <- tims_crashes |> 
  filter(ACCIDENT_YEAR >= 2024) |> 
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
                      kernel = "uniform",
                      bwselect = "mserd")

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
                  kernel = "uniform",
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



