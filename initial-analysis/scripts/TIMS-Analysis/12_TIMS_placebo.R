
# -------------------------------------------------------------------------

## Regression Discontinuity Placebo Tests
## Inputs: initial/analysis/data-clean

# -------------------------------------------------------------------------

# Load libraries and datasets
library(tidyverse)
library(rdrobust)

tims_data <- readRDS("initial-analysis/data-clean/01_TIMS_Cleaned.rds")


# Test 1 ------------------------------------------------------------------

# Create the data for the control model

placebo_data1 <- tims_data |> 
  filter(ACCIDENT_YEAR %in% c(2014, 2015)) |> 
  filter(PED_ACTION == "B" & INTERSECTION == "Y") |>
  mutate(MONTH = floor_date(ymd(COLLISION_DATE), "month"),
         Time = interval(as.Date("2015-01-01"), MONTH) %/% months(1),
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


placebo_model1 <- rdrobust(y = placebo_data1$Total_crashes,
                           x = placebo_data1$Time,
                           covs = model.matrix(~ Season_factor, placebo_data1)[, -1],
                           c = 0,
                           p = 1,
                           h = 12,
                           kernel = "triangular")

summary(placebo_model1)

# Adjusting for seasonality before plotting
season_placebo_model1 <- lm(Total_crashes ~ Season_factor,
                            data = placebo_data1)

placebo_data1$Crash_adj <- resid(season_placebo_model1) + mean(placebo_data1$Total_crashes)

# plot
placebo_out1 <- rdplot(y = placebo_data1$Crash_adj,
                      x = placebo_data1$Time,
                      c = 0,
                      p = 1,
                      h = 12,
                      kernel = "triangular",
                      nbins = c(12, 12))

placebo_out1$rdplot +
  labs(title = "Placebo Test",
       y = "Number of Crashes",
       x = "Months Relative to Jan 2015") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )

ggsave(filename = "initial-analysis/figs/placebo_1_rdit.png",
       height = 10,
       width = 20)



# Test 2 ------------------------------------------------------------------

# Create the data for the control model

placebo_data2 <- tims_data |> 
  filter(ACCIDENT_YEAR %in% c(2015, 2016)) |> 
  filter(PED_ACTION == "B" & INTERSECTION == "Y") |>
  mutate(MONTH = floor_date(ymd(COLLISION_DATE), "month"),
         Time = interval(as.Date("2016-01-01"), MONTH) %/% months(1),
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
placebo_model2 <- rdrobust(y = placebo_data2$Total_crashes,
                           x = placebo_data2$Time,
                           covs = model.matrix(~ Season_factor, placebo_data2)[, -1],
                           c = 0,
                           p = 1,
                           h = 12,
                           kernel = "triangular")

summary(placebo_model2)

# Adjusting for seasonality before plotting
season_placebo_model2 <- lm(Total_crashes ~ Season_factor,
                            data = placebo_data2)

placebo_data2$Crash_adj <- resid(season_placebo_model2) + mean(placebo_data2$Total_crashes)

# plot
placebo_out2 <- rdplot(y = placebo_data2$Crash_adj,
                       x = placebo_data2$Time,
                       c = 0,
                       p = 1,
                       h = 12,
                       kernel = "triangular",
                       nbins = c(12, 12))

placebo_out2$rdplot +
  labs(title = "Placebo Test",
       y = "Number of Crashes",
       x = "Months Relative to Jan 2016") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )

ggsave(filename = "initial-analysis/figs/placebo_2_rdit.png",
       height = 10,
       width = 20)


# Test 3 ------------------------------------------------------------------

# Create the data for the control model

placebo_data3 <- tims_data |> 
  filter(ACCIDENT_YEAR %in% c(2016, 2017)) |> 
  filter(PED_ACTION == "B" & INTERSECTION == "Y") |>
  mutate(MONTH = floor_date(ymd(COLLISION_DATE), "month"),
         Time = interval(as.Date("2017-01-01"), MONTH) %/% months(1),
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


placebo_model3 <- rdrobust(y = placebo_data3$Total_crashes,
                           x = placebo_data3$Time,
                           covs = model.matrix(~ Season_factor, placebo_data3)[, -1],
                           c = 0,
                           p = 1,
                           h = 12,
                           kernel = "triangular")

summary(placebo_model3)

# Adjusting for seasonality before plotting
season_placebo_model3 <- lm(Total_crashes ~ Season_factor,
                            data = placebo_data3)

placebo_data3$Crash_adj <- resid(season_placebo_model3) + mean(placebo_data3$Total_crashes)

# plot
placebo_out3 <- rdplot(y = placebo_data3$Crash_adj,
                       x = placebo_data3$Time,
                       c = 0,
                       p = 1,
                       h = 12,
                       kernel = "triangular",
                       nbins = c(12, 12))

placebo_out3$rdplot +
  labs(title = "Placebo Test",
       y = "Number of Crashes",
       x = "Months Relative to Jan 2017") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )

ggsave(filename = "initial-analysis/figs/placebo_3_rdit.png",
       height = 10,
       width = 20)



# Test 4 ------------------------------------------------------------------

# Create the data for the control model

placebo_data4 <- tims_data |> 
  filter(ACCIDENT_YEAR %in% c(2017, 2018)) |> 
  filter(PED_ACTION == "B" & INTERSECTION == "Y") |>
  mutate(MONTH = floor_date(ymd(COLLISION_DATE), "month"),
         Time = interval(as.Date("2018-01-01"), MONTH) %/% months(1),
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


placebo_model4 <- rdrobust(y = placebo_data4$Total_crashes,
                           x = placebo_data4$Time,
                           covs = model.matrix(~ Season_factor, placebo_data4)[, -1],
                           c = 0,
                           p = 1,
                           h = 12,
                           kernel = "triangular")

summary(placebo_model4)

# Adjusting for seasonality before plotting
season_placebo_model4 <- lm(Total_crashes ~ Season_factor,
                            data = placebo_data4)

placebo_data4$Crash_adj <- resid(season_placebo_model4) + mean(placebo_data4$Total_crashes)

# plot
placebo_out4 <- rdplot(y = placebo_data4$Crash_adj,
                       x = placebo_data4$Time,
                       c = 0,
                       p = 1,
                       h = 12,
                       kernel = "triangular",
                       nbins = c(12, 12))

placebo_out4$rdplot +
  labs(title = "Placebo Test",
       y = "Number of Crashes",
       x = "Months Relative to Jan 2018") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )

ggsave(filename = "initial-analysis/figs/placebo_4_rdit.png",
       height = 10,
       width = 20)


# Test 5 ------------------------------------------------------------------

# Create the data for the control model

placebo_data5 <- tims_data |> 
  filter(ACCIDENT_YEAR %in% c(2018, 2019)) |> 
  filter(PED_ACTION == "B" & INTERSECTION == "Y") |>
  mutate(MONTH = floor_date(ymd(COLLISION_DATE), "month"),
         Time = interval(as.Date("2019-01-01"), MONTH) %/% months(1),
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


placebo_model5 <- rdrobust(y = placebo_data5$Total_crashes,
                           x = placebo_data5$Time,
                           covs = model.matrix(~ Season_factor, placebo_data5)[, -1],
                           c = 0,
                           p = 1,
                           h = 12,
                           kernel = "triangular")

summary(placebo_model5)

# Adjusting for seasonality before plotting
season_placebo_model5 <- lm(Total_crashes ~ Season_factor,
                            data = placebo_data5)

placebo_data5$Crash_adj <- resid(season_placebo_model5) + mean(placebo_data5$Total_crashes)

# plot
placebo_out5 <- rdplot(y = placebo_data5$Crash_adj,
                       x = placebo_data5$Time,
                       c = 0,
                       p = 1,
                       h = 12,
                       kernel = "triangular",
                       nbins = c(12, 12))

placebo_out5$rdplot +
  labs(title = "Placebo Test",
       y = "Number of Crashes",
       x = "Months Relative to Jan 2019") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )

ggsave(filename = "initial-analysis/figs/placebo_5_rdit.png",
       height = 10,
       width = 20)


# Test 6 ------------------------------------------------------------------

# Create the data for the control model

placebo_data6 <- tims_data |> 
  filter(ACCIDENT_YEAR %in% c(2021, 2022)) |> 
  filter(PED_ACTION == "B" & INTERSECTION == "Y") |>
  mutate(MONTH = floor_date(ymd(COLLISION_DATE), "month"),
         Time = interval(as.Date("2022-01-01"), MONTH) %/% months(1),
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


placebo_model6 <- rdrobust(y = placebo_data6$Total_crashes,
                           x = placebo_data6$Time,
                           covs = model.matrix(~ Season_factor, placebo_data6)[, -1],
                           c = 0,
                           p = 1,
                           h = 12,
                           kernel = "triangular")

summary(placebo_model6)

# Adjusting for seasonality before plotting
season_placebo_model6 <- lm(Total_crashes ~ Season_factor,
                            data = placebo_data6)

placebo_data6$Crash_adj <- resid(season_placebo_model6) + mean(placebo_data6$Total_crashes)

# plot
placebo_out6 <- rdplot(y = placebo_data6$Crash_adj,
                       x = placebo_data6$Time,
                       c = 0,
                       p = 1,
                       h = 12,
                       kernel = "triangular",
                       nbins = c(12, 12))

placebo_out6$rdplot +
  labs(title = "Placebo Test",
       y = "Number of Crashes",
       x = "Months Relative to Jan 2022") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )

ggsave(filename = "initial-analysis/figs/placebo_6_rdit.png",
       height = 10,
       width = 20)


# Test 7 ------------------------------------------------------------------

# Create the data for the control model

placebo_data7 <- tims_data |> 
  filter(ACCIDENT_YEAR %in% c(2022, 2023)) |> 
  filter(PED_ACTION == "B" & INTERSECTION == "Y") |>
  mutate(MONTH = floor_date(ymd(COLLISION_DATE), "month"),
         Time = interval(as.Date("2023-01-01"), MONTH) %/% months(1),
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


placebo_model7 <- rdrobust(y = placebo_data7$Total_crashes,
                           x = placebo_data7$Time,
                           covs = model.matrix(~ Season_factor, placebo_data7)[, -1],
                           c = 0,
                           p = 1,
                           h = 12,
                           kernel = "triangular")

summary(placebo_model7)

# Adjusting for seasonality before plotting
season_placebo_model7 <- lm(Total_crashes ~ Season_factor,
                            data = placebo_data7)

placebo_data7$Crash_adj <- resid(season_placebo_model7) + mean(placebo_data7$Total_crashes)

# plot
placebo_out7 <- rdplot(y = placebo_data7$Crash_adj,
                       x = placebo_data7$Time,
                       c = 0,
                       p = 1,
                       h = 12,
                       kernel = "triangular",
                       nbins = c(12, 12))

placebo_out7$rdplot +
  labs(title = "Placebo Test",
       y = "Number of Crashes",
       x = "Months Relative to Jan 2023") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )

ggsave(filename = "initial-analysis/figs/placebo_7_rdit.png",
       height = 10,
       width = 20)



# Combined Plot -----------------------------------------------------------

# Placebo results
placebo_results <- tibble(
  cutoff = c("Jan 2015", "Jan 2016", "Jan 2017", "Jan 2018",
             "Jan 2019", "Jan 2022", "Jan 2023"),
  estimate = c(-76.197, -103.704, -68.854, -20.075, -58.669, -66.568, -49.719),
  ci_low   = c(-124.301, -192.909, -203.998, -90.508, -146.671, -92.476, -79.862),
  ci_high  = c(4.235, -25.260, 39.906, 76.878, 29.847, -2.311, 8.596),
  p_value  = c(0.067, 0.011, 0.187, 0.873, 0.195, 0.039, 0.114)
) |>
  mutate(
    cutoff = factor(cutoff, levels = rev(cutoff)),
    significant = if_else(p_value < 0.05, "Significant at the 5% level", "Not significant")
  )

# Plot
placebo_plot <- ggplot(placebo_results, aes(x = estimate, y = cutoff)) +
  geom_vline(xintercept = 0, color = "grey60", linewidth = 0.4) +
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high, color = significant),
                 height = 0.15, linewidth = 0.8) +
  geom_point(aes(color = significant), size = 3) +
  scale_color_manual(values = c("Significant at the 5% level" = "#C16200",
                                "Not significant" = "#888780")) +

  labs(title = "Placebo Test RD Effects",
       x = "RD Effect Estimate (change in crashes)",
       y = NULL,
       color = NULL) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "grey40"),
    axis.title.x = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

placebo_plot

ggsave(filename = "initial-analysis/figs/placebo_forest_plot.png",
       plot = placebo_plot,
       height = 6,
       width = 10,
       dpi = 300)

