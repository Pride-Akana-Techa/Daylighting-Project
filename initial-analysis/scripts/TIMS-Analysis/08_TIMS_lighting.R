
# -------------------------------------------------------------------------
## Investigating the relationship between lighting condition and crashes ##

# -------------------------------------------------------------------------

# load libraries and dataset
library(tidyverse)
library(rdrobust)

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
  filter(ACCIDENT_YEAR >= "2022",
         PED_ACTION == "B",
         INTERSECTION == "Y") |> 
  filter(!is.na(LIGHTING)) |>
  mutate(MONTH_DATE = floor_date(COLLISION_DATE, "month")) |>   
  group_by(MONTH_DATE, LIGHTING) |> 
  summarise(CRASHES = n(), .groups = "drop") |> 
  group_by(MONTH_DATE) |> 
  mutate(PROPORTION = CRASHES / sum(CRASHES)) |> 
  ungroup()



# Daylight -----------------------------------------------------------

## RDiT Model for clear daylight crashes ##
# Prepare data
daylight_data <- lighting_monthly |> 
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


daylight_model <- rdrobust(y = daylight_data$PROPORTION,
                        x = daylight_data$Time,
                        covs = model.matrix(~ Season_factor, daylight_data)[, -1],
                        c = 0,
                        p = 1,
                        h = 24,
                        kernel = "uniform")

summary(daylight_model)

# Adjusting for seasonality before plotting
season_daylight_model <- lm(PROPORTION ~ Season_factor,
                         data = daylight_data)

daylight_data$Crash_adj <- resid(season_daylight_model) + mean(daylight_data$PROPORTION)

# plot
daylight_rd_out <- rdplot(y = daylight_data$Crash_adj,
                 x = daylight_data$Time,
                 c = 0,
                 p = 1,
                 h = 24,
                 kernel = "uniform",
                 nbins = c(24, 24))

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



# Dark ----------------------------------------------------------------------
## RDiT Model for clear weather crashes ##
# Prepare data
dark_data <- lighting_monthly |> 
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


dark_model <- rdrobust(y = dark_data$PROPORTION,
                           x = dark_data$Time,
                           covs = model.matrix(~ Season_factor, dark_data)[, -1],
                           c = 0,
                           p = 1,
                           h = 24,
                           kernel = "uniform")

summary(dark_model)

# Adjusting for seasonality before plotting
season_dark_model <- lm(PROPORTION ~ Season_factor,
                            data = dark_data)

dark_data$Crash_adj <- resid(season_dark_model) + mean(dark_data$PROPORTION)

# plot
dark_rd_out <- rdplot(y = dark_data$Crash_adj,
                          x = dark_data$Time,
                          c = 0,
                          p = 1,
                          h = 24,
                          kernel = "uniform",
                          nbins = c(24, 24))

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


# Dusk/Dawn ---------------------------------------------------------------

## RDiT Model for clear weather crashes ##
# Prepare data
dusk_data <- lighting_monthly |> 
  filter(LIGHTING == "Dusk/Dawn") |> 
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


dusk_model <- rdrobust(y = dusk_data$PROPORTION,
                           x = dusk_data$Time,
                           covs = model.matrix(~ Season_factor, dusk_data)[, -1],
                           c = 0,
                           p = 1,
                           h = 24,
                           kernel = "uniform",
                           bwselect = "mserd")

summary(dusk_model)

# Adjusting for seasonality before plotting
season_dusk_model <- lm(PROPORTION ~ Season_factor,
                            data = dusk_data)

dusk_data$Crash_adj <- resid(season_dusk_model) + mean(dusk_data$PROPORTION)

# plot
dusk_rd_out <- rdplot(y = dusk_data$Crash_adj,
                          x = dusk_data$Time,
                          c = 0,
                          p = 1,
                          h = 24,
                          kernel = "uniform",
                          nbins = c(24, 24))

dusk_rd_out$rdplot +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(y = "Crash Rate",
       title = "Dusk/Dawn RDiT",
       x = "Months Relative to Jan 2024") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )

ggsave(filename = "initial-analysis/figs/dusk_rdit.png",
       height = 10,
       width = 20)




