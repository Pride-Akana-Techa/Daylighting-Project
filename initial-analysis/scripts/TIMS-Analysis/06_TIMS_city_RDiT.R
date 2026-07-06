
# -------------------------------------------------------------------------

# Modified RDiT for Specific Cities


# -------------------------------------------------------------------------


# Load packages and data
library(tidyverse)

tims_crashes <-  readRDS("initial-analysis/data/TIMS_Filtered.rds") 

# city-specific cutoff dates
city_cutoffs <- tribble(
  ~CITY,            ~cutoff_date,
  "SAN DIEGO",      as.Date("2025-03-01"),
  "SAN LEANDRO",    as.Date("2025-03-01"),
  "SAN FRANCISCO",  as.Date("2025-01-01"),
  "BERKELEY",       as.Date("2025-04-01")
)

specific_cities <- tims_crashes |> 
  filter(CITY %in% city_cutoffs$CITY) |> 
  filter(PED_ACTION == "B" & INTERSECTION == "Y") |> 
  mutate(MONTH = floor_date(ymd(COLLISION_DATE), "month")) |> 
  left_join(city_cutoffs, by = "CITY") |> 
  mutate(Time = interval(cutoff_date, MONTH) %/% months(1)) |> 
  filter(Time >= -12, Time <= 12) |> 
  mutate(Post = ifelse(Time >= 0, 1, 0),
         Calendar_month = factor(month(MONTH))  # actual Jan=1 Feb=2 etc, not relative
  ) |> 
  group_by(CITY, Time, Post, Calendar_month) |> 
  summarise(Total_crashes = n(), .groups = "drop")

# Run the model
c <- 0
rdit_model <- lm(Total_crashes ~ Post + I(Time - c) + Post * I(Time - c) +                    Calendar_month + CITY,
                 data = specific_cities)
summary(rdit_model)


# Visualize the model
seasonal_model <- lm(Total_crashes ~ Calendar_month, data = specific_cities)
summary(seasonal_model)

# Remove seasonal component, add mean to residual
specific_cities$crashes_sa <- residuals(seasonal_model) + mean(specific_cities$Total_crashes)

# Plot
ggplot(specific_cities,
       aes(Time, Total_crashes)) +
  geom_point(size = 2) +
  
  geom_smooth(data = subset(specific_cities, Post == 0),
              method = "lm",
              se = TRUE) +
  
  geom_smooth(data = subset(specific_cities, Post == 1),
              method = "lm",
              se = TRUE) +
  
  geom_vline(xintercept = -0.5,
             linetype = "dashed") +
  
  theme_minimal(base_size = 13) +
  scale_x_continuous(breaks = seq(-12, 12, by = 1)) +
  
  facet_wrap( ~CITY) +
  
  labs(title = "Regression Discontinuity in Time: AB 413",
       x = "Months Relative to cutoff",
       y = "Monthly Pedestrian Crashes") 



# Trying a Poisson model --------------------------------------------------


# Check for dispersion
mean(specific_cities$Total_crashes)
var(specific_cities$Total_crashes)

# poisson model
rdit_model_poisson <- glm(
  Total_crashes ~ Post + I(Time - c) + Post:I(Time - c) + Calendar_month + CITY,
  family = poisson(link = "log"),
  data = specific_cities
)
summary(rdit_model_poisson)


# allow Post effect to vary by city
rdit_city_specific <- glm(
  Total_crashes ~ Post * CITY + I(Time - c) + Post:I(Time - c) + Calendar_month,
  family = poisson(link = "log"),
  data = specific_cities
)
summary(rdit_city_specific)

# test whether city-specific effects are statistically justified
anova(rdit_model_poisson, rdit_city_specific, test = "Chisq")

# pre-trend test
pre_trend <- glm(
  Total_crashes ~ I(Time - c) * CITY + Calendar_month,
  family = poisson(link = "log"),
  data = specific_cities |> filter(Post == 0)
)
summary(pre_trend)


# Visualizations
ggplot(specific_cities, aes(x = Time, y = Total_crashes)) +
  geom_point(alpha = 0.7) +
  geom_smooth(data = . %>% filter(Post == 0), 
              method = "glm", method.args = list(family = "poisson"),
              se = TRUE, color = "steelblue") +
  geom_smooth(data = . %>% filter(Post == 1), 
              method = "glm", method.args = list(family = "poisson"),
              se = TRUE, color = "tomato") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  #facet_wrap(~ CITY, scales = "free_y") +
  labs(x = "Months relative to cutoff", 
       y = "Monthly crashes",
       title = "RDiT: Pedestrian crashes at intersections",
       subtitle = "Blue = pre-cutoff trend, Red = post-cutoff trend") +
  theme_minimal()





