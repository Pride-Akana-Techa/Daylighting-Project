
# -------------------------------------------------------------------------

# Modified RDiT for Specific Cities


# -------------------------------------------------------------------------


# Load packages and data
library(tidyverse)

tims_crashes <-  readRDS("initial-analysis/data/TIMS_Filtered.rds") 

san_diego <- tims_crashes |> 
  filter(COUNTY == "SAN DIEGO")|> 
  filter(ACCIDENT_YEAR >= 2022) |> 
  filter(PED_ACTION == "B" & INTERSECTION == "Y") 

san_diego |> 
  mutate( CITY = case_when(CITY == "SAN DIEGO" ~ "San Diego",
                           TRUE ~ "Other")) |> 
  group_by(CITY, ACCIDENT_YEAR) |> 
  summarise(crashes = n()) |> 
  ggplot(aes(x = ACCIDENT_YEAR, y = crashes, color = CITY)) +
  geom_line() +
  theme_minimal(base_size = 13) +
  labs(x = "Year",
       y = "Number of Crashes",
         color = "City")

san_diego |> 
  mutate( CITY = case_when(CITY == "SAN DIEGO" ~ "San Diego",
                           TRUE ~ "Other")) |> 
  filter(ACCIDENT_YEAR == "2025") |> 
mutate(Modified_time = parse_date_time(COLLISION_DATE, 
                                       orders = c("mdy HM", "mdy HMS"))) |> 
  mutate(MONTH = month(Modified_time, label = TRUE, abbr = FALSE)
  group_by(CITY, ACCIDENT_YEAR) |>
  
