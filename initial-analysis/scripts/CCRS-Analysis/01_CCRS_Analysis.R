## -------------------------------------------------------------------------
## Exploratory analysis using the CCRS dataset to examine variation in
## crashes involving pedestrians at intersections by periods of the day and
## weather conditions.

## Inputs: 
## Outputs:


# load packages
library(tidyverse)

# Importing and Cleaning ----------------------------------------------------

# Merge the datasets
yearly_crashes <- read_csv("initial-analysis/scripts/CCRS-statewide-crashes.csv")

problems(yearly_crashes)

# converts the crash time to standard date-time object
updated_crashes <- yearly_crashes |> 
  mutate(Modified_time = parse_date_time(`Crash Date Time`, 
                                         orders = c("mdy HM", "mdy HMS"))) |> 

  # selects the month and year in the standard time and create a Month and year variables  
  mutate(Month = month(Modified_time, label = TRUE, abbr = FALSE),
         Year = year(Modified_time)) |> 
 
  filter_out(Year == 2026) |> 
  
  # Remove columns with at least 70% null values 
  select(-c(IsAOIOneSameAsLocation, IsLocationReferToNarrative, HasDigitalMediaFiles, IsAdditonalObjectStruck, IsCountyRoad, ReportingDistrictCode, PedestrianActionDesc, NotificationDate, MotorVehicleInvolvedWithDesc, MotorVehicleInvolvedWithOtherDesc, MilepostDistance, MilepostMarker, MilepostUnitOfMeasure, `Weather 2`, `Road Condition 2`, MilepostDirection, ReportingDistrict, EvidenceNumber, NotificationTimeDescription, IsAttachmentsMailed, `Collision Type Other Desc`, HitRun, `Special Condition`, SketchDesc, IsFreeway, Beat, DispatchNotified, IsDeleted, IsTowAway, `Special Condition`, `Primary Collision Factor Code`, PrimaryCollisionFactorIsCited, PrimaryCollisionPartyNumber, SecondaryUnitOfMeasure, IsAdditonalObjectStruck, HasDigitalMediaFiles, HasPhotographs)) |> 
  
  write_csv(file.path("initial-analysis/scripts/updated-crashes.csv"))

# Check for null values in each variable
colSums(is.na(updated_crashes)) 


# Time Series: Pedestrian and Bicycle Crashes -----------------------------

# Grouping for pedestrian and bicyclist crashes
updated_crashes |>  
  group_by(Year) |> 
  filter(MotorVehicleInvolvedWithCode %in% c("B", "G")) |>
  filter_out(Year == "2026") |> 
  summarize(Crashes_with_Pedestrians = sum(MotorVehicleInvolvedWithCode == "B"),             Crashes_with_Bicyclists = sum(MotorVehicleInvolvedWithCode == "G"),
            .groups = "drop") |> 

  pivot_longer(
    cols = c(Crashes_with_Pedestrians, Crashes_with_Bicyclists),
    names_to = "Crash Type",
    values_to = "Number of Crashes"
  ) |> 
  ggplot(aes(x = Year, y = `Number of Crashes`, color = `Crash Type`, group = `Crash Type`)) + 
  geom_line() +
  scale_x_continuous(breaks = seq(2016, 2025, by = 1))
  labs(title = "Pedestrian and Bicycle Crashes by Year",
      x = "Year", 
      y = "Number of Crashes", 
      color = "Crash Type")

  # Saving the plot
  ggsave("figs/yearly-ped-and-bike-crashes.png",
         width = 10, height = 6)

# Pedestrian Crashes at Intersections
  updated_crashes |>
    filter_out(Year == "2026") |> 
    group_by(Year) |> 
    filter(PedestrianActionCode == "B") |> 
    summarise(Pedestrian_Intersection_Crashes = n(),
              .groups = "drop") |> 
    
    ggplot(aes(x = Year, y = Pedestrian_Intersection_Crashes, group = 1)) +
    geom_line() +
    scale_x_continuous(breaks = seq(2016, 2025, by = 1)) +
    labs(title = "Yearly Crashes at Intersections Involving Pedestrians",
         x = "Year",
         y = "Number of Crashes")
  
  # Saving the plot
  ggsave("figs/yearly-ped-intersection-crashes.png", width = 10, height = 6)

  # Analyzing Injuries of Pedestrians at Intersections
  updated_crashes |> 
  filter_out(Year == "2026") |> 
    filter(PedestrianActionCode == "B") |> 
    group_by(Year) |> 
    summarise(Number_Injured = sum(as.numeric(NumberInjured), na.rm = TRUE),
              Number_Killed = sum(as.numeric(NumberKilled), na.rm = TRUE),
              .groups = "drop") |> 
  
    ggplot(aes(x = Year, y = Number_Injured)) +
    geom_line() +
    scale_x_continuous(breaks = seq(2016, 2025, by = 1)) +
    labs(title = "Pedestrians Injured at Intersections",
         x = "Year",
         y = "Number of Injured Pedestrians")
  
  ggsave("figs/yearly-ped-injuries.png",
         width = 10,
         height = 6)
  
  ggsave("figs/yearly-ped-int-injuries.png",
         width = 10,
         height = 6)


# Analysis by Lighting ---------------------------------------------

  
# filtering for pedestrian crashes by time of the day and plotting (2016-2025)
# 1. Yearly
    
  updated_crashes |> 
    filter(PedestrianActionCode == "B") |> 
    mutate(LightingDescription = case_when(
      LightingDescription == "DAYLIGHT" ~ "Daylight",                     
      LightingDescription %in% c("DARK-STREET LIGHTS NOT FUNCTIONING",
                                 "DARK-STREET LIGHTS",
                                 "DARK-NO STREET LIGHTS") ~ "Dark",
      LightingDescription =="DUSK-DAWN" ~"Dusk-Dawn"
    )) |> 
    filter_out(is.na(LightingDescription))|> 
    group_by(Year, LightingDescription) |> 
    summarize(Pedestrian_int_crashes = n(),
              .groups = "drop") |> 
    ggplot(aes(x = Pedestrian_int_crashes, y = LightingDescription, fill = LightingDescription)) +
    geom_col() +
    theme(axis.text.y = element_text(angle = 45, vjust = 1, hjust=1)) +
    facet_wrap(~Year) +
    labs(title = "Pedestrian Crashes by Periods of the Day",
         x = "Number of Crashes",
         y = "Lighting Description")
  
# 2.Monthly 
  updated_crashes <- read_csv("initial-analysis/scripts/updated-crashes.csv")
  
  updated_crashes |> 
    filter(PedestrianActionCode == "B") |> 
    mutate(Month = factor(Month,
                           levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
                           )
           ) |> 
    mutate(LightingDescription = case_when(
      LightingDescription == "DAYLIGHT" ~ "Daylight",                     
      LightingDescription %in% c("DARK-STREET LIGHTS NOT FUNCTIONING",
                                 "DARK-STREET LIGHTS",
                                 "DARK-NO STREET LIGHTS") ~ "Dark",
      LightingDescription =="DUSK-DAWN" ~"Dusk-Dawn"
    )) |> 
    filter_out(is.na(LightingDescription))|> 
    group_by(Month, LightingDescription) |> 
    summarize(Pedestrian_int_crashes = n(),
              .groups = "drop") |> 
    ggplot(aes(x = Pedestrian_int_crashes, y = LightingDescription, fill = LightingDescription)) +
    geom_col() +
    theme(axis.text.y = element_text(angle = 45, vjust = 1, hjust=1)) +
    facet_wrap(~Month) +
    labs(title = "Pedestrian Crashes by Periods of the Day",
         x = "Number of Crashes",
         y = "Lighting Description")
  
  ggsave("initial-analysis/figs/monthly-ped-crashes-daytime.png",
         width = 10,
         height = 6)
  
  

# Analysis by Weather -----------------------------------------------------
  # Yearly
  updated_crashes <- read_csv("initial-analysis/scripts/updated-crashes.csv")
  updated_crashes |> 
    filter(PedestrianActionCode == "B") |>
    filter_out(is.na(`Weather 1`)) |> 
    group_by(Year, `Weather 1`) |> 
    summarize(Ped_int_crashes = n(),
               .groups = "drop") |> 
    ggplot(aes(x = Year, y = Ped_int_crashes, fill = `Weather 1`)) +
    geom_col() +
    scale_x_continuous(breaks = seq(2016, 2025, by = 1))
    
  # Monthly
  updated_crashes |> 
    filter(PedestrianActionCode == "B") |> 
    filter(Year == 2025) |> 
    filter_out(is.na(`Weather 1`)) |> 
    mutate(Weather = case_when(
      `Weather 1` == "CLEAR" ~ "Clear",
      `Weather 1` == "CLOUDY" ~ "Cloudy",
      `Weather 1` == "RAINING" ~ "Raining",
      `Weather 1`%in% c("WIND", "UNKNOWN", "SNOWING", "SMOKY", "SMOKEY", 
                        "OTHER", "FOG/VISIBILITY") ~ "Other"
        )
      ) |> 
    group_by(Month, Weather) |> 
    summarize(Ped_crashes = n(),
              .groups = "drop") |> 
    ggplot(aes(x = Ped_crashes, y = Weather, fill = Weather)) +
    geom_col()
  class(updated_crashes$Year)
  
  