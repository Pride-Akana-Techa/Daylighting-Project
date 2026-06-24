# Load packages
library(tidyverse)

# Merge datasets

# 1. Get a list of all your file paths
# Assuming they are all in one folder ending in .csv
files <- list.files(path = "initial-analysis/data/weather", 
                    pattern = "*.csv", 
                    full.names = TRUE)

# 2. Read all files and bind them into one dataframe
# .id = "source" creates a column showing which file the data came from
weather_data <- files |> 
  map_df(~read_csv(.x), .id = "source")
