#### Preamble ####
# Purpose: Check the validity of the cleaned datasets 
# Author: Yongqi Liu
# Date: 21 Sep 2024 
# Contact: cassieliu.liu@mail.utoronto.ca
# License: MIT
# Pre-requisites: Obtain and have the cleaned dataset ready

#### Workspace setup ####
library(tidyverse)

#### Test data ####
# Test for Missing Values in the crime_data
crime_data <- read.csv("data/raw_data/simulated_crime_data.csv")
missing_values_check <- colSums(is.na(crime_data))
print(missing_values_check)

# Check for missing values in critical columns (e.g., crime counts, population)
missing_values_critical <- crime_data_ranked %>%
  select(Assault:Homicide, Population) %>%
  summarise_all(~ sum(is.na(.)))
print("Missing Values in Critical Columns:")
print(missing_values_critical)
# Check the structure of the dataset to confirm data types
str(crime_data_ranked)

# Specifically, check if the crime columns are numeric and the neighborhood columns are characters
check_data_types <- sapply(crime_data_ranked, class)
print("Data Types Check:")
print(check_data_types)

# Check that crime counts and rates are within reasonable ranges
crime_data_range_check <- crime_data_ranked %>%
  summarise(across(Assault:Homicide, list(min = min, max = max)),
            across(ends_with("Rate"), list(min = min, max = max)),
            Population_min = min(Population), Population_max = max(Population))

print("Crime Counts and Rates Range Check:")
print(crime_data_range_check)

# Ensure that crime rates are non-negative
crime_rate_negative_check <- crime_data_ranked %>%
  summarise(across(ends_with("Rate"), ~ sum(. < 0)))
print("Negative Crime Rates Check:")
print(crime_rate_negative_check)
# Check for duplicate neighborhood IDs
duplicate_neighborhood_ids <- crime_data_ranked %>%
  group_by(HOOD_ID) %>%
  summarise(n = n()) %>%
  filter(n > 1)

print("Duplicate Neighborhood IDs Check:")
print(duplicate_neighborhood_ids)

# Check for duplicate neighborhood names
duplicate_neighborhood_names <- crime_data_ranked %>%
  group_by(AREA_NAME) %>%
  summarise(n = n()) %>%
  filter(n > 1)

print("Duplicate Neighborhood Names Check:")
print(duplicate_neighborhood_names)

# Check that the latitudes and longitudes are within Toronto's expected ranges
geometry_bounds_check <- neighborhood_geometries %>%
  summarise(min_lat = min(st_coordinates(.)[, 2]), max_lat = max(st_coordinates(.)[, 2]),
            min_lon = min(st_coordinates(.)[, 1]), max_lon = max(st_coordinates(.)[, 1]))

print("Geometry Bounds Check (Latitude/Longitude within Toronto's range):")
print(geometry_bounds_check)

# Ensure no geometries fall outside Toronto's bounding box
latitude_check <- all(st_coordinates(neighborhood_geometries)[, 2] >= 43.6 & 
                        st_coordinates(neighborhood_geometries)[, 2] <= 43.85)
longitude_check <- all(st_coordinates(neighborhood_geometries)[, 1] >= -79.6 & 
                         st_coordinates(neighborhood_geometries)[, 1] <= -79.1)

print(paste("Latitude Check:", latitude_check))
print(paste("Longitude Check:", longitude_check))

# Check if neighborhoods with higher crime counts have lower ranks
rankings_check <- crime_data_ranked %>%
  group_by(Year) %>%
  arrange(Rank) %>%
  mutate(Check = lag(Total_Crime) >= Total_Crime) %>%
  filter(Check == FALSE)

print("Ranking Logic Check (higher crime should have lower rank):")
print(rankings_check)

# Check the range of years in crime_long
print("Range of years in crime_long:")
range(crime_long$Year, na.rm = TRUE)