#### Preamble ####
# Purpose: Check the validity of the cleaned datasets 
# Author: Yongqi Liu
# Date: 21 Sep 2024 
# Contact: cassieliu.liu@mail.utoronto.ca
# License: MIT
# Pre-requisites: Obtain and have the cleaned dataset ready

#### Workspace setup ####
library(tidyverse)
library(sf)

#### Test data ####
# Test for Missing Values in the crime_data
crime_data <- read.csv("data/raw_data/simulated_crime_data.csv")
missing_values_check <- colSums(is.na(crime_data))
print(missing_values_check)

# Test for missing values
missing_values <- crime_data %>%
  select(Assault:Homicide, Population) %>% summarise_all(~ sum(is.na(.)))
print(missing_values)

sum(is.na(crime_data))

# Check the structure of the dataset to confirm data types
str(crime_data)

# Test if the crime columns are numeric and the neighborhood columns are characters
check_data_types <- sapply(crime_data, class)
print(check_data_types)

# Check that crime counts and rates are within reasonable ranges
crime_data_range_check <- crime_data %>%
  summarise(across(Assault:Homicide, list(min = min, max = max)),
            across(ends_with("Rate"), list(min = min, max = max)),
            Population_min = min(Population), Population_max = max(Population))
print(crime_data_range_check)

# Ensure that crime rates are non-negative
crime_rate_negative_check <- crime_data %>%
  summarise(across(ends_with("Rate"), ~ sum(. < 0)))
print(crime_rate_negative_check)

# Check for duplicate neighborhood IDs
duplicate_neighborhood_ids <- crime_data %>%
  group_by(HOOD_ID) %>%
  summarise(n = n()) %>%
  filter(n > 1)
print(duplicate_neighborhood_ids)

# Check for duplicate neighborhood names
duplicate_neighborhood_names <- crime_data %>%
  group_by(AREA_NAME) %>%
  summarise(n = n()) %>%
  filter(n > 1)
print(duplicate_neighborhood_names)


neighborhood_geometry<-readRDS("data/raw_data/simulated_neighborhoods_geometry.rds")

# Check that the latitudes and longitudes are within Toronto's expected ranges
geometry_bounds_check <- neighborhood_geometry %>%
  summarise(min_lat = min(st_coordinates(.)[, 2]), max_lat = max(st_coordinates(.)[, 2]),
            min_lon = min(st_coordinates(.)[, 1]), max_lon = max(st_coordinates(.)[, 1]))
print(geometry_bounds_check)

# Ensure all geometries fall inside Toronto's bounding box
latitude_check <- all(st_coordinates(neighborhood_geometry)[, 2] >= 43.6 & 
                        st_coordinates(neighborhood_geometry)[, 2] <= 43.85)
longitude_check <- all(st_coordinates(neighborhood_geometry)[, 1] >= -79.6 & 
                         st_coordinates(neighborhood_geometry)[, 1] <= -79.1)
print(paste("Latitude Check:", latitude_check))
print(paste("Longitude Check:", longitude_check))

# Check if neighborhoods with higher crime counts have lower ranks
rankings_check <- crime_data %>%
  group_by(Year) %>%
  arrange(Rank) %>%
  mutate(Check = lag(Total_Crime) >= Total_Crime) %>%
  filter(Check == FALSE)
print(rankings_check)

# Check the range of years in crime_long
range(crime_data$Year, na.rm = TRUE)

