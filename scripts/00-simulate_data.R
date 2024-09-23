#### Preamble ####
# Purpose: Simulate the data
# Author: Yongqi Liu
# Date: 18 Sep 2024 
# Contact: cassieliu.liu@mail.utoronto.ca
# License: MIT
# Pre-requisites: Construct the sketch about the data frame roughly
# Any other information needed? NA

#### Workspace setup ####
library(sf) 
library(tidyverse)
library(dplyr)
set.seed(666)

#### Simulate data ####
n_neighborhoods <- 158
years <- 2014:2023
crime_types <- c("Assault", "Robbery", "Break and Enter", "Theft Over", 
                 "Auto Theft", "Bike Theft", "Shooting", "Homicide")
neighborhood_data <- data.frame(
  HOOD_ID = 1:n_neighborhoods,
  AREA_NAME = paste("Neighborhood", 1:n_neighborhoods)
)
head(neighborhood_data)

# Simulate crime counts for each crime type, each year and neighborhood
simulate_crime_counts <- function() {
  sapply(crime_types, function(x) rpois(1, lambda = sample(50:200, 1)))  # Random counts between 50 and 200
}
crime_data <- expand.grid(
  HOOD_ID = neighborhood_data$HOOD_ID,
  Year = years
) %>%
  rowwise() %>%
  mutate(
    Assault = rpois(1, lambda = 100),
    Robbery = rpois(1, lambda = 50),
    `Break and Enter` = rpois(1, lambda = 70),
    `Theft Over` = rpois(1, lambda = 30),
    `Auto Theft` = rpois(1, lambda = 40),
    `Bike Theft` = rpois(1, lambda = 20),
    Shooting = rpois(1, lambda = 10),
    Homicide = rpois(1, lambda = 5)
  ) %>%
  ungroup()
crime_data <- crime_data %>%
  left_join(neighborhood_data, by = "HOOD_ID")
head(crime_data)

# Simulate crime rates (per 100,000 population)
crime_data <- crime_data %>%
  mutate(Population = sample(5000:15000, n(), replace = TRUE)) %>%
  mutate(across(Assault:Homicide, ~ . / Population * 100000, .names = "{.col}_Rate"))
head(crime_data)

# Create a function to simulate rankings based on crime counts
simulate_rankings <- function(data) {
  data %>%
    group_by(Year) %>%
    mutate(Total_Crime = rowSums(across(Assault:Homicide)),
           Rank = rank(-Total_Crime)) %>%
    ungroup()}
crime_data_ranked <- simulate_rankings(crime_data)
head(crime_data_ranked)

# Simulate geometry data (latitude and longitude) for neighborhoods
simulate_geometry <- function(n) {
  latitudes <- runif(n, min = 43.6, max = 43.85)  # Toronto's latitude range
  longitudes <- runif(n, min = -79.6, max = -79.1)  # Toronto's longitude range
  st_as_sf(data.frame(
    lat = latitudes, 
    lon = longitudes
  ), coords = c("lon", "lat"), crs = 4326)
}
neighborhood_geometries <- simulate_geometry(n_neighborhoods)

# Add geometry to the neighborhood data
crime_data_with_geometry <- crime_data %>%
  left_join(st_drop_geometry(neighborhood_geometries) %>% mutate(HOOD_ID = neighborhood_data$HOOD_ID), by = "HOOD_ID")

#### Save Simulated Data ####
write.csv(crime_data_ranked, "data/raw_data/simulated_crime_data.csv", row.names = FALSE)  
saveRDS(neighborhood_geometries, "data/raw_data/simulated_neighborhoods_geometry.rds") 
