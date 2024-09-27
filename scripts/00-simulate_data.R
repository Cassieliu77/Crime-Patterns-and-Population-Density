#### Preamble ####
# Purpose: Simulate the data
# Author: Yongqi Liu
# Date: 18 Sep 2024 
# Contact: cassieliu.liu@mail.utoronto.ca
# License: MIT
# Pre-requisites: Construct a sketch about the data frame roughly.

#### Workspace setup ####
library(sf) 
library(tidyverse)
library(dplyr)
set.seed(666)

# Simulate Neighborhoods
n_neighborhoods <- 158
neighborhood_ids <- 1:n_neighborhoods
neighborhood_names <- paste("Neighborhood", neighborhood_ids)

# Simulate Population Data for 2023
population_2023 <- rnorm(n_neighborhoods, mean = 20000, sd = 5000) 

# Simulate Crime Counts for 2023 (using Poisson distribution) for each neighborhood
crime_types <- c("Assault", "Robbery", "Break and Enter", "Theft Over", 
                 "Auto Theft", "Bike Theft", "Homicide", "Shooting")

# Simulate crime counts for each neighborhood for each crime type
crime_counts <- sapply(crime_types, function(crime_type) {
  rpois(n_neighborhoods, lambda = sample(50:500, 1))  # Generate 158 Poisson-distributed counts for each crime type
})

# Convert crime counts to a data frame (each crime type has 158 rows)
crime_data_sim <- data.frame(
  HOOD_ID = neighborhood_ids,
  AREA_NAME = neighborhood_names,
  POPULATION_2023 = population_2023,
  crime_counts  )

# Rename columns appropriately
colnames(crime_data_sim)[4:11] <- crime_types

# Create crime rates (per 100,000 population)
crime_rate_sim <- crime_data_sim %>%
  mutate(
    ASSAULT_RATE_2023 = Assault / POPULATION_2023 * 100000,
    ROBBERY_RATE_2023 = Robbery / POPULATION_2023 * 100000,
    BREAKENTER_RATE_2023 = `Break and Enter` / POPULATION_2023 * 100000,
    THEFTOVER_RATE_2023 = `Theft Over` / POPULATION_2023 * 100000,
    AUTOTHEFT_RATE_2023 = `Auto Theft` / POPULATION_2023 * 100000,
    BIKETHEFT_RATE_2023 = `Bike Theft` / POPULATION_2023 * 100000,
    HOMICIDE_RATE_2023 = Homicide / POPULATION_2023 * 100000,
    SHOOTING_RATE_2023 = Shooting / POPULATION_2023 * 100000)

# Simulate random spatial points for neighborhoods within a defined range
coords <- data.frame(
  longitude = runif(n_neighborhoods, min = -79.6, max = -79.2),  # Longitude range for Toronto
  latitude = runif(n_neighborhoods, min = 43.6, max = 43.8)      # Latitude range for Toronto
)

# Combine the crime data with the coordinates
crime_rate_sim <- cbind(crime_rate_sim, coords)

# Convert to an sf object (Simple Features for spatial data)
crime_data_sf_sim <- st_as_sf(crime_rate_sim, coords = c("longitude", "latitude"), crs = 4326)

print(head(crime_data_sf_sim))

#### Save Simulated Data ####
saveRDS(crime_data_sf_sim, "data/raw_data/simulated_crime_data.rds")
write_csv(crime_rate_sim, "data/raw_data/simulated_crime_data.csv")
