#### Preamble ####
# Purpose: Clean the raw data and make it available for construcing graphs
# Author: Yongqi Liu
# Date: 20 Sep 2024 
# Contact: cassieliu.liu@mail.utoronto.ca
# License: MIT
# Pre-requisites: Have the raw data ready for processing

#### Workspace setup ####
#install.packages("sf") for handling the spatial data
#install.packages("geojsonsf")
library(geojsonsf)
library(sf)
library(tidyverse)
library(dplyr)

#### Clean data ####
# Handling the spatial data, split the geometry column into longitude and latitude
crime_data <- read.csv("data/raw_data/raw_crime_data.csv")
extract_coordinates <- function(geometry_list) {
  coordinates <- unlist(strsplit(gsub("list\\(c\\(|\\)", "", geometry_list), ","))
  coordinates <- as.numeric(coordinates) 
  n <- length(coordinates) / 2
  longitudes <- coordinates[1:n]
  latitudes <- coordinates[(n+1):(2*n)]
  matrix(c(longitudes, latitudes), ncol = 2, byrow = FALSE)
}
# Apply the function to each row of the geometry column and get a list of coordinate matrices
coordinates_list <- lapply(crime_data$geometry, extract_coordinates)
crime_data_sf <- lapply(coordinates_list, function(coords) {
  if (all(coords[1, ] == coords[nrow(coords), ])) {
    st_polygon(list(coords))  
  } else {
    st_linestring(coords) 
  }
})
crime_data$geometry <- st_sfc(crime_data_sf)
crime_data_sf <- st_as_sf(crime_data)

print(st_geometry(crime_data_sf))

# Get the average crime rate in the whole Toronto for the line chart
crime_rate_columns <- grep("RATE", names(crime_data), value = TRUE)
average_rates <- crime_data %>%
  summarise(across(all_of(crime_rate_columns), mean, na.rm = TRUE))
average_rates <- cbind(AREA_NAME = "Toronto Average", average_rates)
head(average_rates)
crime_long <- average_rates %>%
  pivot_longer(cols = contains("RATE"), 
               names_to = c("Crime_Type", "Year"),  
               names_sep = "_RATE_") %>%
  mutate(Year = as.numeric(Year), 
         Crime_Type = str_to_title(Crime_Type),  
         Crime_Type = case_when( 
           Crime_Type == "Breakenter" ~ "Break and Enters",
           Crime_Type == "Autotheft" ~ "Auto Theft",
           Crime_Type == "Biketheft" ~ "Bike Theft" ,
           Crime_Type == "Theftover" ~ "Theft Over" ,
           TRUE ~ Crime_Type
         )) %>% filter(Crime_Type != "Theftfrommv") 
head(crime_long)

# Clean the dataset for the combination of maps for per type of crime
crime_long_1 <- crime_data_sf %>%
  pivot_longer(cols = c(ASSAULT_2023, ROBBERY_2023, BREAKENTER_2023, THEFTOVER_2023, AUTOTHEFT_2023,
                        BIKETHEFT_2023,SHOOTING_2023,HOMICIDE_2023), 
               names_to = "Crime_Type", 
               values_to = "Crime_Count") %>%
  mutate(Crime_Type = case_when(
    Crime_Type == "ASSAULT_2023" ~ "Assault",
    Crime_Type == "ROBBERY_2023" ~ "Robbery",
    Crime_Type == "BREAKENTER_2023" ~ "Break and Enter",
    Crime_Type == "THEFTOVER_2023" ~ "Theft Over",
    Crime_Type == "AUTOTHEFT_2023" ~ "Auto Theft",
    Crime_Type == "BIKETHEFT_2023" ~ "Bike Theft",
    Crime_Type == "HOMICIDE_2023" ~ "Homicide",
    Crime_Type == "SHOOTING_2023" ~ "Shooting",
    TRUE ~ Crime_Type 
  ))
head(crime_long_1)

#Get the crime count for different neighbourhood in different years
crime_summary_multiple_years <- crime_data %>%
  rowwise() %>%
  mutate(Total_Crime_2014 = sum(c_across(contains("_2014") & !contains("_RATE")), na.rm = TRUE),
         Total_Crime_2015 = sum(c_across(contains("_2015") & !contains("_RATE")), na.rm = TRUE),
         Total_Crime_2016 = sum(c_across(contains("_2016") & !contains("_RATE")), na.rm = TRUE),
         Total_Crime_2017 = sum(c_across(contains("_2017") & !contains("_RATE")), na.rm = TRUE),
         Total_Crime_2018 = sum(c_across(contains("_2018") & !contains("_RATE")), na.rm = TRUE),
         Total_Crime_2019 = sum(c_across(contains("_2019") & !contains("_RATE")), na.rm = TRUE),
         Total_Crime_2020 = sum(c_across(contains("_2020") & !contains("_RATE")), na.rm = TRUE),
         Total_Crime_2021 = sum(c_across(contains("_2021") & !contains("_RATE")), na.rm = TRUE),
         Total_Crime_2022 = sum(c_across(contains("_2022") & !contains("_RATE")), na.rm = TRUE),
         Total_Crime_2023 = sum(c_across(contains("_2023") & !contains("_RATE")), na.rm = TRUE)) %>%
  ungroup() %>%
  select(AREA_NAME, HOOD_ID, starts_with("Total_Crime_"))  # Select total crime columns

# View the summarized data
head(crime_summary_multiple_years)

crime_ranking <- crime_summary_multiple_years %>%
  mutate(Rank_2014 = rank(-Total_Crime_2014),  # Rank based on crime count (descending)
         Rank_2015 = rank(-Total_Crime_2015),
         Rank_2016 = rank(-Total_Crime_2016),
         Rank_2017 = rank(-Total_Crime_2017),
         Rank_2018 = rank(-Total_Crime_2018),
         Rank_2019 = rank(-Total_Crime_2019),
         Rank_2020 = rank(-Total_Crime_2020),
         Rank_2021 = rank(-Total_Crime_2021),
         Rank_2022 = rank(-Total_Crime_2022),
         Rank_2023 = rank(-Total_Crime_2023)) %>%
  select(AREA_NAME, HOOD_ID, starts_with("Rank_"))

# View the rankings
head(crime_ranking)

crime_ranking_summary <- crime_ranking %>%
  mutate(Neighborhood_ID_Name = paste(HOOD_ID, AREA_NAME, sep = " - ")) %>%
  select(Neighborhood_ID_Name, everything())  # Move the new column to the front
# View the updated data
head(crime_ranking_summary)

# Calculate the average rank for each neighborhood across the years 2014-2023
crime_ranking_summary <- crime_ranking_summary %>%
  rowwise() %>%
  mutate(Average_Rank = mean(c(Rank_2014, Rank_2015, Rank_2016, Rank_2017, 
                               Rank_2018, Rank_2019, Rank_2020, Rank_2021, 
                               Rank_2022, Rank_2023), na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(Average_Rank)
colnames(crime_ranking_summary)
# View top 10 neighborhoods with consistently highest crime
top_10_neighborhoods_consistent <- crime_ranking_summary %>% head(10)
print(top_10_neighborhoods_consistent)

# View bottom 10 neighborhoods with consistently lowest crime
bottom_10_neighborhoods_consistent <- crime_ranking_summary %>% tail(10)
print(bottom_10_neighborhoods_consistent)



#### Save data ####
saveRDS(crime_data_sf, "data/analysis_data/cleaning_crime_data.rds")
write_csv(crime_data_sf, "data/analysis_data/cleaning_crime_data.csv")
write_csv(crime_long, "data/analysis_data/toronto_crime_average_rates.csv")
write_rds(crime_long_1, "data/analysis_data/cleaning_crime_count.rds")
write_csv(top_10_neighborhoods_consistent, "data/analysis_data/top_10_neighborhoods_consistent.csv")
write_csv(bottom_10_neighborhoods_consistent, "data/analysis_data/bottom_10_neighborhoods_consistent.csv")

