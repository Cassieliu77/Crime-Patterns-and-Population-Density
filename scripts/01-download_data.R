#### Preamble ####
# Purpose: Download the needed dataset and save it.
# Author: Yongqi Liu
# Date: 20 Sep 2024 
# Contact: cassieliu.liu@mail.utoronto.ca
# License: MIT
# Pre-requisites: Find the dataset called "About Neighborhood Crime Rates" on Open Data Toronto
# Any other information needed? NA


#### Workspace setup ####
library(opendatatoronto)
library(tidyverse)
library(dplyr)

#### Download data ####
# get the package
package <- show_package("neighbourhood-crime-rates")
package
# get all resources for this package
resources <- list_package_resources("neighbourhood-crime-rates")
# identify datastore resources; by default, Toronto Open Data sets datastore resource format to CSV for non-geospatial and GeoJSON for geospatial resources
datastore_resources <- filter(resources, tolower(format) %in% c('csv', 'geojson'))
# load the first datastore resource as a sample
data <- filter(datastore_resources, row_number()==1) %>% get_resource()
data

#### Save data ####
write_csv(data, "data/raw_data/raw_crime_data.csv") 

         
