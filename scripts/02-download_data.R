#### Preamble ####
# Purpose: Downloads and saves the data from wordbankr package, retrieved from wordbank website
# Author: Yongqi Liu
# Date: 13 Nov 2024 
# Contact: cassieliu.liu@mail.utoronto.ca
# License: MIT
# Pre-requisites: Need the website URL: https://wordbank.stanford.edu
# Any other information needed? NA


#### Workspace setup ####
#install.packages("devtools")
library(usethis)
library(devtools)
# Install wordbankr from GitHub
#devtools::install_github("langcog/wordbankr", force = TRUE)
library(tidyverse)
library(wordbankr)
library(arrow)

#See what instruments (languages and forms) are available in the packages
instrument <- get_instruments()

#### Download data ####
#The get_administration_data() function gives by-administration information,
#either for a specific language and/or form or for all instruments.

items <- get_item_data(language = "English (American)", form = "WS")

if (!is.null(items)) {
  raw_data <- get_instrument_data(language = "English (American)",
                                   form = "WS",
                                   items = items$item_id,
                                   administration_info = TRUE,
                                   item_info = TRUE)
}


#### Save data ####
# Because the dataset is too large (1.2GB in csv file). Alternatively, save the downloaded data as a Parquet file.
write_parquet(x = raw_data, sink = "data/01-raw_data/raw_data.parquet")

