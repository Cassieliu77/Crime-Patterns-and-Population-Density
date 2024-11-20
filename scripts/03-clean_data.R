#### Preamble ####
# Purpose: Clean data from wordbankr package, retrieved from worbank website
# Author: Yongqi Liu
# Date: 13 Nov 2024 
# Contact: cassieliu.liu@mail.utoronto.ca
# License: MIT
# Pre-requisites: Need have the raw dataset from wordbankr available
# Any other information needed? NA


#### Workspace setup ####
# Load necessary libraries
library(tidyverse)
library(wordbankr)
library(dplyr)
library(arrow)

#### Clean data ####
# Read the raw data parquet file
raw_data <- read_parquet("data/01-raw_data/raw_data.parquet")

# Select the needed columns
cleaned_data <- raw_data %>% select(data_id, item_id, language, form, item_kind, 
                                 category, uni_lemma, lexical_category, date_of_test,
                                 age, comprehension, production, 
                                 is_norming, child_id)

# Ensure `date_of_test` is in Date format
cleaned_data <- cleaned_data %>%
  mutate(date_of_test = as.Date(date_of_test))

# Filter data to include only rows with date_of_test after 2000-01-01
cleaned_data <- cleaned_data %>%
  filter(date_of_test > as.Date("2000-01-01"))

# Omit rows with NA values
cleaned_data <- cleaned_data %>% 
  na.omit()

# Look into first 6 rows of the finalized cleaned data
head(cleaned_data)

#### Save data ####
write_parquet(cleaned_data, sink = "data/02-analysis_data/cleaned_data.parquet")



