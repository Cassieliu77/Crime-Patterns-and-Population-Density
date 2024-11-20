#### Preamble ####
# Purpose: Test the structure and validity of the simulated wordbank dataset.
# Author: Yongqi Liu
# Date: 11 Nov 2024 
# Contact: cassieliu.liu@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
  # - The `tidyverse` and `testthat` package must be installed and loaded
  # - 00-simulate_data.R must have been run
# Any other information needed? Make sure you are in the `starter_folder` rproj


#### Workspace setup ####
# Load necessary packages
library(tidyverse)
library(testthat)

# Load the simulated data
simulated_data <- read_csv("data/00-simulated_data/simulated_data.csv")

# Test if the data was successfully loaded
test_that("dataset was successfully loaded", {
  expect_true(exists("simulated_data"))
})


#### Test data ####

# Check if the dataset has the correct number of rows
test_that("dataset has 10000 rows", {
  expect_equal(nrow(simulated_data), 10000)
})

# Check if the dataset has the correct number of columns
test_that("dataset has 14 columns", {
  expect_equal(ncol(simulated_data), 14)
})

# Check if the 'language' column contains only valid languages
valid_languages <- c(
  "English (American)", "Spanish", "French", "German", "Chinese", "Japanese",
  "Korean", "Arabic", "Russian", "Hindi", "Portuguese", "Italian", 
  "Swedish", "Dutch", "Turkish", "Bengali", "Vietnamese", "Thai"
)
test_that("'language' column contains only valid languages", {
  expect_true(all(simulated_data$language %in% valid_languages))
})

# Check if the 'category' column contains only valid categories
valid_categories <- c(
  "sounds", "animals", "vehicles", "toys", "food_drink", "clothing", 
  "body_parts", "furniture_rooms", "household", "outside", "people", 
  "games_routines", "action_words", "time_words", "descriptive_words", 
  "pronouns", "question_words", "locations", "quantifiers", "places", 
  "helping_verbs", "connecting_words"
)
test_that("'category' column contains only valid categories", {
  expect_true(all(simulated_data$category %in% valid_categories))
})

# Check if there are no missing values in the dataset
test_that("dataset contains no missing values", {
  expect_true(all(!is.na(simulated_data)))
})

# Check if 'age' falls within a valid range
test_that("'age' values are between 18 and 60", {
  expect_true(all(simulated_data$age >= 18 & simulated_data$age <= 60))
})

# Check if 'comprehension' falls within a valid range
test_that("'comprehension' values are between 100 and 800", {
  expect_true(all(simulated_data$comprehension >= 100 & simulated_data$comprehension <= 800))
})

# Check if 'production' falls within a valid range
test_that("'production' values are between 100 and 700", {
  expect_true(all(simulated_data$production >= 100 & simulated_data$production <= 700))
})

# Check if 'date_of_test' is within a valid range
test_that("'date_of_test' is within the year 2010", {
  expect_true(all(simulated_data$date_of_test >= as.Date("2010-01-01") &
                    simulated_data$date_of_test <= as.Date("2010-12-31")))
})

### Test Finished ###
# If all tests pass, print the success message
cat("Tests all passed\n")