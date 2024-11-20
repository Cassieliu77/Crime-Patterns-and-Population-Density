#### Preamble ####
# Purpose: Simulates a dataset for the wordbank data.
# Author: Yongqi Liu
# Date: 9 Nov 2024 
# Contact: cassieliu.liu@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - The `tidyverse` package must be installed and loaded
# - Need write a sketch about how the real dataset looks like
# Any other information needed? NA


#### Workspace setup ####
# Load necessary libraries
library(tidyverse)

# Set seed for reproducibility
set.seed(6666)  

#### Simulate data ####
# Simulating data_id as a unique identifier
data_id <- 1:10000

# Simulating item_id (e.g., "item_1", "item_2", etc.)
item_id <- paste("item", sample(1:20, 10000, replace = TRUE), sep = "_")

# Simulating language (e.g., "English (American)")
language <- sample(
  c(
    "English (American)", "Spanish", "French", "German", "Chinese", "Japanese",
    "Korean", "Arabic", "Russian", "Hindi", "Portuguese", "Italian",
    "Swedish", "Dutch", "Turkish", "Bengali", "Vietnamese", "Thai"
  ),
  size = 10000,
  replace = TRUE)

# Simulating form (e.g., "WS")
form <- sample(
  c("Words and Sentences", "Short Form of Words and Sentences", 
    "Words and Gestures", "Short Form of Words and Gestures"),
  size = 10000,
  replace = TRUE)

# Simulating item_kind (e.g., "word")
item_kind <- sample(
  c("first_signs", "phrases", "starting_to_talk", "word", "gestures_first",
    "gestures_games", "gestures_objects", "gestures_parent", "gestures_adult",
    "how_use_words", "word_endings", "word_forms_nouns", "word_forms_verbs",
    "word_endings_nouns", "word_endings_verbs", "combine", "complexity"),
  size = 10000,
  replace = TRUE)

# Simulating category (e.g., "sounds")
# Define unique categories
categories <- c(
  "sounds", "animals", "vehicles", "toys", "food_drink", "clothing", "body_parts", 
  "furniture_rooms", "household", "outside", "people", "games_routines", "action_words", 
  "time_words", "descriptive_words", "pronouns", "question_words", "locations", 
  "quantifiers", "places", "helping_verbs", "connecting_words")

# Simulate category (randomly assign categories to each row)
category <- sample(categories, size = 10000, replace = TRUE)


# Define a subset of uni_lemma values
uni_lemma_values <- c(
  "crocodile", "bee", "cat", "dog", "elephant", "fish (animal)", "frog",
  "giraffe", "hat", "pants", "shoe", "blanket", "bottle", "knife", 
  "spoon", "tissue", "toothbrush", "mommy", "daddy", "baby", 
  "apple", "banana", "bread", "cheese", "chocolate", "juice", 
  "pizza", "ice cream", "sandwich", "carrot"
)

# Simulate uni_lemma values
uni_lemma <- sample(uni_lemma_values, size = 10000, replace = TRUE)


# Simulating lexical_category (e.g., "other")
lexical_category <- sample(
  c("other", "noun", "verb", "adjective", "other"),
  size = 10000,
  replace = TRUE)


# Simulating date_of_test (random dates within a range)
date_of_test <- sample(
  seq(as.Date("2010-01-01"), as.Date("2010-12-31"), by = "day"),
  size = 10000,
  replace = TRUE)

# Simulating age (random ages between 18 and 60)
age <- sample(18:60, size = 10000, replace = TRUE)

# Simulating comprehension (random values between 100 and 800)
comprehension <- sample(100:800, size = 10000, replace = TRUE)

# Simulating production (random values between 100 and 700)
production <- sample(100:700, size = 10000, replace = TRUE)

# Simulating is_norming (TRUE/FALSE values)
is_norming <- sample(c(TRUE, FALSE), size = 10000, replace = TRUE)

# Simulating child_id (unique child IDs between 10000 and 99999)
child_id <- sample(10000:99999, size = 10000, replace = TRUE)

# Combine all simulated columns into a single dataset
simulated_data <- tibble(
  data_id = data_id,
  item_id = item_id,
  language = language,
  form = form,
  item_kind = item_kind,
  category = category,
  uni_lemma = uni_lemma,
  lexical_category = lexical_category,
  date_of_test = date_of_test,
  age = age,
  comprehension = comprehension,
  production = production,
  is_norming = is_norming,
  child_id = child_id)

# View the first few rows of the combined dataset
head(simulated_data)

#### Save data ####
# Save the simulated dataset to a CSV file
write_csv(simulated_data, "data/00-simulated_data/simulated_data.csv")
