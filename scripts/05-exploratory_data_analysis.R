#### Preamble ####
# Purpose: Exploratory data analysis on the cleaned data
# Author: Yongqi Liu
# Date: 15 Nov 2024 
# Contact: cassieliu.liu@mail.utoronto.ca
# License: MIT
# Pre-requisites: Make sure cleaned_data.parquet have been run
# Any other information needed? NA

#### Workspace setup ####
#Load Libraries
#install.packages("brms")
#install.packages("scales")
library(tidyverse)
library(wordbankr)
library(arrow)
library(ggplot2)
library(rstanarm)
library(scales)
library(dplyr)

#### Read data ####
analysis_data <- read_parquet("data/02-analysis_data/cleaned_data.parquet")

#### Part A: Data Structure ####
# A1. See the structure of the dataset
str(analysis_data)

# A2. Summary statistics for numeric variables
summary(analysis_data)

#-------------------------------------------------------------------------------
#### Part B: Data Distribution####
# B1. Look at 'age' distribution
ggplot(analysis_data, aes(x = age)) +
  geom_histogram(binwidth = 0.5, fill = "blue", alpha = 1) +
  labs(title = "Age Distribution", x = "Age (months)", y = "Count") +
  scale_y_continuous(labels = comma) +  # Format y-axis labels as integers with commas
  theme_minimal()

# B2. Analyze the distribution of 'category'
ggplot(analysis_data, aes(x = category)) +
  geom_bar(fill = "steelblue", color = "white", alpha = 0.8) +
  labs(title = "Category Distribution", x = "Category", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# B3. Analyze the distribution of 'lexical_category'
ggplot(analysis_data, aes(x = lexical_category)) +
  geom_bar(fill = "steelblue", color = "white", alpha = 0.8) +
  labs(title = "Lexical Category Distribution", x = "Lexical Category", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# B4. Distribution of Vocabulary 'Comprehension'
ggplot(analysis_data, aes(x = comprehension)) +
  geom_histogram(binwidth = 50, fill = "steelblue", color = "white", alpha = 0.8) +
  labs(title = "Distribution of Comprehension Vocabulary Size", x = "Vocabulary Size", y = "Count") +
  theme_minimal()+scale_y_continuous(labels = scales::comma)

# B5. Distribution of Vocabulary 'Production'
ggplot(analysis_data, aes(x = production)) +
  geom_histogram(binwidth = 50, fill = "darkorange", color = "white", alpha = 0.8) +
  labs(title = "Distribution of Production Vocabulary Size", x = "Vocabulary Size", y = "Count") +
  theme_minimal()+scale_y_continuous(labels = scales::comma)

# B6. Plot the Distribution of 'date_of_test'
ggplot(analysis_data, aes(x = month_year, y = test_count)) +
  geom_col(fill = "blue", color = "white", alpha = 1) + 
  labs(title = "Monthly Distribution of Test Dates",
    subtitle = "Visualizing the count of tests conducted each month",
    x = "Month-Year",
    y = "Number of Tests") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months",  
    expand = c(0.01, 0)) +
  scale_y_continuous(
    labels = scales::comma) +
  theme_minimal(base_size = 14) +  # Clean minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5))

# B7. Plot the Distribution of 'is_norming'
ggplot(analysis_data, aes(x = is_norming)) +
  geom_bar(fill = "purple", alpha = 0.8) +
  labs(title = "Proportion of Norming Data", x = "Is Norming?", y = "Count") +
  theme_minimal()

#-------------------------------------------------------------------------------
### Part C: Visualize Relation between Variables ###
# C1. Vocabulary size by age:Plot Production by Age to see how children’s vocabulary develops over time
# Vocabulary size by age: Plot 'Production' vs. 'Age'
analysis_data %>%
  group_by(age) %>%
  summarize(
    avg_production = mean(production, na.rm = TRUE)) %>%
  ggplot(aes(x = age)) +
  geom_line(aes(y = avg_production, color = "Production"), linewidth = 1) +
  labs(
    title = "Average Vocabulary Size by Age",
    subtitle = "Tracking production vocabulary growth",
    x = "Age (months)",
    y = "Average Vocabulary Size",
    color = "Vocabulary Type") +
  scale_color_manual(values = c("Production" = "red")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12))

# C2. Average Production by Item Category and Age
analysis_data %>%
  group_by(category, age) %>%
  summarize(avg_production = mean(production, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = age, y = avg_production, fill = category)) +
  geom_col(position = "stack") +  # Use stack for stacked bars
  labs(
    title = "Average Production by Item Category and Age",
    x = "Age (months)",
    y = "Average Production Score",
    fill = "Item Category"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11))

# C3. Time Series Analysis
# Summarize by month
monthly_summary <- analysis_data %>%
  mutate(month = floor_date(date_of_test, "month")) %>%
  group_by(month) %>%
  summarize(avg_comprehension = mean(comprehension, na.rm = TRUE),
    avg_production = mean(production, na.rm = TRUE))

# Plot production trends
ggplot(monthly_summary, aes(x = month)) +
  geom_line(aes(y = avg_production, color = "Producted Vocabulary"), size = 1) +
  labs(title = "Time Trends in Vocabulary Production",
    x = "Month",
    y = "Average Vocabulary Size",
    color = "Vocabulary Type") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top",
    plot.title = element_text(size = 14, face = "bold"))
  
#-------------------------------------------------------------------------------  
### Part D: Model data ####
# D1. Define the Bayesian regression model
bayesian_model <-stan_glm(
    formula = production ~ age + category + lexical_category,  # Model formula
    data = analysis_data,                               # Dataset
    family = gaussian(),                                 # Gaussian distribution for continuous outcome
    prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
    prior_intercept = 
      normal(location = 0, scale = 2.5, autoscale = TRUE),
    seed = 853,
    iter = 2000,                                        # Number of iterations
    chains = 4,                                         # Number of chains
    cores = 4)                                        # Use multiple core                     
  
# Print the summary of the Bayesian model
summary(bayesian_model)
  
# Visualize posterior distributions
plot(bayesian_model)
  
# Check posterior predictive checks
pp_check(bayesian_model)

# D2. Define a logistic regression model
# Add a binary outcome variable
analysis_data <- analysis_data %>%
  mutate(high_comprehension = ifelse(comprehension > 300, 1, 0)) 

# Combine production and comprehension into a new variable
analysis_data <- analysis_data %>%
  mutate(
    prod_comp_mean = (production + comprehension) / 2,  # Average
    prod_comp_diff = production - comprehension,       # Difference (not used)
    prod_comp_ratio = production / comprehension       # Ratio (not used)
  )

# Drop rows where prod_comp_mean is NA
analysis_data <- analysis_data %>% drop_na(prod_comp_mean)

# Standardize numerical predictors
analysis_data <- analysis_data %>%
  mutate(
    age_scaled = scale(age),  # Standardize age
    prod_comp_mean_scaled = scale(prod_comp_mean)  # Standardize prod_comp_mean
  )

# Collapse `lexical_category` into top 5 most frequent levels and group the rest
analysis_data <- analysis_data %>%
  mutate(
    lexical_category = fct_lump(lexical_category, n = 5)  # Top 5 frequent categories
  )

# Build the logistic regression model
logistic_model <- glm(
  high_comprehension ~ age_scaled + prod_comp_mean_scaled + lexical_category,  # Formula
  data = analysis_data,  # Data
  family = binomial,  # Logistic regression
  weights = ifelse(high_comprehension == 1, 10, 1)  # Adjust weights for class imbalance
)

# Print model summary
summary(logistic_model)

# D3. Define a multiple linear regression model
# Create a new variable: average of comprehension and production
analysis_data <- analysis_data %>%
  mutate(avg_comp_prod = (comprehension + production) / 2)

# Fit the linear model with age, lexical_category, is_norming as indicator variables
linear_model <- lm(
  avg_comp_prod ~ age + lexical_category + is_norming, data = analysis_data)

# Print the summary
summary(linear_model)


#### Save model ####
saveRDS(bayesian_model, file = "models/bayesian_model.rds")
saveRDS(logistic_model, file = "models/logistic_model_first.rds")
saveRDS(linear_model, file = "models/linear_model.rds")
