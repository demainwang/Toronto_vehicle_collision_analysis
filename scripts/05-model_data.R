#### Preamble ####
# Purpose: Construct two bayesian models for the prediction of injury occurrence and severity
# Author: Yitong Wang
# Date: 21 November 2024
# Contact: stevenn.wang@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
#   - 03-clean_data.R must have been run
# Any other information needed? Make sure you are in the `toronto_collision_analysis` rproj

# Load packages
library(tidyverse)
library(brms)
library(here)
library(tidybayes)
library(bayesplot)
library(pROC)

# Set Seed for Reproducibility
set.seed(777)

# Data Preparation
# Prepare data for both models with different predictor sets

# Model 1: Injury Occurrence (focuses on circumstantial and infrastructural factors)
occurrence_data <- cleaned_data %>%
  mutate(
    # Binary outcome
    injury_occurred = case_when(
      injury == "None" ~ "No",
      TRUE ~ "Yes"
    ) %>% factor(levels = c("No", "Yes")),
    
    # Environmental conditions
    visibility = case_when(
      visibility == "Clear" ~ "Good",
      TRUE ~ "Poor"
    ) %>% factor(),
    
    light = case_when(
      light %in% c("Daylight", "Daylight, artificial") ~ "Day",
      TRUE ~ "Night"
    ) %>% factor(),
    
    rdsfcond = case_when(
      rdsfcond == "Dry" ~ "Good",
      TRUE ~ "Poor"
    ) %>% factor(),
    
    # Infrastructure factors
    road_class = case_when(
      road_class %in% c("Major Arterial", "Expressway") ~ "Major",
      road_class %in% c("Minor Arterial", "Collector") ~ "Minor",
      TRUE ~ "Local"
    ) %>% factor(),
    
    # Location type
    accloc = case_when(
      str_detect(accloc, "Intersection") ~ "Intersection",
      str_detect(accloc, "Private") ~ "Private",
      TRUE ~ "Other"
    ) %>% factor(),
    
    # Traffic control
    traffctl = case_when(
      traffctl == "Traffic Signal" ~ "Signal",
      traffctl == "Stop Sign" ~ "Stop",
      traffctl == "No Control" ~ "None",
      TRUE ~ "Other"
    ) %>% factor(),
    
    # Time-based features
    hour = as.numeric(substr(time, 1, 2)),
    is_rush_hour = factor(ifelse(hour %in% c(7:9, 16:18), "Yes", "No")),
    is_weekend = factor(ifelse(weekdays(date) %in% c("Saturday", "Sunday"), "Yes", "No")),
    
    # Impact type
    impactype = factor(impactype)
  ) %>%
  select(injury_occurred, visibility, light, rdsfcond, road_class,
         accloc, traffctl, is_rush_hour, is_weekend, impactype)

# Check for class imbalance in the occurrence data
print(table(occurrence_data$injury_occurred))

# Model 2: Injury Severity (focuses on behavioral and impact factors)
severity_data <- cleaned_data %>%
  filter(injury != "None") %>%
  mutate(
    # Simplified severity levels (binary factor)
    injury_severity = case_when(
      injury %in% c("Fatal", "Major") ~ "Severe",
      TRUE ~ "Minor"
    ) %>% factor(levels = c("Minor", "Severe")),
    
    # Driver behavior
    speeding = factor(speeding),
    ag_driv = factor(ag_driv),
    alcohol = factor(alcohol),
    disability = factor(disability),
    
    # Driver age groups
    age_group = case_when(
      invage < 25 ~ "Young",
      invage < 65 ~ "Adult",
      TRUE ~ "Senior"
    ) %>% factor(),
    
    # Impact type simplified
    impact = case_when(
      str_detect(impactype, "Pedestrian") ~ "Pedestrian",
      str_detect(impactype, "Cyclist") ~ "Cyclist",
      str_detect(impactype, "Rear") ~ "Rear End",
      str_detect(impactype, "Angle") ~ "Angle",
      TRUE ~ "Other"
    ) %>% factor(),
    
    # Environmental context
    light = case_when(
      light %in% c("Daylight", "Daylight, artificial") ~ "Day",
      TRUE ~ "Night"
    ) %>% factor(),
    
    road_class = case_when(
      road_class %in% c("Major Arterial", "Expressway") ~ "Major",
      TRUE ~ "Minor"
    ) %>% factor()
  ) %>%
  select(injury_severity, speeding, ag_driv, alcohol, 
         disability, age_group, impact, light, road_class)

# Check for class imbalance in the severity data
print(table(severity_data$injury_severity))

# Multicollinearity Check

# For Occurrence Model
# Create a formula for VIF calculation
occurrence_formula <- injury_occurred ~ visibility + light + rdsfcond + 
  road_class + accloc + traffctl + is_rush_hour + is_weekend + impactype

# Fit a simple logistic regression for VIF calculation
occurrence_glm <- glm(occurrence_formula, data = occurrence_data, family = binomial())

# Calculate VIF
occurrence_vif <- check_collinearity(occurrence_glm)
print(occurrence_vif)

# For Severity Model
# Create a formula for VIF calculation
severity_formula <- injury_severity ~ speeding + ag_driv + alcohol + 
  disability + age_group + impact + light + road_class

# Fit a logistic regression for VIF calculation
severity_glm <- glm(severity_formula, data = severity_data, family = binomial())

# Calculate VIF
severity_vif <- check_collinearity(severity_glm)
print(severity_vif)


# Data Splitting

# Split data for both models using stratified sampling to maintain class proportions

# Split for occurrence model
occurrence_split <- initial_split(occurrence_data, prop = 0.7, strata = injury_occurred)
occurrence_train <- training(occurrence_split)
occurrence_test <- testing(occurrence_split)

# Split for severity model
severity_split <- initial_split(severity_data, prop = 0.7, strata = injury_severity)
severity_train <- training(severity_split)
severity_test <- testing(severity_split)


# Model 1: Injury Occurrence Model

# Specify weakly informative priors
priors_occurrence <- c(
  set_prior("normal(0, 5)", class = "b")
)

# Fit the occurrence model with increased chains and iterations
occurrence_model <- brm(
  injury_occurred ~ visibility + light + rdsfcond + 
    road_class + accloc + traffctl +
    is_rush_hour + is_weekend +
    impactype,
  data = occurrence_train,
  family = bernoulli(),
  prior = priors_occurrence,
  chains = 4,
  iter = 4000,
  warmup = 2000,
  seed = 123,
  control = list(adapt_delta = 0.95)
)


# Model 2: Injury Severity Model

# Specify weakly informative priors
priors_severity <- c(
  set_prior("normal(0, 5)", class = "b")
)

# Fit the severity model using Bernoulli
severity_model <- brm(
  injury_severity ~ speeding + ag_driv + alcohol + 
    disability + age_group + impact +
    light + road_class,
  data = severity_train,
  family = bernoulli(link = "logit"),
  prior = priors_severity,
  chains = 4,
  iter = 4000,
  warmup = 2000,
  seed = 123,
  control = list(adapt_delta = 0.95)
)


# Model Validation and Evaluation

# Function to calculate evaluation metrics
calculate_metrics <- function(model, test_data, response_var) {
  # Get predicted probabilities
  predicted_probs <- fitted(model, newdata = test_data, summary = FALSE)
  predicted_probs_mean <- colMeans(predicted_probs)
  
  # Predicted classes
  predicted_class <- ifelse(predicted_probs_mean > 0.5, levels(test_data[[response_var]])[2], levels(test_data[[response_var]])[1])
  predicted_class <- factor(predicted_class, levels = levels(test_data[[response_var]]))
  
  # Actual classes
  actual_class <- test_data[[response_var]]
  
  # Confusion Matrix
  cm <- confusionMatrix(predicted_class, actual_class)
  print(cm)
  
  # ROC and AUC
  actual_numeric <- ifelse(actual_class == levels(test_data[[response_var]])[2], 1, 0)
  roc_obj <- roc(actual_numeric, predicted_probs_mean)
  print(roc_obj)
  plot(roc_obj, main = paste("ROC Curve for", response_var))
  
  # Calculate RMSE
  rmse_value <- sqrt(mean((actual_numeric - predicted_probs_mean)^2))
  print(paste("RMSE:", round(rmse_value, 4)))
  
  return(list(confusion_matrix = cm, rmse = rmse_value, roc = roc_obj))
}

# Evaluate Occurrence Model
metrics_occurrence <- calculate_metrics(
  model = occurrence_model,
  test_data = occurrence_test,
  response_var = "injury_occurred"
)

# Evaluate Severity Model
metrics_severity <- calculate_metrics(
  model = severity_model,
  test_data = severity_test,
  response_var = "injury_severity"
)


# Save models and test data
saveRDS(occurrence_model, here("models", "occurrence_model.rds"))
saveRDS(severity_model, here("models", "severity_model.rds"))
write_parquet(occurrence_test, here("data", "03-model_related_data", "occurrence_test.parquet"))
write_parquet(severity_test, here("data", "03-model_related_data", "severity_test.parquet"))

# Generate Predictions for Scenarios

# Create specific scenarios for each model based on their predictors
# Occurrence scenarios
occurrence_scenarios <- expand_grid(
  visibility = c("Good", "Poor"),
  light = c("Day", "Night"),
  rdsfcond = c("Good", "Poor"),
  road_class = c("Major", "Minor", "Local"),
  accloc = c("Intersection", "Private", "Other"),
  traffctl = c("Signal", "Stop", "None", "Other"),
  is_rush_hour = c("Yes", "No"),
  is_weekend = c("Yes", "No"),
  impactype = levels(occurrence_train$impactype)[1:3]  # Top 3 common types
)

# Severity scenarios
severity_scenarios <- expand_grid(
  speeding = c("None", "Yes"),
  ag_driv = c("None", "Yes"),
  alcohol = c("None", "Yes"),
  disability = c("None", "Yes"),
  age_group = c("Young", "Adult", "Senior"),
  impact = c("Pedestrian", "Cyclist", "Rear End", "Angle", "Other"),
  light = c("Day", "Night"),
  road_class = c("Major", "Minor")
)

# Ensure factor levels match training data
occurrence_scenarios <- occurrence_scenarios %>%
  mutate(
    visibility = factor(visibility, levels = levels(occurrence_train$visibility)),
    light = factor(light, levels = levels(occurrence_train$light)),
    rdsfcond = factor(rdsfcond, levels = levels(occurrence_train$rdsfcond)),
    road_class = factor(road_class, levels = levels(occurrence_train$road_class)),
    accloc = factor(accloc, levels = levels(occurrence_train$accloc)),
    traffctl = factor(traffctl, levels = levels(occurrence_train$traffctl)),
    is_rush_hour = factor(is_rush_hour, levels = levels(occurrence_train$is_rush_hour)),
    is_weekend = factor(is_weekend, levels = levels(occurrence_train$is_weekend)),
    impactype = factor(impactype, levels = levels(occurrence_train$impactype))
  )

severity_scenarios <- severity_scenarios %>%
  mutate(
    speeding = factor(speeding, levels = levels(severity_train$speeding)),
    ag_driv = factor(ag_driv, levels = levels(severity_train$ag_driv)),
    alcohol = factor(alcohol, levels = levels(severity_train$alcohol)),
    disability = factor(disability, levels = levels(severity_train$disability)),
    age_group = factor(age_group, levels = levels(severity_train$age_group)),
    impact = factor(impact, levels = levels(severity_train$impact)),
    light = factor(light, levels = levels(severity_train$light)),
    road_class = factor(road_class, levels = levels(severity_train$road_class))
  )

# Get predictions
occurrence_pred <- fitted(
  occurrence_model, 
  newdata = occurrence_scenarios,
  summary = TRUE,
  allow_new_levels = TRUE
)

severity_pred <- fitted(
  severity_model, 
  newdata = severity_scenarios,
  summary = TRUE,
  allow_new_levels = TRUE
)

# Combine scenarios with their predictions
occurrence_results <- bind_cols(
  occurrence_scenarios,
  Injury_Probability = occurrence_pred[, "Estimate"]
) %>%
  arrange(desc(Injury_Probability))

severity_results <- bind_cols(
  severity_scenarios,
  Severity_Probability = severity_pred[, "Estimate"]
) %>%
  arrange(desc(Severity_Probability))

# Save predictions
write_parquet(occurrence_results, here("data", "03-model_related_data", "occurrence_predictions.parquet"))
write_parquet(severity_results, here("data", "03-model_related_data", "severity_predictions.parquet"))
