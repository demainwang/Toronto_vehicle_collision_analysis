#### Preamble ####
# Purpose: Tests the structure and validity of the simulated motor collision data
# Author: Yitong Wang
# Date: 21 November 2024
# Contact: stevenn.wang@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
  # - The `tidyverse`, `testthat`, `arrow` packages must be installed and loaded
  # - 00-simulate_data.R must have been run
# Any other information needed? Make sure you are in the `toronto_collision_analysis` rproj


#### Workspace setup ####
install.packages("testthat")
library(tidyverse)
library(testthat)
library(arrow)

simulated_data <- read_parquet("data/00-simulated_data/simulated_data.parquet")

#### Test data ####
test_that("Simulated data has the correct structure and properties", {
  # Check the number of columns
  expect_equal(ncol(simulated_data), 15)
  
  # Check if the dataset has the expected column names
  expected_columns <- c(
    "date", "time", "road_class", "accloc", "traffctl", 
    "visibility", "light", "rdsfcond", "impactype", 
    "invage", "injury", "speeding", "ag_driv", "alcohol", "disability"
  )
  expect_equal(colnames(simulated_data), expected_columns)
  
  # Ensure there are no duplicated rows
  expect_equal(nrow(simulated_data), nrow(distinct(simulated_data)))
})

test_that("Simulated data has no missing values", {
  # Check for missing values in the dataset
  expect_true(all(complete.cases(simulated_data)))
})

test_that("Simulated data has correct date and time formatting", {
  # Check that `date` is properly formatted as YYYY-MM-DD
  expect_true(all(!is.na(as.Date(simulated_data$date, format = "%Y-%m-%d"))))
  # Check that `time` is numeric and within valid 24-hour time range
  expect_true(all(simulated_data$time >= 0 & simulated_data$time <= 2359))
})

test_that("Year range in date column is valid", {
  # Extract year from the date column
  year_range <- format(as.Date(simulated_data$date, format = "%Y-%m-%d"), "%Y")
  year_range <- as.numeric(year_range)
  # Check that all years fall within the expected range
  expect_true(all(year_range >= 2006 & year_range <= 2023))
})

test_that("Numeric columns have valid ranges", {
  # Ensure `invage` is within the valid range (16 to 90)
  expect_true(all(simulated_data$invage >= 16 & simulated_data$invage <= 90))
})

test_that("Categorical columns have valid levels", {
  # Validate `road_class` levels
  expect_true(all(unique(simulated_data$road_class) %in% c("Major Arterial", "Minor Arterial", "Local Road")))
  
  # Validate `accloc` levels
  expect_true(all(unique(simulated_data$accloc) %in% c("Intersection", "Mid-Block", "Other")))
  
  # Validate `traffctl` levels
  expect_true(all(unique(simulated_data$traffctl) %in% c("Signal", "Stop Sign", "Yield")))
  
  # Validate `visibility` levels
  expect_true(all(unique(simulated_data$visibility) %in% c("Clear", "Rain", "Fog", "Snow")))
  
  # Validate `injury` levels
  expect_true(all(unique(simulated_data$injury) %in% c("Fatal", "Major", "Minor", "None")))
})

test_that("No extra spaces in categorical variables", {
  # Check for leading or trailing spaces in all categorical columns
  categorical_columns <- c(
    "road_class", "accloc", "traffctl", "visibility", 
    "light", "rdsfcond", "impactype", "injury", 
    "speeding", "ag_driv", "alcohol", "disability"
  )
  for (col in categorical_columns) {
    expect_false(any(grepl("^\\s|\\s$", simulated_data[[col]])))
  }
})