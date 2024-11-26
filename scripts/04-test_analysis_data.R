#### Preamble ####
# Purpose: Tests the structure and validity of the cleaned motor collision data
# Author: Yitong Wang
# Date: 21 November 2024
# Contact: stevenn.wang@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
#   - The `tidyverse`, `testthat`, `arrow` packages must be installed and loaded
#   - 03-clean_data.R must have been run
# Any other information needed? Make sure you are in the `toronto_collision_analysis` rproj


#### Workspace setup ####
install.packages("testthat")
library(tidyverse)
library(testthat)
library(arrow)

analysis_data <- read_parquet("data/02-analysis_data/analysis_data.parquet")

#### Test data ####
test_that("Analysis data has the correct structure and properties", {
  # Check the number of columns
  expect_equal(ncol(analysis_data), 15)
  
  # Check if the dataset has the expected column names
  expected_columns <- c(
    "date", "time", "road_class", "accloc", "traffctl", 
    "visibility", "light", "rdsfcond", "impactype", 
    "invage", "injury", "speeding", "ag_driv", "alcohol", "disability"
  )
  expect_equal(colnames(analysis_data), expected_columns)
  
  # Ensure there are no duplicated rows
  expect_equal(nrow(analysis_data), nrow(distinct(analysis_data)))
})

test_that("Analysis data has no missing values", {
  # Check for missing values in the dataset
  expect_true(all(complete.cases(analysis_data)))
})

test_that("Analysis data has correct date and time formatting", {
  # Check that `date` is properly formatted as YYYY-MM-DD
  expect_true(all(!is.na(as.Date(analysis_data$date, format = "%Y-%m-%d"))))
  # Check that `time` is numeric and within valid 24-hour time range
  expect_true(all(analysis_data$time >= 0 & analysis_data$time <= 2359))
})

test_that("Year range in date column is valid", {
  # Extract year from the date column
  year_range <- format(as.Date(analysis_data$date, format = "%Y-%m-%d"), "%Y")
  year_range <- as.numeric(year_range)
  # Check that all years fall within the expected range
  expect_true(all(year_range >= 2006 & year_range <= 2023))
})

test_that("Numeric columns have valid ranges", {
  # Ensure `invage` is within the valid range (16 to 90)
  expect_true(all(analysis_data$invage >= 16))
})

test_that("Categorical columns have valid levels", {
  # Validate `road_class` levels
  expect_true(all(unique(analysis_data$road_class) %in% c("Major Arterial", "Minor Arterial", "Collector", "Local", 
                                                          "Other", "Pending", "Laneway", "Expressway", "Expressway Ramp", "Major Shoreline")))
  
  # Validate `accloc` levels
  expect_true(all(unique(analysis_data$accloc) %in% c("Intersection Related", "At Intersection", "Non Intersection", 
                                                      "Private Driveway", "At/Near Private Drive", "Underpass or Tunnel", 
                                                      "Overpass or Bridge", "Trail", "Laneway", "Other")))
  
  # Validate `traffctl` levels
  expect_true(all(unique(analysis_data$traffctl) %in% c("No Control", "Traffic Signal", "Pedestrian Crossover", "Stop Sign",
                                                        "Yield Sign", "Traffic Controller", "School Guard", "Police Control", 
                                                        "Traffic Gate", "Streetcar (Stop for)")))
  
  # Validate `visibility` levels
  expect_true(all(unique(analysis_data$visibility) %in% c("Clear", "Snow", "Other", "Rain", "Strong wind", 
                                                          "Fog, Mist, Smoke, Dust", "Drifting Snow", "Freezing Rain")))
  
  # Validate `injury` levels
  expect_true(all(unique(analysis_data$injury) %in% c("Fatal", "Major", "Minimal", "Minor", "None")))
})

test_that("No extra spaces in categorical variables", {
  # Check for leading or trailing spaces in all categorical columns
  categorical_columns <- c(
    "road_class", "accloc", "traffctl", "visibility", 
    "light", "rdsfcond", "impactype", "injury", 
    "speeding", "ag_driv", "alcohol", "disability"
  )
  for (col in categorical_columns) {
    expect_false(any(grepl("^\\s|\\s$", analysis_data[[col]])))
  }
})