#### Preamble ####
# Purpose: Cleans the raw collision data
# Author: Yitong Wang
# Date: 21 November 2024
# Contact: stevenn.wang@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
#   - The `tidyverse`, `testthat`, `arrow` packages must be installed and loaded
#   - 02-download_data.R must have been run
# Any other information needed? Make sure you are in the `toronto_collision_analysis` rproj

#### Workspace setup ####
library(tidyverse)
library(here)
library(arrow)

#### Clean data ####
# Read raw data
raw_data <- read_parquet(here("data", "01-raw_data", "collision_data.parquet")) %>%
  clean_names()

# Define a function to convert age ranges to averages
convert_age <- function(age) {
  if (grepl("to", age)) {  # Check if the value is a range
    numbers <- as.numeric(unlist(strsplit(age, " to ")))  # Split range
    return(mean(numbers))  # Calculate and return midpoint
  } else if (is.na(as.numeric(age))) {
    return(NA)  # Handle non-numeric values (e.g., "Unknown")
  } else {
    return(as.numeric(age))  # Return single numeric values as-is
  }
}

# Clean and filter relevant columns
cleaned_data <- raw_data %>%
  select(
    date, time, road_class, accloc, traffctl, visibility, light, 
    rdsfcond, impactype, invage, injury, speeding, ag_driv, alcohol, disability
  ) %>%
  # Convert date to Date format
  mutate(
    date = as.Date(date, format = "%Y-%m-%d"),
    time = as.numeric(time),  # Convert time to numeric values for easier analysis
    invage = sapply(invage, convert_age),  # Apply the age conversion function
  ) %>%
  # Remove rows where traffctl, visibility, light, rdsfcond, or impactype contain "None"
  filter(
    accloc != "None",
    road_class != "None",
    traffctl != "None",
    visibility != "None",
    light != "None",
    rdsfcond != "None",
    impactype != "None"
  ) %>%
  # Handle missing data
  drop_na() %>%
  # Filter legal driving age
  filter(invage >= 16) %>%
  # Remove duplicate rows
  distinct()

summary(cleaned_data)

#### Save data ####
write_parquet(cleaned_data, "data/02-analysis_data/analysis_data.parquet")
