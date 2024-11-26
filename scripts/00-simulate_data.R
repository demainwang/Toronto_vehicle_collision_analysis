#### Preamble ####
# Purpose: Simulates a dataset of motor vehicle collision from opendatatoronto
# Author: Yitong Wang
# Date: 21 November 2024
# Contact: stevenn.wang@mail.utoronto.ca
# License: MIT
# Pre-requisites: The `tidyverse` package must be installed
# Any other information needed? Make sure you are in the `toronto_collision_analysis` rproj


#### Workspace setup ####
# load libraries
library(tidyverse)
library(lubridate)
library(arrow)

# set seeed for reproducibility
set.seed(555)

# define simulated observation values
n_sim <- 2000

#### Simulate data ####
simulated_data <- data.frame(
  date = sample(seq(as.Date("2006-01-01"), as.Date("2023-12-31"), by = "day"), n_sim, replace = TRUE),
  time = sample(0:2359, n_sim, replace = TRUE),  # Simulate time in 24-hour format
  road_class = sample(c("Major Arterial", "Minor Arterial", "Local Road"), n_sim, replace = TRUE),
  accloc = sample(c("Intersection", "Mid-Block", "Other"), n_sim, replace = TRUE),
  traffctl = sample(c("Signal", "Stop Sign", "Yield"), n_sim, replace = TRUE),
  visibility = sample(c("Clear", "Rain", "Fog", "Snow"), n_sim, replace = TRUE, prob = c(0.7, 0.2, 0.05, 0.05)),
  light = sample(c("Daylight", "Dark", "Dawn/Dusk"), n_sim, replace = TRUE, prob = c(0.5, 0.4, 0.1)),
  rdsfcond = sample(c("Dry", "Wet", "Snow/Ice"), n_sim, replace = TRUE, prob = c(0.8, 0.15, 0.05)),
  impactype = sample(c("Rear-End", "Side Impact", "Head-On", "Other"), n_sim, replace = TRUE),
  invage = round(runif(n_sim, min = 16, max = 90)),  # Simulate ages between 16 and 90
  injury = sample(c("Fatal", "Major", "Minor", "None"), n_sim, replace = TRUE, prob = c(0.01, 0.1, 0.4, 0.49)),
  speeding = sample(c("Yes", "No"), n_sim, replace = TRUE, prob = c(0.2, 0.8)),
  ag_driv = sample(c("Yes", "No"), n_sim, replace = TRUE, prob = c(0.15, 0.85)),
  alcohol = sample(c("Yes", "No"), n_sim, replace = TRUE, prob = c(0.1, 0.9)),
  disability = sample(c("Yes", "No"), n_sim, replace = TRUE, prob = c(0.05, 0.95))
)


#### Save data ####
write_parquet(simulated_data, "data/00-simulated_data/simulated_data.parquet")
