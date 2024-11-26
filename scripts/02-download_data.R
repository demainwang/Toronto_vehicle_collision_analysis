#### Preamble ####
# Purpose: Downloads and saves the motor vehicle collision data from opendatatoronto
# Author: Yitong Wang
# Date: 21 November 2024
# Contact: stevenn.wang@mail.utoronto.ca
# License: MIT
# Pre-requisites: None
# Any other information needed? Make sure you are in the `toronto_collision_analysis` rproj


#### Workspace setup ####
install.packages("tidyverse")
install.packages("readr")
library(tidyverse)
library(readr)
library(arrow)

#### Download data ####
# Define url for csv file
collision_url <- "https://ckan0.cf.opendata.inter.prod-toronto.ca/dataset/0b6d3a00-7de1-440b-b47c-7252fd13929f/resource/fdb2834f-3a92-41dd-b098-fbd84acb9cfe/download/Motor%20Vehicle%20Collisions%20with%20KSI%20Data%20-%204326.csv"

#### Save data ####
# Download vehicle collision data
collision_data <- read_csv(collision_url)
write_parquet(collision_data, "data/01-raw_data/collision_data.parquet") 