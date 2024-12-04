# Motor Traffic Collision Patterns in Toronto: Analysis based on Environmental Conditions and Driver Behaviours

## Overview

This study employs a two stage bayesian modeling approach to analyze vehicle collision patterns in Toronto, examining how environmental conditions and driver behavior influence both injury occurrence and severity. The first model predicts the probability of injury based on environmental and infrastructural factors, while the second estimates injury severity using driver characteristics and behavior. Analysis of historical collision data from 2006 to 2023 shows that while environmental conditions primarily influence injury occurrence, driver behavior factors like speeding and alcohol use more strongly affect severity levels. These findings demonstrate the need for a dual-pronged approach to urban safety through targeted infrastructure improvements in high-risk areas to prevent collisions, alongside enhanced enforcement strategies focusing on dangerous driving behaviors.

## File Structure

The repo is structured as:

-   `data/00-simulated_data` contains the simulated data based on collision data obtained from opendatatoronto
-   `data/01-raw_data` contains the raw collision data as obtained from opendatatoronto
-   `data/02-analysis_data` contains the analysis collison data after cleaning.
-   `data/03-model_data` contains the model related data.
-   `model` contains two fitted models' rds files. 
-   `other` contains details about LLM chat interactions, sketches, and literature.
-   `paper` contains the files used to generate the paper, including the Quarto document and reference bibliography file, as well as the PDF of the paper. 
-   `scripts` contains `00-simulate-data.R` scripts used to simulate, `01-test_simulated_data.R` for test simulated data, `02-download_data.R` for data downloading, `03-clean_data.R` for data cleaning, `04-test_analysis_data.R` for test of cleaned data, `05-model_data.R` for model constructing.


## Statement on LLM usage

Aspects of the code and some debugging were written with the help of ChatGpt 4o. Entire chat history is available in `other/llms/usage.txt`.
