# Environmental Conditions and Driver Behavior Shape Collision Outcomes in Toronto

## Overview

This study employs a two stage bayesian modeling approach to analyze vehicle collision patterns in Toronto, examining how environmental conditions and driver behavior influence both injury occurrence and severity. The first model predicts the probability of injury based on environmental and infrastructural factors, while the second estimates injury severity using driver characteristics and behavior. Analysis of historical collision data from 2006 to 2023 reveals that while environmental conditions primarily influence injury occurrence, driver behavior factors like speeding and alcohol use more strongly affect severity levels. Our dual model analysis providing evidence based guidance for targeting both infrastructure improvements and enforcement strategies to reduce severe collision outcomes.

## File Structure

The repo is structured as:

-   `data/00-simulated_data` contains the simulated data based on collision dat obtained from OpenDataToronto
-   `data/01-raw_data` contains the raw data as obtained from opendatatoronto
-   `data/02-analysis_data` contains the cleaned dataset that was constructed.
-   `data/03-model_data` contains the model related data.
-   `model` contains two fitted models. 
-   `other` contains details about LLM chat interactions, and sketches.
-   `paper` contains the files used to generate the paper, including the Quarto document and reference bibliography file, as well as the PDF of the paper. 
-   `scripts` contains the R scripts used to simulate, download, clean and model data.


## Statement on LLM usage

Aspects of the code and some debugging were written with the help of ChatGpt 4o. Entire chat history is available in `other/llms/usage.txt`.
