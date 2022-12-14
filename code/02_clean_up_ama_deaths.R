## 02_clean_up_ama_deaths.R ----
## 
## Takes the AMA Physician Mortality file off of Redivis and saves them.
## This raw file has three columns:
##    ME (ID number)
##    DEATH_DT (date of death)
##    CAUSE_OF_DEATH (cause of death but only for COVID)
## Every row in the file is a death from somebody in the AMA Master File.

## Imports ----
library(tidyverse)
library(here)
library(janitor)

## Data ----
raw_data <- readr::read_csv(here::here("data_raw", "Subset_to_after_2014_output.csv"))

## Clean it up and make better dates ----
clean_data <- raw_data %>%
    janitor::clean_names() %>%
    dplyr::rename(date_death = death_dt) %>%
    dplyr::mutate(
        year = lubridate::year(date_death),
        month = lubridate::month(date_death),
        day = lubridate::day(date_death)
    )

## Save ----
saveRDS(clean_data,
        here::here("data", "cleaned_deaths.RDS"),
        compress = "xz")
