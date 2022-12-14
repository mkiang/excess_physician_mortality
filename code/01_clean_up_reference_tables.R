## 01_clean_up_reference_tables.R ----
## 
## Just cleans up the AMA reference files from Redivis and saves them.

## Imports ----
library(tidyverse)
library(here)
library(janitor)

## Data ----
pe_ref <-
    readr::read_csv(here::here("data_raw", "STANFORD_PE_REFERENCE.csv")) %>%
    janitor::clean_names()
top_cd_ref <-
    readr::read_csv(here::here("data_raw", "STANFORD_TOP_REFERENCE.csv")) %>%
    janitor::clean_names()
spec_ref <-
    readr::read_csv(here::here("data_raw", "STANFORD_SPEC_REFERENCE.csv")) %>%
    janitor::clean_names()

## Save ----
save(pe_ref,
     top_cd_ref,
     spec_ref,
     file = here::here("data", "reference_tables.RDA"))
