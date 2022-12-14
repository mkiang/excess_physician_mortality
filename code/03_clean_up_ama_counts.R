## 03_clean_up_ama_counts.R ----
## 
## Clean up the AMA Master File to get denominators. This also adds back the 
## information from the reference columns so it is easier to work with. 
## 
## The raw AMA Master File columns are:
##      ME (ID number)
##      IMG_FLAG (international medical graduate)
##      TOP_CD (type of practice)
##      PE_CD (place of employment)
##      PRIM_SPEC_CD (primary specialty)
##      MAILING_STATE
##      BIRTH_YEAR
##      GENDER
##      RACE_ETHNICITY

## Imports ----
library(tidyverse)
library(here)
library(janitor)

## Data ----
load(here::here("data", "reference_tables.RDA"))
raw_data <- readr::read_csv(here::here(
    "data_raw",
    "Union_on_all_the_years_and_add_year_column_output.csv"
))
deaths_orig <- readRDS(here::here("data", "cleaned_deaths.RDS"))

## Add descriptions and better variables ----
clean_data <- raw_data %>%
    janitor::clean_names() %>%
    dplyr::rename(spec_cd = prim_spec_cd) %>%
    dplyr::mutate(img = img_flag + 0,
           age = year - birth_year,
           me_num = as.numeric(me)) %>%
    dplyr::left_join(pe_ref %>%
                  dplyr::rename(pe_desc = description)) %>%
    dplyr::left_join(spec_ref %>%
                  dplyr::rename(spec_desc = description)) %>%
    dplyr::left_join(top_cd_ref %>%
                  dplyr::rename(top_desc = description))

## Rearrange columns and rows ----
clean_data <- clean_data %>%
    dplyr::select(
        me,
        year,
        age,
        gender,
        race_ethnicity,
        img,
        mailing_state,
        top_cd,
        top_desc,
        pe_cd,
        pe_desc,
        spec_cd,
        spec_desc,
        me_num,
        dplyr::everything()
    ) %>%
    dplyr::arrange(year, me_num)

## Add in the deaths so we can remove dupes in subsequent years ----
clean_data <- clean_data %>%
    dplyr::left_join(deaths_orig %>%
                  dplyr::rename(deaths_year = year),
              by = "me")

## Now we want only the earliest instance of a death so they don't reappear
## in the denominator in a later year.
## Subset into mutually exclusive categories: (1) did not die, (2) died but
## date of death makes sense (i.e., occurs after last observation), (3) died
## but date of death does not make sense (i.e., take the earliest observation).
## 
## At this point, clean_data has 8675306 observations, did_not_die has 8559082
## observations, died_after_obs has 112208 observations, and died_before_obs
## has 1975 observations. After all the cleaning and reassembling, the new
## clean_counts has 8673265 (~2000 fewer)
did_not_die <- clean_data %>%
    dplyr::filter(is.na(date_death))

died_after_obs <- clean_data %>%
    dplyr::filter(!is.na(date_death),
           deaths_year >= year)

died_before_obs <- clean_data %>%
    dplyr::filter(!is.na(date_death),
           deaths_year < year) %>%
    dplyr::arrange(year, .by_group = TRUE) %>%
    dplyr::filter(year == min(year))

clean_counts <- dplyr::bind_rows(did_not_die,
                          died_after_obs,
                          died_before_obs) %>%
    dplyr::ungroup() %>%
    dplyr::select(-day, -month, -deaths_year, -cause_of_death, -date_death) %>%
    dplyr::distinct()

## Save ----
saveRDS(clean_counts,
        here::here("data", "cleaned_ama_counts.RDS"),
        compress = "xz")
