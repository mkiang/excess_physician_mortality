## 05_summarize_counts.R ----
## 
## Summarize the counts from the AMA Master File to yearly counts across
## the groups of interest. 

## Imports ----
library(tidyverse)
library(here)
source(here::here("code", "utils.R"))

## Data ----
counts_df <- readRDS(here::here("data", "cleaned_ama_counts.RDS"))

## Flag to those who are active ----
counts_df <- counts_df %>%
    create_active_grp()

## Add larger age categories that match CDC ----
counts_df <- counts_df %>%
    create_age_grp()

## IMG vs USMG ----
counts_df <- counts_df %>%
    create_img_grp()

## Broader race groups ----
counts_df <- counts_df %>%
    create_race_grp()

## Create high risk specialties grouping ----
counts_df <- counts_df %>%
    dplyr::mutate(spec_grp = "all")

## Make an all ages group ----
counts_df <- dplyr::bind_rows(
    counts_df,
    counts_df %>%
        dplyr::mutate(age_grp = "all")
    ) %>%
    dplyr::filter(!is.na(age_grp))

## Make a group for the main ages of interest ----
counts_df <- dplyr::bind_rows(
    counts_df,
    counts_df %>%
        dplyr::filter(age_grp %in% c("65to74", "45to64", "75to84")) %>%
        dplyr::mutate(age_grp = "45to84")
)

## Make an all active status group ----
counts_df <- dplyr::bind_rows(
    counts_df,
    counts_df %>%
        dplyr::mutate(active_grp = "all")
    ) %>%
    dplyr::filter(!is.na(active_grp))

## Make "direct patient care" only active status group ----
counts_df <- dplyr::bind_rows(
    counts_df,
    counts_df %>%
        dplyr::filter(top_desc == "Direct Patient Care",
                      active_grp == "active") %>%
        dplyr::mutate(active_grp = "direct_pt_care")
)

## Make a "not direct patient care" active status group ----
counts_df <- dplyr::bind_rows(
    counts_df,
    counts_df %>% 
        dplyr::filter(
            top_desc %in% c(
                "Medical Research",
                "Administration",
                "Unclassified",
                "Non-Patient Care",
                "Medical Teaching",
                "Resident"
            ),
            active_grp == "active"
        ) %>%
        dplyr::mutate(active_grp = "not_direct_pt_care")
)

## Make aggregations ----
## The identifying tuple is 
##    {year, active_grp, race_grp, age_grp, spec_grp, img_grp}

## Summarize at yearly level ----
summarized_counts <- counts_df %>%
    dplyr::mutate(spec_grp = "all",
                  img_grp = "all",
                  race_grp = "all") %>%
    dplyr::group_by(year, age_grp, active_grp, race_grp, img_grp, spec_grp) %>%
    dplyr::summarize(n_pop = dplyr::n_distinct(me)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(active_grp,
                   race_grp,
                   age_grp,
                   spec_grp,
                   img_grp,
                   year)

## Save ----
saveRDS(summarized_counts,
        here::here("data", "summarized_counts_yearly.RDS"),
        compress = "xz")
