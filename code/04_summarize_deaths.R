## 04_summarize_deaths.R ----
## 
## Summarize the deaths to the monthly level across the groupings we are
## interested in.

## Imports ----
library(tidyverse)
library(here)
source(here::here("code", "utils.R"))

## Constants ----
FIRST_MONTH <- as.Date("2016-01-01")

## Data ----
deaths_orig <- readRDS(here::here("data", "cleaned_deaths.RDS"))
counts_orig <- readRDS(here::here("data", "cleaned_ama_counts.RDS"))

## Add day of the week, just for reference ----
deaths_df <- deaths_orig %>%
    dplyr::mutate(wday = lubridate::wday(date_death, label = TRUE))

## Subset counts to those that died ----
## We need the physician characteristics of those who died and that's in the
## AMA Master File. Since we observe them in all living years, we'll take
## the most recent year (i.e., closest to their death)
counts_df <- counts_orig %>%
    dplyr::filter(me %in% deaths_df$me)

counts_df <- counts_df %>%
    dplyr::arrange(me_num, year) %>%
    dplyr::group_by(me) %>%
    dplyr::filter(year == max(year)) %>%
    dplyr::ungroup() %>%
    dplyr::rename(year_counts_df = year)

## Flag those who are active ----
counts_df <- counts_df %>%
    create_active_grp()

## Add larger age categories that match CDC ----
counts_df <- counts_df %>%
    create_age_grp()

## Create placeholder indices ----
## We aren't going to look at IMG status, race, or specialties in this paper,
## but we want the coded to be flexible enough to look at it in the future
## if we decide. It's easiest to set up the index columns here.
## IMG vs USMG ----
counts_df <- counts_df %>%
    create_img_grp()

## Broader race groups ----
counts_df <- counts_df %>%
    create_race_grp()

## Create a placeholder specialties grouping ----
counts_df <- counts_df %>%
    dplyr::mutate(spec_grp = "all")

## Join with provider characteristics ----
deaths_df <- deaths_df %>%
    dplyr::left_join(counts_df,
              by = "me") %>%
    dplyr::mutate(age = year - birth_year) %>%
    dplyr::filter(age >= 25) %>% 
    dplyr::filter(!is.na(age))

## Add month markers ----
deaths_df <- deaths_df %>%
    dplyr::filter(date_death >= FIRST_MONTH) %>%
    dplyr::mutate(month_from_start = (year - min(year)) * 12 + month)

## Save individual-level deaths ----
saveRDS(deaths_df, here::here("data", "deaths_df_individual_level.RDS"))

## Make an all ages group ----
deaths_df <- dplyr::bind_rows(deaths_df,
                       deaths_df %>%
                           dplyr::mutate(age_grp = "all"))

## Make a group for the main ages of interest ----
deaths_df <- dplyr::bind_rows(
    deaths_df,
    deaths_df %>%
        dplyr::filter(age_grp %in% c("65to74", "45to64", "75to84")) %>%
        dplyr::mutate(age_grp = "45to84")
    )

## Make an all active status group ----
deaths_df <- dplyr::bind_rows(deaths_df,
                       deaths_df %>%
                           dplyr::mutate(active_grp = "all"))

## Make "direct patient care" only active status group ----
deaths_df <- dplyr::bind_rows(
    deaths_df,
    deaths_df %>%
        dplyr::filter(top_desc == "Direct Patient Care",
                      active_grp == "active") %>%
        dplyr::mutate(active_grp = "direct_pt_care")
)

## Make a "not direct patient care" active status group ----
deaths_df <- dplyr::bind_rows(
    deaths_df,
    deaths_df %>%
        dplyr::filter(
            active_grp == "active",
            top_desc %in% c(
                "Medical Research",
                "Administration",
                "Unclassified",
                "Non-Patient Care",
                "Medical Teaching",
                "Resident"
            )
        ) %>%
        dplyr::mutate(active_grp = "not_direct_pt_care")
)

## Make an all causes vs excluding covid group ----
deaths_df <- dplyr::bind_rows(
    deaths_df %>%
        dplyr::mutate(death_type = "all_cause"),
    deaths_df %>%
        dplyr::filter(is.na(cause_of_death) |
                   cause_of_death != "COVID-19") %>%
        dplyr::mutate(death_type = "exclude_covid")
)

## Summarize at monthly level ----
summarized_monthly_deaths <- deaths_df %>%
    dplyr::mutate(spec_grp = "all",
           img_grp = "all",
           race_grp = "all") %>%
    dplyr::group_by(month_from_start,
             death_type,
             age_grp,
             active_grp,
             race_grp,
             img_grp,
             spec_grp) %>%
    dplyr::summarize(
        date_start = min(date_death),
        date_end = max(date_death),
        n_deaths = dplyr::n()
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(death_type,
            active_grp,
            race_grp,
            age_grp,
            spec_grp,
            img_grp,
            date_start) %>%
    dplyr::select(
        month_from_start,
        date_start,
        date_end,
        death_type,
        age_grp,
        active_grp,
        race_grp,
        img_grp,
        spec_grp,
        n_deaths,
        dplyr::everything()
    )

## Save ----
saveRDS(
    summarized_monthly_deaths,
    here::here("data", "summarized_deaths_monthly.RDS"),
    compress = "xz"
)
