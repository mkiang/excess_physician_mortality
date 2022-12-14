## 06_create_analytic_dataset.R ----
##
## Combine the counts and deaths to create a monthly analytic data set. Note
## that we need to linearly interpolate the population counts since we have
## end-of-year estimates and we'll need to adjust for deaths that occur
## in each year. We don't actually analyze by race/ethnicity, img status,
## or specialty due to small population counts, but we might want to one day
## (for example, using a different model and imposing more assumptions on
## the data) so the analytic file will have those columns with only a
## single value ("all").

## Imports ----
library(tidyverse)
library(here)

## Constants ----
FIRST_MONTH <- as.Date("2016-01-01")
LAST_MONTH  <- as.Date("2021-12-31")

## Data ----
pop_df <- readRDS(here::here("data", "summarized_counts_yearly.RDS"))
monthly_orig <- readRDS(here::here("data", "summarized_deaths_monthly.RDS")) %>%
    dplyr::filter(death_type == "all_cause")

## Make a weekly, linearly interpolated population ----
pop_combos <- pop_df %>%
    dplyr::select(-n_pop, -year) %>%
    dplyr::distinct()

interpolated_data <- vector("list", NROW(pop_combos))
for (i in 1:NROW(pop_combos)) {
    age_grp_x <- pop_combos$age_grp[i]
    spec_x <- pop_combos$spec_grp[i]
    img_x <- pop_combos$img_grp[i]
    active_x <- pop_combos$active_grp[i]
    race_x <- pop_combos$race_grp[i]
    
    holder <- NULL
    for (y in 2016:2021) {
        ## Let's assume the denominator is gathered over the course of a year,
        ## so that the final count is actually the end-of-the-year tally.
        ## Then, let's take the deaths for a year, subtract that from the
        ## end of the year tally, to get the death-adjusted denominator.
        start_date <- as.Date(sprintf("%s-01-01", y))
        end_date <- as.Date(sprintf("%s-12-31", y))
        seq_date <- seq(start_date, end_date, by = 1)
        
        if (y > 2016) {
            start_deaths <- monthly_orig %>%
                dplyr::mutate(year = lubridate::year(date_start)) %>%
                dplyr::filter(
                    age_grp == age_grp_x,
                    spec_grp == spec_x,
                    img_grp == img_x,
                    active_grp == active_x,
                    race_grp == race_x,
                    year == y - 1
                ) %>%
                dplyr::pull(n_deaths) %>%
                sum()
        } else {
            start_deaths <- 0
        }
        
        end_deaths <- monthly_orig %>%
            dplyr::mutate(year = lubridate::year(date_start)) %>%
            dplyr::filter(
                age_grp == age_grp_x,
                spec_grp == spec_x,
                img_grp == img_x,
                active_grp == active_x,
                race_grp == race_x,
                year == y
            ) %>%
            dplyr::pull(n_deaths) %>%
            sum()
        
        start_pop <- pop_df %>%
            dplyr::filter(
                age_grp == age_grp_x,
                spec_grp == spec_x,
                img_grp == img_x,
                active_grp == active_x,
                race_grp == race_x,
                year == y - 1
            ) %>%
            dplyr::pull(n_pop) -
            start_deaths
        
        end_pop <-  pop_df %>%
            dplyr::filter(
                age_grp == age_grp_x,
                spec_grp == spec_x,
                img_grp == img_x,
                active_grp == active_x,
                race_grp == race_x,
                year == y
            ) %>%
            dplyr::pull(n_pop) -
            end_deaths
        
        seq_pop <- round(seq(start_pop, end_pop, along.with = seq_date))
        
        holder <- dplyr::bind_rows(
            holder,
            dplyr::tibble(
                active_grp = active_x,
                race_grp = race_x,
                age_grp = age_grp_x,
                spec_grp = spec_x,
                img_grp = img_x,
                date = seq_date,
                n_pop = seq_pop
            )
        )
    }
    
    interpolated_data[[i]] <- holder
}
interpolated_data <- dplyr::bind_rows(interpolated_data)

## Add month markers ----
interpolated_data <- interpolated_data %>%
    dplyr::mutate(month_from_start = (lubridate::year(date) -
                                          lubridate::year(FIRST_MONTH)) *
                      12 + lubridate::month(date))

## Subset to monthly dates ----
monthly_pop <- interpolated_data %>%
    dplyr::filter(date >= FIRST_MONTH,
                  date <= LAST_MONTH) %>%
    dplyr::group_by(active_grp,
                    race_grp,
                    age_grp,
                    spec_grp,
                    img_grp,
                    month_from_start) %>%
    dplyr::summarize(
        date_start = min(date),
        date_end = max(date),
        n_pop = dplyr::first(n_pop)
    ) %>%
    dplyr::ungroup()

## Join weekly tables ----
monthly_analytic <-  dplyr::bind_rows(
    monthly_pop %>%
        dplyr::left_join(
            monthly_orig %>%
                dplyr::filter(death_type == "all_cause") %>%
                dplyr::select(-date_start, -date_end)
        ) %>%
        dplyr::mutate(
            death_type = "all_cause",
            n_deaths = ifelse(is.na(n_deaths), 0, n_deaths)
        ),
    monthly_pop %>%
        dplyr::left_join(
            monthly_orig %>%
                dplyr::filter(death_type == "exclude_covid") %>%
                dplyr::select(-date_start, -date_end)
        ) %>%
        dplyr::mutate(
            death_type = "exclude_covid",
            n_deaths = ifelse(is.na(n_deaths), 0, n_deaths)
        )
)

## Create a population time series ----
pop_df <- monthly_analytic %>%
    dplyr::select(active_grp,
                  age_grp,
                  date = date_end,
                  population = n_pop) %>%
    dplyr::distinct()

## Save ----
saveRDS(monthly_analytic, here::here("data", "analytic_monthly.RDS"))
saveRDS(pop_df, here::here("data", "population_monthly.RDS"))
