## 08_calculate_genpop_excess.R ----
## 
## Run the same analysis for the US general population. Files come from
## the US Census Bureau population estimates program (denominators) or from
## the NCHS provisional mortality (weekly) data. 

## Imports ----
library(tidyverse)
library(here)
library(fs)
library(janitor)
library(future)
library(parallel)
library(furrr)

## Constants ----
FORECAST_START <- as.Date("2020-03-01")
LAST_WEEK <- as.Date("2021-12-31")
FREQUENCY <- 52
TRAIN_SPLIT <- 70 / 100 ## Model selection is based on last 30% of TRAINING data
N_HARMONICS <- 1:4
N_TREND_KNOTS <- (0:2) / 5
PREDICTION_WINDOW <- as.integer(difftime(LAST_WEEK, FORECAST_START, units = "weeks") * 7 / 12)
N_CORE <- 15

## Bug workaround ----
## See: https://github.com/rstudio/rstudio/issues/6692
## Revert to 'sequential' setup of PSOCK cluster in RStudio Console on macOS and R 4.0.0
if (Sys.getenv("RSTUDIO") == "1" &&
    !nzchar(Sys.getenv("RSTUDIO_TERM")) &&
    Sys.info()["sysname"] == "Darwin" && getRversion() >= "4.0.0") {
    parallel:::setDefaultClusterOptions(setup_strategy = "sequential")
}

## Set your max number of cores ----
future::plan(future::multisession(workers = N_CORE))

## Create analytic dataframe for general US population ----
if (!fs::file_exists(here::here("data", "supp_analytic_weekly_usgenpop.RDS"))) {
    
    ### US CB URLs ----
    ## Note: sex {0, 1, 2} corresponds to {total, male, female} and
    ## age {999} corresponds to {total}.
    url_2021 <-
        paste0(
            "https://www2.census.gov/programs-surveys/popest/",
            "datasets/2020-2021/national/asrh/",
            "nc-est2021-agesex-res.csv"
        )
    url_2010 <-
        paste0(
            "https://www2.census.gov/programs-surveys/popest/",
            "datasets/2010-2020/national/asrh/",
            "nc-est2020-agesex-res.csv"
        )
    
    if (!fs::file_exists(here::here("data_raw", basename(url_2021)))) {
        utils::download.file(url_2021, here::here("data_raw", basename(url_2021)))
    }
    if (!fs::file_exists(here::here("data_raw", basename(url_2010)))) {
        utils::download.file(url_2010, here::here("data_raw", basename(url_2010)))
    }
    
    ## Data ----
    deaths_orig <- readr::read_csv(here::here(
        "data_raw",
        "Weekly_Counts_of_Deaths_by_Jurisdiction_and_Age.csv"
    )) %>%
        janitor::clean_names()
    pop_2010 <- readr::read_csv(here::here("data_raw", basename(url_2010))) %>%
        janitor::clean_names()
    pop_2021 <- readr::read_csv(here::here("data_raw", basename(url_2021))) %>%
        janitor::clean_names()
    
    ## Cleaning death file ----
    death_df <- deaths_orig %>%
        dplyr::filter(state_abbreviation == "US",
               type == "Unweighted") %>%
        dplyr::mutate(
            date_end = lubridate::mdy(week_ending_date),
            date_start = lubridate::mdy(week_ending_date) - lubridate::days(6)
        ) %>%
        dplyr::filter(date_start <= LAST_WEEK) %>%
        dplyr::select(date_start,
               date_end,
               year,
               week,
               age_group,
               n_deaths = number_of_deaths) %>%
        dplyr::mutate(
            age_grp = dplyr::case_when(
                age_group == "Under 25 years" ~ "under25",
                age_group == "25-44 years" ~ "25to44",
                age_group == "45-64 years" ~ "45to64",
                age_group == "65-74 years" ~ "65to74",
                age_group == "75-84 years" ~ "75to84",
                age_group == "85 years and older" ~ "85over"
            )
        )
    
    ## Cleaning the pop file ----
    pop_df <- pop_2010 %>%
        dplyr::select(-popestimate2020) %>%
        dplyr::left_join(pop_2021) %>%
        dplyr::filter(sex == 0, age < 999) %>%
        dplyr::select(sex, age, dplyr::starts_with("popestimate")) %>%
        tidyr::pivot_longer(
            cols = dplyr::starts_with("popestimate"),
            names_to = "year",
            values_to = "n_pop"
        ) %>%
        dplyr::mutate(year = as.numeric(gsub("popestimate", "", year)))
    
    pop_df <- pop_df %>%
        dplyr::mutate(
            age_grp = dplyr::case_when(
                dplyr::between(age,  0, 24) ~ "under25",
                dplyr::between(age, 25, 44) ~ "25to44",
                dplyr::between(age, 45, 64) ~ "45to64",
                dplyr::between(age, 65, 74) ~ "65to74",
                dplyr::between(age, 75, 84) ~ "75to84",
                dplyr::between(age, 85, 150) ~ "85over"
            )
        ) %>%
        dplyr::group_by(year, age_grp) %>%
        dplyr::summarize(n_pop = sum(n_pop))
    
    ## Extrapolate out to 2022 ----
    pop_df <- dplyr::bind_rows(
        pop_df,
        pop_df %>%
            dplyr::filter(year %in% 2020:2021) %>%
            dplyr::group_by(age_grp) %>%
            dplyr::mutate(pop_change = n_pop[year == 2021] / n_pop [year == 2020]) %>%
            dplyr::arrange(age_grp, year) %>%
            dplyr::mutate(pop_2022 = round(n_pop * pop_change ^ (
                as.numeric(difftime(
                    as.Date("2021-07-01"), as.Date("2022-07-01")
                )) / 365
            ))) %>%
            dplyr::filter(year == max(year)) %>%
            dplyr::mutate(year = 2022,
                          n_pop = pop_2022) %>%
            dplyr::select(-pop_2022, -pop_change)
    ) %>%
        dplyr::arrange(age_grp, year) %>%
        dplyr::ungroup()
    
    ## Make a weekly, linearly interpolated population ----
    pop_combos <- pop_df %>%
        dplyr::select(-n_pop, -year) %>%
        dplyr::distinct()
    
    interpolated_data <- vector("list", NROW(pop_combos))
    for (i in 1:NROW(pop_combos)) {
        age_grp_x <- pop_combos$age_grp[i]
        
        holder <- NULL
        for (y in 2014:2021) {
            ## Let's assume the denominator is gathered over the course of a year,
            ## so that the final count is actually the end-of-the-year tally.
            ## Then, let's take the deaths for a year, subtract that from the
            ## end of the year tally, to get the death-adjusted denominator.
            start_date <- as.Date(sprintf("%s-07-01", y))
            end_date <- as.Date(sprintf("%s-07-01", y + 1))
            seq_date <- seq(start_date, end_date - 1, by = 1)
            
            start_pop <- pop_df %>%
                dplyr::filter(age_grp == age_grp_x,
                              year == y) %>%
                dplyr::pull(n_pop)
            
            end_pop <-  pop_df %>%
                dplyr::filter(age_grp == age_grp_x,
                              year == y + 1) %>%
                dplyr::pull(n_pop)
            
            seq_pop <-
                round(seq(start_pop, end_pop, along.with = seq_date))
            
            holder <- dplyr::bind_rows(
                holder,
                dplyr::tibble(
                    age_grp = age_grp_x,
                    date = seq_date,
                    n_pop = seq_pop
                )
            )
        }
        
        interpolated_data[[i]] <- holder
    }
    interpolated_data <- dplyr::bind_rows(interpolated_data)
    
    ## Create analytic dataframe ----
    analytic_df <- death_df %>%
        dplyr::left_join(interpolated_data,
                  by = c("age_grp", "date_end" = "date")) %>% 
        dplyr::mutate(death_type = "all_cause") %>% 
        dplyr::rename(pop = n_pop)
    
    ## Create a 45 to 84 year old group ----
    analytic_df <- dplyr::bind_rows(
        analytic_df,
        analytic_df %>%
            dplyr::filter(age_grp %in% c("45to64", "65to74", "75to84")) %>%
            dplyr::mutate(age_grp = "45to84",
                   age_group = "45-84 years") %>%
            dplyr::group_by(date_start, date_end, year, week, age_group, age_grp, death_type) %>%
            dplyr::summarize(n_deaths = sum(n_deaths),
                      pop = sum(pop)) %>%
            dplyr::ungroup()
    )
    
    ## Save analytic dataframe ----
    saveRDS(analytic_df,
            here::here("data", "supp_analytic_weekly_usgenpop.RDS"))
} else {
    analytic_df <- readRDS(here::here("data", "supp_analytic_weekly_usgenpop.RDS"))
}

## Training data ----
### Make sure to remove *all* of the holdout (prediction) data ----
training_df <- dplyr::filter(analytic_df, date_end < FORECAST_START)

## Get a vector of test dates we will for out of sample forecasting errors
all_dates <- sort(unique(training_df$date_start))
end_training <- seq.Date(
    all_dates[round(NROW(all_dates) * TRAIN_SPLIT)],
    max(all_dates) - PREDICTION_WINDOW * 7,
    by = "1 week"
)

### Create a search grid of all models we'll need to run ----
model_grid <- expand.grid(
    death_type = unique(analytic_df$death_type),
    age_grp = unique(analytic_df$age_grp),
    n_harmonic = N_HARMONICS,
    n_knot = N_TREND_KNOTS,
    training_start = min(all_dates),
    training_end = end_training,
    stringsAsFactors = FALSE
) %>%
    dplyr::arrange(
        death_type, 
        n_harmonic,
        n_knot,
        age_grp,
        training_start,
        training_end
    ) %>%
    dplyr::mutate(forecast_end = training_end + PREDICTION_WINDOW * 7) %>%
    dplyr::as_tibble()

### Fit the training data on a rolling forecasting origin ----
## For all outcomes with at least 2 deaths per week
if (!file.exists(here::here("data", "supp_model_forecasting_errors_usgenpop.RDS"))) {
    forecast_errors <- furrr::future_map_dfr(
        .x = 1:NROW(model_grid),
        .f = ~ {
            age_grp_x <- model_grid$age_grp[.x]
            death_x <- model_grid$death_type[.x]
            n_harmonics <- model_grid$n_harmonic[.x]
            n_knots <- model_grid$n_knot[.x]
            date_end  <- model_grid$forecast_end[.x]
            train_end <- model_grid$training_end[.x]
            train_start <- model_grid$training_start[.x]
            
            current_training_df <- training_df %>% 
                dplyr::filter(age_grp == age_grp_x,
                              death_type == death_x, 
                              date_end <= date_end) %>% 
                dplyr::transmute(date = date_start,
                                 outcome = n_deaths,
                                 population = pop)
            
            exclude_date <- current_training_df %>% 
                dplyr::filter(date > train_end) %>% 
                dplyr::pull(date)
            
            if (mean(current_training_df$outcome) >= 2) {
                current_model <- excessmort::compute_expected(
                    current_training_df,
                    harmonics = n_harmonics,
                    trend.knots.per.year = n_knots,
                    exclude = exclude_date,
                    frequency = FREQUENCY,
                    verbose = FALSE
                ) 
                
                current_model %>%
                    dplyr::as_tibble() %>%
                    dplyr::select(date, 
                                  observed = outcome, 
                                  predicted = expected, 
                                  excluded) %>%
                    ## Filter out observations used for fitting
                    dplyr::filter(excluded) %>%
                    dplyr::mutate(
                        model_grid_ix = .x,
                        death_type = death_x,
                        age_grp = age_grp_x,
                        n_harmonic = n_harmonics,
                        n_knot = n_knots,
                        train_start = train_start,
                        train_end = train_end,
                        forecast_start = min(exclude_date),
                        forecast_end = max(exclude_date),
                        n_train_dates = sum(!current_model$excluded, na.rm = TRUE),
                        n_test_dates = sum(current_model$excluded, na.rm = TRUE)
                    ) %>%
                    dplyr::select(-excluded)
            }
        }
    )
    saveRDS(forecast_errors, 
            here::here("data", "supp_model_forecasting_errors_usgenpop.RDS"), 
            compress = "xz")
} else {
    forecast_errors <- readRDS(here::here("data", "supp_model_forecasting_errors_usgenpop.RDS"))
}

### Summarize the out of sample errors ----
if (!file.exists( here::here("data", "supp_model_error_summary_usgenpop.RDS"))) {
    error_summary <- forecast_errors %>%
        dplyr::mutate(
            error = predicted - observed,
            abs_error = abs(predicted - observed),
            sq_error = (predicted - observed) ^ 2,
            abs_perc_error = abs(predicted - observed) / observed * 100
        ) %>%
        dplyr::group_by(death_type,
                        age_grp,
                        n_harmonic,
                        n_knot) %>%
        dplyr::summarize(
            n_predictions = sum(!is.na(predicted)),
            mae = mean(abs_error, na.rm = TRUE),
            mse = mean(sq_error, na.rm = TRUE),
            rmse = sqrt(mean(sq_error, na.rm = TRUE)),
            mape = mean(abs_perc_error, na.rm = TRUE)
        ) %>%
        dplyr::group_by(death_type, age_grp) %>%
        dplyr::mutate(model_rank = dplyr::row_number(mse)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(n_predictions > 0) %>%
        dplyr::arrange(death_type,
                       age_grp,
                       model_rank)
    
    saveRDS(error_summary, here::here("data", "supp_model_error_summary_usgenpop.RDS"))
} else {
    error_summary <- readRDS(here::here("data", "supp_model_error_summary_usgenpop.RDS"))
}

## Fit counterfactual models based on lowest out of sample errors ----
## Now take the best (in terms of out of sample prediction) for each
## race/outcome combination and fit it to the full training data and predict
## out to our holdout set.
best_models <- error_summary %>% 
    dplyr::filter(model_rank == 1)

## Need a vector of datas to *NOT* train the model on
exclude_dates <- analytic_df %>% 
    dplyr::filter(date_end >= FORECAST_START) %>% 
    dplyr::pull(date_end) %>% 
    unique()

if (!fs::file_exists(here::here("data", "supp_expected_deaths_usgenpop.RDS"))) {
    expected_deaths <- furrr::future_map_dfr(
        .x = 1:NROW(best_models),
        .f = ~ {
            age_grp_x <- best_models$age_grp[.x]
            death_x <- best_models$death_type[.x]
            n_harmonics <- best_models$n_harmonic[.x]
            n_knots <- best_models$n_knot[.x]
            date_end  <- best_models$forecast_end[.x]
            train_end <- best_models$training_end[.x]
            train_start <- best_models$training_start[.x]
            
            train_data <- analytic_df %>% 
                dplyr::filter(age_grp == age_grp_x,
                              death_type == death_x) %>% 
                dplyr::transmute(date = date_end, outcome = n_deaths, population = pop)
            
            excessmort::compute_expected(
                train_data,
                harmonics = n_harmonics, 
                trend.knots.per.year = n_knots, 
                exclude = exclude_dates, 
                frequency = FREQUENCY
            )  %>%
                dplyr::as_tibble() %>% 
                dplyr::mutate(
                    death_type = death_x,
                    age_grp = age_grp_x,
                    lower_expected = exp(log(expected) - 1.96 * log_expected_se),
                    upper_expected = exp(log(expected) + 1.96 * log_expected_se)
                ) 
        }
    ) 
    saveRDS(expected_deaths, here::here("data", "supp_expected_deaths_usgenpop.RDS"))
} else {
    expected_deaths <- readRDS(here::here("data", "supp_expected_deaths_usgenpop.RDS"))
}

## Calculate excess mortality over time for each group ----
if (!fs::file_exists(here::here("data", "supp_excess_deaths_usgenpop.RDS"))) {
    excess_deaths <- furrr::future_map_dfr(
        .x = 1:NROW(best_models),
        .f = ~ {
            age_grp_x <- best_models$age_grp[.x]
            death_x <- best_models$death_type[.x]
            n_harmonics <- best_models$n_harmonic[.x]
            n_knots <- best_models$n_knot[.x]
            date_end  <- best_models$forecast_end[.x]
            train_end <- best_models$training_end[.x]
            train_start <- best_models$training_start[.x]
            
            train_data <- analytic_df %>% 
                dplyr::filter(age_grp == age_grp_x,
                              death_type == death_x) %>% 
                dplyr::transmute(date = date_end, outcome = n_deaths, population = pop)
            
            excess_mort <- excessmort::excess_model(
                train_data,
                # start = as.Date("2020-01-01"), 
                start = min(exclude_dates),
                end = max(exclude_dates),
                exclude = exclude_dates,
                model = "quasipoisson",
                trend.knots.per.year = n_knots,
                harmonics = n_harmonics,
                discontinuity = FALSE, 
                frequency = FREQUENCY,
                knots.per.year = 6
            )
            
            dplyr::tibble(
                date = excess_mort$date,
                observed = excess_mort$observed,
                expected = excess_mort$expected,
                log_expected_se = excess_mort$log_expected_se,
                fitted = excess_mort$fitted,
                se = excess_mort$se,
                sd = excess_mort$sd
            ) %>%
                dplyr::mutate(
                    ## Note that abs_change is the *smoothed* version of obs
                    ## vs fitted. Therefore, abs_change_se is the standard
                    ## error assuming the two are independent. 
                    abs_change = expected * fitted,
                    abs_change_se = sqrt(
                        (expected^2 * log_expected_se^2) * se^2 + 
                            (expected^2 * log_expected_se^2) * fitted^2 + 
                            se^2 * expected^2
                    ),
                    abs_change_lower = abs_change - 1.96 * abs_change_se,
                    abs_change_upper = abs_change + 1.96 * abs_change_se
                ) %>%
                dplyr::mutate(
                    rel_change = (observed - expected)/expected,
                    rel_change_lower = fitted - 1.96 * se,
                    rel_change_upper = fitted + 1.96 * se
                ) %>% 
                dplyr::mutate(
                    death_type = death_x,
                    age_grp = age_grp_x,
                    lower_expected = exp(log(expected) - 1.96 * log_expected_se),
                    upper_expected = exp(log(expected) + 1.96 * log_expected_se)
                )  
            
        }
    )
    saveRDS(excess_deaths, here::here("data", "supp_excess_deaths_usgenpop.RDS"))
} else {
    excess_deaths <- readRDS(here::here("data", "supp_excess_deaths_usgenpop.RDS"))
}

## Calculate cumulative excess mortality for each group ----
if (!fs::file_exists(here::here("data", "supp_cume_excess_deaths_usgenpop.RDS"))) {
    cume_excess_deaths <- furrr::future_map_dfr(
        .x = 1:NROW(best_models),
        .f = ~ {
            age_grp_x <- best_models$age_grp[.x]
            death_x <- best_models$death_type[.x]
            n_harmonics <- best_models$n_harmonic[.x]
            n_knots <- best_models$n_knot[.x]
            date_end  <- best_models$forecast_end[.x]
            train_end <- best_models$training_end[.x]
            train_start <- best_models$training_start[.x]
            
            train_data <- analytic_df %>% 
                dplyr::filter(age_grp == age_grp_x,
                              death_type == death_x) %>% 
                dplyr::transmute(date = date_end, outcome = n_deaths, population = pop)
            
            excess_mort <- excessmort::excess_model(
                train_data,
                # start = as.Date("2020-01-01"), 
                start = min(exclude_dates),
                end = max(exclude_dates),
                exclude = exclude_dates,
                model = "quasipoisson",
                trend.knots.per.year = n_knots,
                harmonics = n_harmonics,
                discontinuity = FALSE, 
                frequency = FREQUENCY,
                knots.per.year = 6
            )
            
            excessmort::excess_cumulative(excess_mort, 
                                          start = FORECAST_START,
                                          # start = as.Date("2020-01-01"),
                                          end = LAST_WEEK) %>% 
                dplyr::mutate(
                    death_type = death_x,
                    age_grp = age_grp_x,
                    upper = fitted + 1.96 * se,
                    lower = fitted - 1.96 * se
                )
        }
    )
    saveRDS(cume_excess_deaths, here::here("data", "supp_cume_excess_deaths_usgenpop.RDS"))
}

## Close connections or RStudio crashes on restart ----
closeAllConnections()
doParallel::stopImplicitCluster()
