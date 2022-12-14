## 07_calculate_excess_using_poisson.R ----
## 
## Using the analytic data set, calculate monthly excess mortality (and
## cumulative excess mortality) using Rolando and Rafa's excessmort package. 
## See their paper for details on the statistical framework. 
## 
## Note that the code is fairly inefficient. I calculate expected, excess,
## and cumulative excess deaths in separate stages but they could be done in
## a single go. That said, the models run quickly and I think doing it 
## stepwise will be more scalable in the future if we do many more groups. 

## Imports ----
library(tidyverse)
library(here)
library(excessmort)
library(future)
library(furrr)
library(fs)

## Bug workaround ----
## See: https://github.com/rstudio/rstudio/issues/6692
## Revert to 'sequential' setup of PSOCK cluster in RStudio Console on macOS and R 4.0.0
if (Sys.getenv("RSTUDIO") == "1" &&
    !nzchar(Sys.getenv("RSTUDIO_TERM")) &&
    Sys.info()["sysname"] == "Darwin" && getRversion() >= "4.0.0") {
    parallel:::setDefaultClusterOptions(setup_strategy = "sequential")
}

## Constants ----
FORECAST_START <- as.Date("2020-03-01")
LAST_MONTH <- as.Date("2021-12-31")
FREQUENCY <- 12
TRAIN_SPLIT <- 70 / 100 ## Model selection is based on 30% of training data
N_HARMONICS <- 1:4
N_TREND_KNOTS <- (0:2) / 5
PREDICTION_WINDOW <- 12
N_CORE <- 15
    
## Set your max number of cores ----
future::plan(future::multisession(workers = N_CORE))

## Data ----
analytic_df <-
    readRDS(here::here("data", "analytic_monthly.RDS")) %>%
    dplyr::filter(date_start <= LAST_MONTH) %>%
    dplyr::rename(pop = n_pop)

## Training data ----
### Make sure to remove *all* of the holdout (prediction) data ----
training_df <- dplyr::filter(analytic_df, date_end < FORECAST_START)

## Get a vector of test dates we will for out of sample forecasting errors
all_dates <- sort(unique(training_df$date_start))
end_training <- seq.Date(all_dates[round(NROW(all_dates) * TRAIN_SPLIT)],
                         max(all_dates) - PREDICTION_WINDOW * 7,
                         by = "1 month")

### Create a search grid of all models we'll need to run ----
model_grid <- expand.grid(
    age_grp = unique(analytic_df$age_grp),
    img_grp = unique(analytic_df$img_grp),
    spec_grp = unique(analytic_df$spec_grp),
    race_grp = unique(analytic_df$race_grp),
    active_grp = unique(analytic_df$active_grp),
    death_type = unique(analytic_df$death_type),
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
        active_grp,
        age_grp,
        race_grp,
        img_grp,
        spec_grp,
        training_start,
        training_end
    ) %>%
    dplyr::mutate(forecast_end = training_end + PREDICTION_WINDOW * 7) %>%
    dplyr::as_tibble()

## Subset the search grid to just combinations of data that exists
model_grid <- model_grid %>%
    left_join(
        analytic_df %>%
            select(active_grp, race_grp, age_grp, spec_grp, img_grp, death_type) %>%
            distinct() %>%
            mutate(keep = 1)
    ) %>%
    filter(keep == 1) %>%
    select(-keep)

### Fit the training data on a rolling forecasting origin ----
## For all outcomes with at least 2 deaths per week
if (!file.exists(here::here("data", "model_forecasting_errors_monthly.RDS"))) {
    forecast_errors <- furrr::future_map_dfr(
        .x = 1:NROW(model_grid),
        .f = ~ {
            age_grp_x <- model_grid$age_grp[.x]
            death_x <- model_grid$death_type[.x]
            img_x <- model_grid$img_grp[.x]
            spec_x <- model_grid$spec_grp[.x]
            race_x <- model_grid$race_grp[.x]
            active_x <- model_grid$active_grp[.x]
            n_harmonics <- model_grid$n_harmonic[.x]
            n_knots <- model_grid$n_knot[.x]
            date_end  <- model_grid$forecast_end[.x]
            train_end <- model_grid$training_end[.x]
            train_start <- model_grid$training_start[.x]
            
            current_training_df <-
                training_df %>%
                dplyr::filter(
                    age_grp == age_grp_x,
                    img_grp == img_x,
                    spec_grp == spec_x,
                    race_grp == race_x,
                    active_grp == active_x,
                    death_type == death_x,
                    date_end <= date_end
                ) %>%
                dplyr::transmute(date = date_start,
                                 outcome = n_deaths,
                                 population = pop)
            
            exclude_date <-
                current_training_df %>%
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
                        img_grp = img_x,
                        spec_grp = spec_x,
                        race_grp = race_x,
                        active_grp = active_x,
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
        })
    saveRDS(
        forecast_errors,
        here::here("data", "model_forecasting_errors_monthly.RDS"),
        compress = "xz"
    )
} else {
    forecast_errors <-
        readRDS(here::here("data", "model_forecasting_errors_monthly.RDS"))
}

### Summarize the out of sample errors ----
if (!file.exists(here::here("data", "model_error_summary_monthly.RDS"))) {
    error_summary <- forecast_errors %>%
        dplyr::mutate(
            error = predicted - observed,
            abs_error = abs(predicted - observed),
            sq_error = (predicted - observed) ^ 2,
            abs_perc_error = abs(predicted - observed) / observed * 100
        ) %>%
        dplyr::group_by(active_grp,
                        death_type,
                        race_grp,
                        age_grp,
                        img_grp,
                        spec_grp,
                        n_harmonic,
                        n_knot) %>%
        dplyr::summarize(
            n_predictions = sum(!is.na(predicted)),
            mae = mean(abs_error, na.rm = TRUE),
            mse = mean(sq_error, na.rm = TRUE),
            rmse = sqrt(mean(sq_error, na.rm = TRUE)),
            mape = mean(abs_perc_error, na.rm = TRUE)
        ) %>%
        dplyr::group_by(active_grp,
                        death_type,
                        race_grp,
                        age_grp,
                        img_grp,
                        spec_grp) %>%
        dplyr::mutate(model_rank = dplyr::row_number(mse)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(n_predictions > 0) %>%
        dplyr::arrange(active_grp,
                       death_type,
                       race_grp,
                       age_grp,
                       img_grp,
                       spec_grp,
                       model_rank)
    
    saveRDS(error_summary,
            here::here("data", "model_error_summary_monthly.RDS"))
} else {
    error_summary <-
        readRDS(here::here("data", "model_error_summary_monthly.RDS"))
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

if (!fs::file_exists(here::here("data", "expected_deaths_monthly.RDS"))) {
    expected_deaths <- furrr::future_map_dfr(
        .x = 1:NROW(best_models),
        .f = ~ {
            age_grp_x <- best_models$age_grp[.x]
            death_x <- best_models$death_type[.x]
            img_x <- best_models$img_grp[.x]
            spec_x <- best_models$spec_grp[.x]
            race_x <- best_models$race_grp[.x]
            active_x <- best_models$active_grp[.x]
            n_harmonics <- best_models$n_harmonic[.x]
            n_knots <- best_models$n_knot[.x]
            date_end <- best_models$forecast_end[.x]
            train_end <- best_models$training_end[.x]
            train_start <- best_models$training_start[.x]
            
            train_data <-
                analytic_df %>%
                dplyr::filter(
                    age_grp == age_grp_x,
                    img_grp == img_x,
                    spec_grp == spec_x,
                    race_grp == race_x,
                    active_grp == active_x,
                    death_type == death_x
                ) %>%
                dplyr::transmute(date = date_end,
                                 outcome = n_deaths,
                                 population = pop)
            
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
                    img_grp = img_x,
                    spec_grp = spec_x,
                    race_grp = race_x,
                    active_grp = active_x,
                    lower_expected = exp(log(expected) - 1.96 * log_expected_se),
                    upper_expected = exp(log(expected) + 1.96 * log_expected_se)
                )
        })
    saveRDS(expected_deaths,
            here::here("data", "expected_deaths_monthly.RDS"))
} else {
    expected_deaths <-
        readRDS(here::here("data", "expected_deaths_monthly.RDS"))
}

## Calculate excess mortality over time for each group ----
if (!fs::file_exists(here::here("data", "excess_deaths_monthly.RDS"))) {
    excess_deaths <- furrr::future_map_dfr(
        .x = 1:NROW(best_models),
        .f = ~ {
            age_grp_x <- best_models$age_grp[.x]
            death_x <- best_models$death_type[.x]
            img_x <- best_models$img_grp[.x]
            spec_x <- best_models$spec_grp[.x]
            race_x <- best_models$race_grp[.x]
            active_x <- best_models$active_grp[.x]
            n_harmonics <- best_models$n_harmonic[.x]
            n_knots <- best_models$n_knot[.x]
            date_end  <- best_models$forecast_end[.x]
            train_end <- best_models$training_end[.x]
            train_start <- best_models$training_start[.x]
            
            train_data <-
                analytic_df %>%
                dplyr::filter(
                    age_grp == age_grp_x,
                    img_grp == img_x,
                    spec_grp == spec_x,
                    race_grp == race_x,
                    active_grp == active_x,
                    death_type == death_x
                ) %>%
                dplyr::transmute(date = date_end,
                                 outcome = n_deaths,
                                 population = pop)
            
            excess_mort <-
                excessmort::excess_model(
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
                    knots.per.year = 4
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
                        (expected ^ 2 * log_expected_se ^ 2) * se ^ 2 +
                            (expected ^ 2 * log_expected_se ^ 2) * fitted ^ 2 +
                            se ^ 2 * expected ^ 2
                    ),
                    abs_change_lower = abs_change - 1.96 * abs_change_se,
                    abs_change_upper = abs_change + 1.96 * abs_change_se
                ) %>%
                mutate(
                    rel_change = (observed - expected) / expected,
                    rel_change_lower = fitted - 1.96 * se,
                    rel_change_upper = fitted + 1.96 * se
                ) %>%
                dplyr::mutate(
                    death_type = death_x,
                    age_grp = age_grp_x,
                    img_grp = img_x,
                    spec_grp = spec_x,
                    race_grp = race_x,
                    active_grp = active_x,
                    lower_expected = exp(log(expected) - 1.96 * log_expected_se),
                    upper_expected = exp(log(expected) + 1.96 * log_expected_se)
                )
            
        })
    saveRDS(excess_deaths,
            here::here("data", "excess_deaths_monthly.RDS"))
} else {
    excess_deaths <-
        readRDS(here::here("data", "excess_deaths_monthly.RDS"))
}

## Calculate cumulative excess mortality for each group ----
if (!fs::file_exists(here::here("data", "cume_excess_deaths_monthly.RDS"))) {
    cume_excess_deaths <- furrr::future_map_dfr(
        .x = 1:NROW(best_models),
        .f = ~ {
            age_grp_x <- best_models$age_grp[.x]
            death_x <- best_models$death_type[.x]
            img_x <- best_models$img_grp[.x]
            spec_x <- best_models$spec_grp[.x]
            race_x <- best_models$race_grp[.x]
            active_x <- best_models$active_grp[.x]
            n_harmonics <- best_models$n_harmonic[.x]
            n_knots <- best_models$n_knot[.x]
            date_end  <- best_models$forecast_end[.x]
            train_end <- best_models$training_end[.x]
            train_start <- best_models$training_start[.x]
            
            train_data <-
                analytic_df %>%
                dplyr::filter(
                    age_grp == age_grp_x,
                    img_grp == img_x,
                    spec_grp == spec_x,
                    race_grp == race_x,
                    active_grp == active_x,
                    death_type == death_x
                ) %>%
                dplyr::transmute(date = date_end,
                                 outcome = n_deaths,
                                 population = pop)
            
            excess_mort <-
                excessmort::excess_model(
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
                    knots.per.year = 4
                )
            
            excessmort::excess_cumulative(excess_mort,
                                          start = FORECAST_START,
                                          end = LAST_MONTH) %>%
                dplyr::mutate(
                    death_type = death_x,
                    age_grp = age_grp_x,
                    img_grp = img_x,
                    spec_grp = spec_x,
                    race_grp = race_x,
                    active_grp = active_x,
                    upper = fitted + 1.96 * se,
                    lower = fitted - 1.96 * se
                )
        })
    saveRDS(cume_excess_deaths,
            here::here("data", "cume_excess_deaths_monthly.RDS"))
}

## Close connections or RStudio crashes on restart ----
closeAllConnections()
