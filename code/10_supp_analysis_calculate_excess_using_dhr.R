## 10_supp_analysis_calculate_excess_using_dhr.R ----
## 
## As our reviewers noted, all excess mortality models assume that the 
## baseline (i.e., expected) counterfactual model is correctly specified (or
## nearly correctly specified). This, in practice, is not testable or even
## knowable. However, here, we will use a completely different family of 
## models (time series vs Poisson) to see if our results are sensitive to
## our underlying model assumptions. 

## Imports ----
library(forecast)
library(fable)
library(feasts)
library(future.apply)
library(tsibble)
library(tidyverse)
library(here)
library(config)
library(future)
library(fs)
library(furrr)
source(here::here("code", "utils.R"))

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
LAST_WEEK <- as.Date("2021-12-31")
FORECAST_INTERVAL <- 21
N_BOOT <- 50000
N_CORE <- 15

## Set your max number of cores ----
future::plan(future::multisession(workers = N_CORE))

## Data ----
analytic_df <-
    readRDS(here::here("data", "analytic_monthly.RDS")) %>%
    dplyr::filter(date_start <= LAST_WEEK) %>%
    dplyr::rename(pop = n_pop)

death_df <- analytic_df %>%
    group_by(active_grp, race_grp, age_grp, spec_grp, img_grp, death_type) %>%
    dplyr::filter(mean(n_deaths) > 2) %>%
    dplyr::ungroup() %>%
    rename(date = date_start)

## Training data ----
### Make sure to remove *all* of the holdout (prediction) data ----
training_df <- death_df %>%
    dplyr::filter(date_end < FORECAST_START) %>%
    dplyr::arrange(active_grp,
                   race_grp,
                   age_grp,
                   spec_grp,
                   img_grp,
                   death_type,
                   date) %>%
    dplyr::mutate(yearmonth = tsibble::yearmonth(date)) %>%
    tsibble::tsibble(
        key = c(
            "active_grp",
            "race_grp",
            "age_grp",
            "spec_grp",
            "img_grp",
            "death_type"
        ),
        index = yearmonth
    )

## Create all models on training data ----
if (!fs::file_exists(here::here("data", "supp_dhr_month_all_models.RDS"))) {
    ## fable package has weird memory leak issues (see Github Issue below) so
    ## you should use fewer cores than normal (14 cores results in ~200GB of
    ## memory usage), or you should use a sequential plan and let it run
    ## overnight.
    ##
    ## https://github.com/tidyverts/fabletools/issues/146
    ## https://github.com/tidyverts/fable/issues/230
    
    future::plan(future::multisession(workers = N_CORE))
    # future::plan(sequential)
    
    model_df <- training_df %>%
        fabletools::model(
            k00 = fable::ARIMA(
                n_deaths ~ PDQ(0, 0, 0) + pop,
                stepwise = FALSE,
                approximation = FALSE
            ),
            k01 = k_autoarima(k = 01, p = 12),
            k02 = k_autoarima(k = 02, p = 12),
            k03 = k_autoarima(k = 03, p = 12),
            k04 = k_autoarima(k = 04, p = 12),
            k05 = k_autoarima(k = 05, p = 12),
            k06 = k_autoarima(k = 06, p = 12)
        )
    
    saveRDS(model_df,
            here::here("data", "supp_dhr_month_all_models.RDS"),
            compress = "xz")
} else {
    model_df <-
        readRDS(here::here("data", "supp_dhr_month_all_models.RDS"))
}

## Transform our mable into a tibble with the *best* models only along with
## their fit statistics and autocorrelation tests
if (!fs::file_exists(here::here("data", "supp_dhr_month_best_models.RDS"))) {
  best_models <- fabletools::glance(model_df) %>%
    dplyr::left_join(model_df %>%
                       fabletools::augment(model_df) %>%
                       fabletools::features(.innov, ljung_box, lag = 10, dof = 3)) %>%
    dplyr::group_by(active_grp, race_grp, age_grp, spec_grp, img_grp, death_type) %>%
    dplyr::arrange(active_grp, race_grp, age_grp, spec_grp, img_grp, death_type, AICc) %>% 
    # mutate(model_rank = row_number(AICc)) %>% 
    dplyr::filter(AICc == min(AICc, na.rm = TRUE)) %>%
    dplyr::rename(n_harmonics = .model) %>%
    dplyr::left_join(tidyr::pivot_longer(
        model_df,
      names_to = "n_harmonics",
      values_to = "model",
      cols = c(dplyr::starts_with("k"))
    ))
  
  saveRDS(best_models, here::here("data", "supp_dhr_month_best_models.RDS"), 
          compress = "xz")
} else {
  best_models <- readRDS(here::here("data", "supp_dhr_month_best_models.RDS"))
}

## Set your max number of cores ----
doParallel::stopImplicitCluster()
closeAllConnections()
future::plan(future::multisession(workers = N_CORE))

## Calculate the total excess mortality over the period of interest and
## bootstrapped CIs
param_grid <- best_models %>% 
  dplyr::ungroup() %>% 
  dplyr::select(active_grp, race_grp, age_grp, spec_grp, img_grp, death_type) %>% 
  dplyr::distinct()

if (!fs::file_exists(here::here("data", "supp_dhr_month_excess_metrics.RDS"))) {
  all_excess_df <- furrr::future_map_dfr(
    .x = 1:NROW(param_grid),
    .f = ~ {
      calculate_all_excess_metrics(
        death_df,
        active_x = param_grid$active_grp[.x],
        race_x = param_grid$race_grp[.x],
        age_x = param_grid$age_grp[.x],
        spec_x = param_grid$spec_grp[.x],
        img_x = param_grid$img_grp[.x],
        death_x = param_grid$death_type[.x],
        best_model_df = best_models,
        forecast_start = FORECAST_START,
        n_reps = N_BOOT
      )
    })
  
  saveRDS(all_excess_df,
          here::here("data", "supp_dhr_month_excess_metrics.RDS"),
          compress = "xz")
} else {
  all_excess_df <- readRDS(here::here("data", "supp_dhr_month_excess_metrics.RDS"))
}

## Close up ----
closeAllConnections()
doParallel::stopImplicitCluster()
