## Imports ----
library(forecast)
library(fable)
library(feasts)
library(future.apply)
library(tidyverse)

## Helper functions ----
create_age_grp <- function(df) {
    df %>%
        dplyr::mutate(
            age_grp = dplyr::case_when(
                dplyr::between(age, 0, 44) ~ "under45",
                dplyr::between(age, 45, 64) ~ "45to64",
                dplyr::between(age, 65, 74) ~ "65to74",
                dplyr::between(age, 75, 84) ~ "75to84",
                dplyr::between(age, 85, 150) ~ "85andup",
                TRUE ~ NA_character_
            )
        )
}

categorize_age <- function(df) {
    df %>%
        dplyr::mutate(age_cat = factor(
            age_grp,
            levels = c(
                "all",
                "under45",
                "45to64",
                "65to74",
                "75to84",
                "45to84",
                "85andup"
            ),
            labels = c(
                "All ages",
                "25 to 44",
                "45 to 64",
                "65 to 74",
                "75 to 84",
                "All (45 to 84)",
                "85 and over"
            ),
            ordered = TRUE
        )) %>%
        dplyr::mutate(age_cat_rev = factor(
            age_grp,
            levels = rev(
                c(
                    "all",
                    "under45",
                    "45to64",
                    "65to74",
                    "75to84",
                    "45to84",
                    "85andup"
                )
            ),
            labels = rev(
                c(
                    "All ages",
                    "25 to 44",
                    "45 to 64",
                    "65 to 74",
                    "75 to 84",
                    "All (45 to 84)",
                    "85 and over"
                )
            ),
            ordered = TRUE
        ))
}

create_active_grp <- function(df) {
    df %>%
        dplyr::mutate(
            active_grp = dplyr::case_when(
                top_desc %in% c(
                    "Retired",
                    "Temporarily Not In Practice",
                    "Not Active For Other Reasons",
                    "Semi-Retired"
                ) ~ "not_active",
                top_desc %in% c(
                    "Direct Patient Care",
                    "Medical Research",
                    "Administration",
                    "Unclassified",
                    "Non-Patient Care",
                    "Medical Teaching",
                    "Resident"
                ) ~ "active",
                TRUE ~ NA_character_
            )
        )
}

categorize_active <- function(df) {
    df %>%
        dplyr::mutate(active_cat = factor(
            active_grp,
            levels = c(
                "all",
                "active",
                "direct_pt_care",
                "not_direct_pt_care",
                "not_active",
                "genpop"
            ),
            labels = c(
                "All physicians",
                "All actively practicing",
                "Active physician, provides\ndirect patient care",
                "Active physician, does not\nprovide direct patient care",
                "Non-active physician",
                "US general population"
            ),
            ordered = TRUE
        )) %>%
        dplyr::mutate(active_cat_rev = factor(
            active_grp,
            levels = rev(
                c(
                    "all",
                    "active",
                    "direct_pt_care",
                    "not_direct_pt_care",
                    "not_active",
                    "genpop"
                )
            ),
            labels = rev(
                c(
                    "All physicians",
                    "All actively practicing",
                    "Active, provides direct patient care",
                    "Active, does not provide direct patient care",
                    "Not actively practicing",
                    "US general population"
                )
            ),
            ordered = TRUE
        ))
}


create_race_grp <- function(df) {
    df %>%
        dplyr::mutate(
            race_grp = dplyr::case_when(
                race_ethnicity == "Unknown" ~ "other_unknown",
                race_ethnicity == "White" ~ "white",
                race_ethnicity == "Asian" ~ "asian",
                race_ethnicity == "Black or African American" ~ "black",
                race_ethnicity == "Hispanic, Latino, or Spanish Origin" ~ "hispanic",
                race_ethnicity == "Other" ~ "other_unknown",
                race_ethnicity == "American Indian or Alaskan Native" ~ "other_unknown",
                race_ethnicity == "Mixed Race/Ethnicity" ~ "other_unknown",
                TRUE ~ "other_unknown"
            )
        )
}

categorize_race <- function(df) {
    df %>%
        dplyr::mutate(race_cat = factor(
            race_grp,
            levels = c(
                "all",
                "white",
                "black",
                "asian",
                "hispanic",
                "other_unknown"
            ),
            labels = c(
                "All race/ethnicity",
                "White",
                "Black",
                "Asian",
                "Hispanic",
                "Other or unknown"
            ),
            ordered = TRUE
        ))
}

create_img_grp <- function(df) {
    df %>%
        dplyr::mutate(img_grp = dplyr::case_when(
            img == 0 ~ "us_med_grad",
            img == 1 ~ "intl_med_grad",
            TRUE ~ NA_character_
        ))
}

categorize_img <- function(df) {
    df %>%
        dplyr::mutate(img_cat = factor(
            img_grp,
            levels = c("all", "us_med_grad", "intl_med_grad"),
            labels = c("All", "US grad", "Int'l grad"),
            ordered = TRUE
        ))
}

categorize_death <- function(df) {
    df %>%
        dplyr::mutate(death_cat = factor(
            death_type,
            levels = c("all_cause", "exclude_covid", "usgenpop"),
            labels = c("All causes", "Excluding COVID-19", "US Population"),
            ordered = TRUE
        ))
}

categorize_all <- function(df) {
    df %>%
        categorize_spec() %>%
        categorize_img() %>%
        categorize_race() %>%
        categorize_age() %>%
        categorize_active() %>%
        categorize_death()
}

## DHR / ARIMA helpers ----
k_autoarima <- function(k, p = 365.25 / 7) {
    ## Note default period is weekly so change accordingly
    fable::ARIMA(
        n_deaths ~ fourier(K =  k, period = p) + PDQ(0, 0, 0) + pop,
        stepwise = FALSE,
        approximation = FALSE
    )
}

calculate_all_excess_metrics <- function(
        death_df,
        active_x,
        race_x,
        age_x,
        spec_x,
        img_x,
        death_x,
        best_model_df,
        forecast_start = FORECAST_START,
        n_reps = N_BOOT
        ) {
    ## Get observed total number of deaths in our interval of interest
    sub_death <- death_df %>%
        dplyr::filter(
            active_grp == active_x,
            race_grp == race_x,
            age_grp == age_x,
            spec_grp == spec_x,
            img_grp == img_x,
            death_type == death_x
        )
    
    sub_model <- best_model_df %>%
        dplyr::ungroup() %>%
        dplyr::filter(
            active_grp == active_x,
            race_grp == race_x,
            age_grp == age_x,
            spec_grp == spec_x,
            img_grp == img_x,
            death_type == death_x
        )
    
    ## Pull out the model used for forecasting
    target_model <- sub_model %>%
        dplyr::pull(model)
    target_model <- target_model[[1]]
    
    ## Need population in a tsibble for projection
    new_dat <- death_df %>%
        dplyr::filter(date_end >= FORECAST_START) %>%
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
        ) %>%
        dplyr::filter(
            active_grp == active_x,
            race_grp == race_x,
            age_grp == age_x,
            spec_grp == spec_x,
            img_grp == img_x,
            death_type == death_x
        )
    
    ## Get bootstrap samples of the expected total deaths
    projected_reps <- replicate(expr = {
        forecast(
            target_model,
            simulate = TRUE,
            times = 1,
            new_data = new_dat
        )
    },
    n = n_reps)
    
    projected_deaths <- dplyr::tibble(
        active_grp = active_x,
        race_grp = race_x,
        age_grp = age_x,
        spec_grp = spec_x,
        img_grp = img_x,
        death_type = death_x,
        month_from_start = unlist(projected_reps[4, ]),
        exp_deaths = unlist(projected_reps[3, ]),
        pop = unlist(projected_reps[7, ]),
        sim_id = rep(1:NCOL(projected_reps),
                     each = NROW(projected_reps[[1]]))
    ) %>%
        dplyr::left_join(sub_death %>%
                             dplyr::rename(obs_deaths = n_deaths)) %>%
        dplyr::mutate(excess_deaths = obs_deaths - exp_deaths) %>%
        dplyr::group_by(active_grp,
                        race_grp,
                        age_grp,
                        spec_grp,
                        img_grp,
                        death_type,
                        sim_id) %>%
        dplyr::arrange(date, .by_group = TRUE) %>%
        dplyr::mutate(cume_excess = cumsum(excess_deaths)) %>%
        dplyr::ungroup()
    
    ## Get weekly expected deaths ----
    expected_deaths <- projected_deaths %>%
        dplyr::group_by(
            active_grp,
            race_grp,
            age_grp,
            spec_grp,
            img_grp,
            death_type,
            month_from_start,
            date,
            date_end,
            obs_deaths,
            pop
        ) %>%
        dplyr::summarize(
            nsim = max(sim_id),
            expected_mean = mean(exp_deaths),
            expected_p025 = stats::quantile(exp_deaths, .025),
            expected_p250 = stats::quantile(exp_deaths, .25),
            expected_p500 = stats::quantile(exp_deaths, .5),
            expected_p750 = stats::quantile(exp_deaths, .75),
            expected_p975 = stats::quantile(exp_deaths, .975),
            expected_min = min(exp_deaths),
            expected_max = max(exp_deaths),
            expected_sd = stats::sd(exp_deaths)
        ) %>%
        dplyr::ungroup()
    
    ## Get weekly excess deaths ----
    excess_deaths <- projected_deaths %>%
        dplyr::group_by(
            active_grp,
            race_grp,
            age_grp,
            spec_grp,
            img_grp,
            death_type,
            month_from_start,
            date,
            date_end,
            obs_deaths,
            pop
        ) %>%
        dplyr::summarize(
            nsim = max(sim_id),
            excess_mean = mean(excess_deaths),
            excess_p025 = stats::quantile(excess_deaths, .025),
            excess_p250 = stats::quantile(excess_deaths, .25),
            excess_p500 = stats::quantile(excess_deaths, .5),
            excess_p750 = stats::quantile(excess_deaths, .75),
            excess_p975 = stats::quantile(excess_deaths, .975),
            excess_min = min(excess_deaths),
            excess_max = max(excess_deaths),
            excess_sd = stats::sd(excess_deaths)
        ) %>%
        dplyr::ungroup()
    
    ## Get cumulative excess deaths ----
    cume_excess_deaths <- projected_deaths %>%
        dplyr::group_by(
            active_grp,
            race_grp,
            age_grp,
            spec_grp,
            img_grp,
            death_type,
            month_from_start,
            date,
            date_end,
            obs_deaths,
            pop
        ) %>%
        dplyr::summarize(
            nsim = max(sim_id),
            cume_mean = mean(cume_excess),
            cume_p025 = stats::quantile(cume_excess, .025),
            cume_p250 = stats::quantile(cume_excess, .25),
            cume_p500 = stats::quantile(cume_excess, .5),
            cume_p750 = stats::quantile(cume_excess, .75),
            cume_p975 = stats::quantile(cume_excess, .975),
            cume_min = min(cume_excess),
            cume_max = max(cume_excess),
            cume_sd = stats::sd(cume_excess)
        ) %>%
        dplyr::ungroup()
    
    expected_deaths %>%
        dplyr::left_join(excess_deaths) %>%
        dplyr::left_join(cume_excess_deaths)
}
