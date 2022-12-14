## 09_plot_figure1.R ----

## Imports ----
library(tidyverse)
library(here)
library(patchwork)
library(ggsci)
library(ggstance)
source(here("code", "mk_nytimes.R"))
source(here("code", "utils.R"))

## Data ----
### Physician data ----
pop_df <- readRDS(here("data", "population_monthly.RDS"))

death_df <- readRDS(here("data", "analytic_monthly.RDS")) %>%
    filter(
        date_start >= as.Date("2020-03-01"),
        date_end <= as.Date("2021-12-31"), 
        !is.na(age_grp),
        age_grp != "all",
        race_grp == "all",
        img_grp == "all",
        spec_grp == "all",
        death_type == "all_cause"
    ) %>%
    arrange(race_grp, img_grp, spec_grp, death_type, age_grp, date_start) %>% 
    group_by(age_grp, active_grp) %>%
    summarize(
        n_deaths = sum(n_deaths),
        date = last(date_end),
        month_from_start = last(month_from_start)
    ) %>% 
    ungroup()

cume_df <- readRDS(here("data", "cume_excess_deaths_monthly.RDS")) %>%
    filter(!is.na(age_grp),
           age_grp != "all",
           race_grp == "all",
           img_grp == "all",
           spec_grp == "all") %>%
    select(-race_grp, -img_grp, -spec_grp, -observed) %>%
    categorize_age() %>%
    categorize_death() %>%
    categorize_active() %>%
    left_join(pop_df %>%
                  filter(date >= as.Date("2020-03-01"),
                         date <= as.Date("2021-12-31"))) %>%
    as_tibble()

excess_df <- readRDS(here("data", "excess_deaths_monthly.RDS")) %>%
    filter(!is.na(age_grp),
           age_grp != "all",
           race_grp == "all",
           img_grp == "all",
           spec_grp == "all",
           age_grp != "under45",
           age_grp != "85andup",
           death_type == "all_cause") %>%
    select(-race_grp, -img_grp, -spec_grp) %>%
    categorize_age() %>%
    categorize_death() %>%
    categorize_active() %>% 
    left_join(pop_df) %>%
    as_tibble() %>% 
    filter(!is.na(age_cat))

### Gen pop data ----
genpop_df <- readRDS(here("data", "supp_analytic_weekly_usgenpop.RDS")) %>%
    filter(date_start >= as.Date("2020-03-01"),
           date_end <= as.Date("2021-12-31")) %>% 
    rename(date = date_end) %>% 
    mutate(active_grp = "genpop")

genpop_death <- genpop_df %>%
    filter(
        !is.na(age_grp),
        age_grp != "all",
        death_type == "all_cause"
    ) %>%
    arrange(death_type, age_grp, date_start) %>% 
    group_by(age_grp, active_grp) %>%
    summarize(
        n_deaths = sum(n_deaths),
        date = last(date),
        month_from_start = NA
    ) %>% 
    ungroup()

genpop_cume <- readRDS(
    here("data",
         "supp_cume_excess_deaths_usgenpop.RDS")
    ) %>%
    mutate(active_grp = "genpop") %>% 
    categorize_age() %>%
    categorize_death() %>%
    categorize_active() %>%
    left_join(genpop_df) %>%
    as_tibble() %>% 
    filter(!is.na(age_cat)) 

## Summarize physician cumulative excess deaths ----
## Take the monthly population count as our person-time at risk. Then the 
## cumulative monthly population counts are our cumulative person-months at
## risk and we can annualize it by scaling it 12 (months per year) over the 
## number of months observed. 
N_MONTHS <- n_distinct(cume_df$date)

cume_summary <- cume_df %>%
    group_by(death_type, age_grp, active_grp) %>%
    arrange(death_type, age_cat, active_cat, date) %>%
    mutate(cume_pt_atrisk = cumsum(population)) %>%
    filter(date == max(date)) %>%
    mutate(
        avg_n_prov = round(cume_pt_atrisk / N_MONTHS),
        fitted_per100k = fitted / cume_pt_atrisk * 100000 * 12,
        upper_per100k = upper / cume_pt_atrisk * 100000 * 12,
        lower_per100k = lower / cume_pt_atrisk * 100000 * 12
    ) %>%
    left_join(death_df) %>%
    mutate(
        expected = n_deaths - fitted,
        expected_lower = n_deaths - lower,
        expected_upper = n_deaths - upper
    ) %>%
    ungroup() %>%
    filter(age_grp != "under45",
           age_grp != "85andup",
           active_grp != "all",
           active_grp != "active")

## Summarize genpop cumulative excess deaths ----
## Take the weekly population count as our person-time at risk. Then the 
## cumulative weekly population counts are our cumulative person-weeks at
## risk and we can annualize it by scaling it 365.25/7 (weeks per year) over the 
## number of weeks observed. 
N_WEEKS <- n_distinct(genpop_cume$date)

genpop_summary <- genpop_cume %>%
    group_by(death_type, age_grp, active_grp) %>%
    arrange(death_type, age_cat, active_cat, date) %>%
    mutate(cume_pt_atrisk = cumsum(pop),
           n_deaths = cumsum(n_deaths)) %>%
    filter(date == max(date)) %>%
    mutate(
        avg_n_prov = round(cume_pt_atrisk / N_WEEKS),
        fitted_per100k = fitted / cume_pt_atrisk * 100000 * (365.25 / 7),
        upper_per100k = upper / cume_pt_atrisk * 100000 * (365.25 / 7),
        lower_per100k = lower / cume_pt_atrisk * 100000 * (365.25 / 7)
    ) %>%
    mutate(
        expected = n_deaths - fitted,
        expected_lower = n_deaths - lower,
        expected_upper = n_deaths - upper
    ) %>%
    ungroup() %>% 
    filter(age_grp != "under45",
           age_grp != "85andup")

## Create Figure 1A ----
plot1_df <- bind_rows(cume_summary, genpop_summary)

p1a <- ggplot(
    plot1_df,
    aes(
        y = age_cat_rev,
        group = active_cat_rev,
        x = fitted_per100k,
        xmin = lower_per100k,
        xmax = upper_per100k,
        color = active_cat
    )
) +
    annotate(
        geom = "rect",
        xmin = c(-Inf, -Inf),
        xmax = c(Inf, Inf),
        ymin = c(.4, 2.5),
        ymax = c(1.5, 3.5),
        color = NA,
        fill = "grey80",
        alpha = .5
    ) +
    geom_vline(xintercept = 0) +
    ggstance::geom_linerangeh(position = position_dodge2(width = .7),
                              size = .6,
                              alpha = .8) +
    geom_point(position = position_dodge2(width = .7),
               alpha = .9,
               size = 2.5) +
    scale_x_continuous("Cumulative excess deaths per 100,000 person-years") +
    scale_y_discrete("Age group (years)") +
    ggsci::scale_color_jama(name = NULL) +
    mk_nytimes(legend.position = "bottom",
               panel.grid.major.y = element_blank()) + 
    labs(tag = "A")

## Create Figure 1B ----
p2_data <- excess_df %>% 
    mutate(
        abs_change_100k = abs_change / population * 100000,
        abs_change_lower_100k = abs_change_lower / population * 100000,
        abs_change_upper_100k = abs_change_upper / population * 100000
    ) %>%
    filter(active_grp %in% c("active", "not_active", "all"), 
           age_grp == "45to84")


p1b <- ggplot(data = p2_data %>% 
                  filter(active_grp == "all")) +
    geom_hline(yintercept = 0) +
    geom_ribbon(
        alpha = .2,
        aes(
            x = date,
            y = abs_change,
            ymin = abs_change_lower,
            ymax = abs_change_upper
        )
    ) +
    geom_line(
        aes(
            x = date,
            y = abs_change
        ),
        size = 1.5,
        alpha = 1
    ) +
    mk_nytimes(axis.text.x = element_text(hjust = c(0, .5, .5, .5, 1)),
               legend.position = "none") +
    scale_x_date(
        NULL,
        breaks = c(
            as.Date("2020-04-01"),
            as.Date("2020-07-01"),
            as.Date("2021-01-01"),
            as.Date("2021-07-01"),
            as.Date("2022-01-01")
        ),
        labels = scales::label_date_short(),
        expand = c(0, 1)
    ) +
    scale_y_continuous("Monthly excess deaths\namong physicians (N)",
                       expand = c(0, 1)) +
    labs(tag = "B")

## Combine and save figure and data ----
fig1 <- p1a + p1b + plot_layout(ncol = 1)

ggsave(
    here("plots", "figure1.pdf"),
    fig1,
    width = 8,
    height = 8,
    scale = 1,
    device = cairo_pdf
)
ggsave(
    here("plots", "figure1.jpg"),
    fig1,
    width = 8,
    height = 8,
    scale = 1,
    dpi = 300
)

## Save raw data ----
fig1a_save <- plot1_df %>%
    select(
        age = age_cat,
        pop_type = active_cat,
        obs_deaths = n_deaths,
        expected_deaths = expected,
        excess_deaths = fitted,
        persontime = cume_pt_atrisk,
        excess_per_100k_personyears = fitted_per100k,
        lower_per100k,
        upper_per100k
    ) %>%
    arrange(age, pop_type)
fig1b_save <- p2_data %>%
    filter(age_grp == "45to84") %>% 
    filter(active_grp == "all") %>% 
    select(
        age = age_cat,
        pop_type = active_cat,
        date, 
        excess_deaths = abs_change,
        excess_lower = abs_change_lower,
        excess_upper = abs_change_upper,
        excess_deaths_100k = abs_change_100k,
        excess_lower_100k = abs_change_lower_100k,
        excess_upper_100k = abs_change_upper_100k
    ) %>%
    arrange(age, pop_type, date)

write_csv(fig1a_save, here("output", "fig1a.csv"))
write_csv(fig1b_save, here("output", "fig1b.csv"))
