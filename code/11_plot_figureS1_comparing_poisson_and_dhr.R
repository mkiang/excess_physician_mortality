## Imports ----
library(tidyverse)
library(here)
source(here("code", "mk_nytimes.R"))
source(here("code", "utils.R"))

## Data ----
### Poisson model ----
cume_df <- readRDS(here("data", "cume_excess_deaths_monthly.RDS")) %>%
    filter(!is.na(age_grp),
           age_grp != "all",
           race_grp == "all",
           img_grp == "all",
           spec_grp == "all",
           age_grp != "under45",
           age_grp != "85andup",
           active_grp != "all",
           active_grp != "active",
           date == max(date)) %>% 
    select(-race_grp, -img_grp, -spec_grp, -observed) %>%
    as_tibble() %>% 
    mutate(model_type = "Primary model (Poisson)")

dhr_df <- readRDS(here("data", "supp_dhr_month_excess_metrics.RDS")) %>%
    filter(
        !is.na(age_grp),
        age_grp != "all",
        race_grp == "all",
        img_grp == "all",
        spec_grp == "all",
        age_grp != "under45",
        age_grp != "85andup",
        active_grp != "all",
        active_grp != "active",
        date == max(date)
    ) %>%
    select(
        date,
        fitted = cume_mean,
        upper = cume_p975,
        lower = cume_p025,
        active_grp,
        age_grp,
        death_type
    ) %>%
    as_tibble() %>% 
    mutate(model_type = "Dynamic harmonic regression (ARIMA)")
    

plot_df <- cume_df %>%
    bind_rows(dhr_df) %>%
    categorize_age() %>%
    categorize_death() %>%
    categorize_active() 

## Create Figure S1 ----
p1 <- ggplot(
    plot_df,
    aes(
        y = age_cat_rev,
        group = model_type,
        x = fitted,
        xmin = lower,
        xmax = upper,
        color = model_type
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
    scale_x_continuous("Cumulative excess deaths (N)") +
    scale_y_discrete("Age group (years)") +
    scale_color_brewer(
        "Model type",
        palette = "Set2",
        guide = guide_legend(
            title.position = "top",
            direction = "vertical",
            reverse = TRUE
        )
    ) +
    mk_nytimes(legend.position = "bottom",
               panel.grid.major.y = element_blank()) +
    facet_grid(active_cat ~ .)

## Save ----
ggsave(
    here("plots", "figureS1.pdf"),
    p1,
    width = 5,
    height = 8,
    scale = 1,
    device = cairo_pdf
)
ggsave(
    here("plots", "figureS1.jpg"),
    p1,
    width = 5,
    height = 8,
    scale = 1,
    dpi = 300
)

write_csv(
    plot_df %>%
        select(age_cat, active_cat, model_type, fitted, lower, upper) %>%
        arrange(age_cat, active_cat, model_type),
    here("output", "figS1.csv")
)
