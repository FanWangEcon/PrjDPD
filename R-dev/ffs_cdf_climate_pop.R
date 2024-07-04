library(dplyr)
library(tidyr)
library(readr)
library(tibble)
library(glue)

# 1. Load in the outputs from R-dev/ffs_simu_climate_pop.R
df_env_by_loc <- readr::read_csv("data-csv/df_env_by_loc.csv")
df_pop_by_loc_demo <- readr::read_csv("data-csv/df_pop_by_loc_demo.csv")
st_env_var <- "pm"
st_loc_var <- "loc"
str_prefix_demo <- "popgroup"
verbose <- TRUE

# 2. Pop by loc demo from from wide to long -----------------------
# Generate total mass across pop cells
fl_total_pop <- sum(
        df_pop_by_loc_demo[, 2:(dim(df_pop_by_loc_demo)[2])], na.rm = TRUE
    )
# Reshape population data, so each observation is location/demo ----
df_pop_by_loc_demo_long <- df_pop_by_loc_demo %>%
    pivot_longer(cols = starts_with(str_prefix_demo),
                    names_to = c(str_prefix_demo),
                    names_pattern = paste0(str_prefix_demo, "(.*)"),
                    values_to = "pop_frac")
df_pop_by_loc_demo_long <- df_pop_by_loc_demo_long %>%
    mutate(pop_frac = pop_frac / fl_total_pop) %>%
    drop_na(pop_frac)
if (verbose) {
    print(sum(df_pop_by_loc_demo_long$pop_frac, na.rm = TRUE))
    print(df_pop_by_loc_demo_long, n=50)
}

# 3. Combine population and pollution data -------------
df_pop_pollution_long <- df_pop_by_loc_demo_long %>%
    left_join(df_env_by_loc, by = st_loc_var) %>%
    drop_na(!!sym(st_env_var))
if (verbose) {
    print(sum(df_pop_by_loc_demo_long$pop_frac, na.rm = TRUE))
    print(df_pop_pollution_long, n=50)
}

# 4. Compute within population group CDF 
# Nearest neighbor percentiles
df_pop_pollution_by_popgrp_cdf <- df_pop_pollution_long %>%
    arrange(!!sym(str_prefix_demo), !!sym(st_env_var)) %>%
    group_by(!!sym(str_prefix_demo)) %>%
    mutate(
            cdf_popenv_by_popgrp = cumsum(pop_frac / sum(pop_frac)),
            pmf_popenv_by_popgrp = (pop_frac / sum(pop_frac))
        )
# Display
if (verbose) {
    print(df_pop_pollution_by_popgrp_cdf, n=200)
}

# 5. Save output
df_popenv_cdf_by_popgrp <- df_pop_pollution_by_popgrp_cdf
write_csv(df_popenv_cdf_by_popgrp, 'data-csv/df_popenv_cdf_by_popgrp.csv')