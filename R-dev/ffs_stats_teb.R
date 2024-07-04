# https://github.com/ClimateInequality/PrjDPD/issues/1
# _diary/_issues/issues_20240627.md

library(dplyr)
library(tidyr)
library(readr)
library(tibble)
library(glue)

# 1. Load in the outputs from R-dev/ffs_simu_climate_pop.R
df_popenv_cdf_by_popgrp <- readr::read_csv("data-csv/df_popenv_cdf_by_popgrp.csv")
st_env_var <- "pm"
st_loc_var <- "loc"
str_prefix_demo <- "popgroup"
# this is the lower-end cut-off, only consider data above this threshold
fl_tl <- 35
# this is the upper-end cut-off, only consider data below this threshold
fl_tu <- Inf

# controls
verbose <- TRUE
it_row_print <- 300
it_file_code <- 953090
bl_main_save <- TRUE

### 2. Algorithm Part 1 ----------------------------------------------------------------
# Filter data by thresholds and generate new group-mass.
df_cdfpg <- df_popenv_cdf_by_popgrp
# a. Generate a boolean for whether the individual observation/row is within the thresholds bounds or not
df_cdfpg <- df_cdfpg %>% 
    mutate(
        bl_in_bound = case_when(
            (!!sym(st_env_var) < fl_tu) & (!!sym(st_env_var) > fl_tl) ~ 1,
            TRUE ~ 0
        )
    )
if (verbose) {
  print(glue::glue("F-{it_file_code}, B2PA"))
  print(
    df_cdfpg %>% 
        group_by(!!sym(str_prefix_demo), bl_in_bound) %>%
        tally(), 
        n = it_row_print
  )
}
# b. Count the total population mass within the threshold bounds across all groups.  
fl_pop_in_bound <- df_cdfpg %>% 
    filter(bl_in_bound == 1) %>% 
    summarize(pop_total = sum(pop_frac, na.rm =T)) %>% pull(pop_total)
# c. Filter out from the input all individual observations/rows where (1) is outside of bounds
df_cdfpg_in_bound <- df_cdfpg %>% filter(bl_in_bound == 1)
if (verbose) {
  print(glue::glue("F-{it_file_code}, B2Pc"))
  print(glue::glue("dim(df_cdfpg) = {dim(df_cdfpg)}"))
  print(glue::glue("dim(df_cdfpg_in_bound) = {dim(df_cdfpg_in_bound)}"))
}
# d. Within-each group, sum the population mass, to obtain threshold-filterd total mass for each group.
df_cdfpg_in_bound <- df_cdfpg_in_bound %>% 
    group_by(!!sym(str_prefix_demo)) %>%
    mutate(sum_pop_frac_popgrp = sum(pop_frac, na.rm=T))
if (verbose) {
  print(glue::glue("F-{it_file_code}, B2Pd"))
  print(
    df_cdfpg_in_bound %>% 
        distinct(
            !!sym(str_prefix_demo), sum_pop_frac_popgrp
            ) %>% arrange(sum_pop_frac_popgrp), 
        n = it_row_print
  )
}

### 3, Algorithm Part 2 ----------------------------------------------------------------
# Generate overall average and group averaged, based on threshold-filtered data.
# a. For all rows, compute threshold-filter population weight that sums to 1 for all remaining rows.
df_cdfpg_in_bound_mn <- df_cdfpg_in_bound %>% mutate(
        pop_frac_in_bound = pop_frac/fl_pop_in_bound
    )
# b. Generate weighted-average for exposure with (1.4) and exposure per individual.
df_cdfpg_in_bound_mn <- df_cdfpg_in_bound_mn %>% 
    ungroup() %>% mutate(
        popenv_mean_in_bound = sum(pop_frac_in_bound*!!sym(st_env_var))
    )
# c. Similar to (2.1), but Within-each group, compute threshold-filterd population weights that sums to 1 for each group, given the observations that remain.
df_cdfpg_in_bound_mn <- df_cdfpg_in_bound_mn %>% mutate(
        pop_frac_by_popgrp_in_bound = pop_frac/sum_pop_frac_popgrp
    )
it_invalid_count <- dim(df_cdfpg_in_bound_mn %>% filter(pop_frac_by_popgrp_in_bound < pop_frac))[1]
if (it_invalid_count > 0) {
    stop(glue::glue("F-{it_file_code}, B3Pc: ",
        "Pop frac by group should always be larger than pop frac unconditional"
        ))
}
if (verbose) {
    print(glue::glue("F-{it_file_code}, B3Pc"))
    print(
        df_cdfpg_in_bound_mn %>% 
            group_by(!!sym(str_prefix_demo)) %>%
            summarize(pop_frac_by_popgrp_in_bound_sum = sum(pop_frac_by_popgrp_in_bound))
    )
}
# d. Similar to (2.2), but for each group, compute weighted-average group-average/total exposure for individuals within the thresholds.
df_cdfpg_in_bound_mn <- df_cdfpg_in_bound_mn %>% 
    group_by(!!sym(str_prefix_demo)) %>%
    mutate(
        popenv_mean_by_popgrp_in_bound = sum(pop_frac_by_popgrp_in_bound*!!sym(st_env_var))
    )
if (verbose) {
  print(glue::glue("F-{it_file_code}, B3Pd"))
  print(df_cdfpg_in_bound_mn, n = it_row_print)
  print(
    df_cdfpg_in_bound_mn %>% 
        distinct(
            !!sym(str_prefix_demo), popenv_mean_in_bound, popenv_mean_by_popgrp_in_bound
            ) %>% arrange(popenv_mean_by_popgrp_in_bound), 
        n = it_row_print
  )
}

### 4, Algorithm Part 3 ----------------------------------------------------------------
# Compute our statistics TEB

# a. **Denominator for each group**: Denominator is equal to (1.4)/(1.2), share of population for group given threshold
# b. **Numerator for each group**: Numerator is equal to ((2.4)x(1.4/1.2))/(2.2), share of pollution for each group given threshold 
# c. **Threshold Excess burden**: = (((2.4)x(1.4/1.2))/(2.2))/((1.4)/(1.2)) - 1 = (2.4)/(2.2) - 1# Algorithm Part 1 -----------
df_teb <- df_cdfpg_in_bound_mn %>% 
    group_by(!!sym(str_prefix_demo)) %>%
    slice_head(n=1) %>% 
    select(!!sym(str_prefix_demo), popenv_mean_in_bound, popenv_mean_by_popgrp_in_bound) %>% 
    mutate(
        teb_by_popgrp = popenv_mean_by_popgrp_in_bound/popenv_mean_in_bound - 1
    )
if (verbose) {
  print(glue::glue("F-{it_file_code}, B4"))
  print(df_teb, n = it_row_print)
}
write_csv(df_teb, 'data-csv/df_teb.csv')


# Script and Function test ----------------------
# Test that function generates the same thing as the script
df_cdfpg <- df_popenv_cdf_by_popgrp
ls_return <- ffp_dpd_teb(
    df_cdfpg,
    st_env_var = st_env_var,
    st_loc_var = st_loc_var,
    str_prefix_demo = str_prefix_demo,
    # this is the lower-end cut-off, only consider data above this threshold
    fl_tl = fl_tl,
    # this is the upper-end cut-off, only consider data below this threshold
    fl_tu = fl_tu,
    verbose = FALSE
    )
# Check on function and script consistency
bl_func_script_consistency <- identical(
  df_teb, ls_return$df_teb)
if (verbose) {
  print(glue::glue("F-{it_file_code}, F"))
  print(glue::glue("bl_func_script_consistency = {bl_func_script_consistency}"))
}
