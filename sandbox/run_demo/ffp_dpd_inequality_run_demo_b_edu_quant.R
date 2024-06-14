# This file runs the ffp_evd_simu_loc_demo_main() function.
# Binary education groups, across within-person and within group quantiles.

# 1. Load libraries ------
library(readr)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
# library(tidyverse)

# 2. Specify path of (1) function (2) data input (3) data output -----
spt_root <- "C:/Users/fan/Documents/Dropbox (UH-ECON)/"
# spt_root <- "C:/Users/fan/Dropbox (UH-ECON)/"
# 2.A Activate program
spt_gpp <- "repos/PrjDPD/sandbox/function/"
spt_path_func <- file.path(spt_root, spt_gpp, "ffp_dpd_inequality_func.R", fsep = .Platform$file.sep)
source(spt_path_func)
spt_path_func <- file.path(spt_root, spt_gpp, "ffp_dpd_aux.R", fsep = .Platform$file.sep)
source(spt_path_func)

# 2.B Data input folder
spt_data <- "PIRE/team/xiuqi_yang/PM2.5_tables/final_21group_input"
spt_path_data <- file.path(spt_root, spt_data,
                           fsep = .Platform$file.sep)

# 2.C Results/data output folder
spt_results <- "PIRE/team/xiuqi_yang/PM2.5_tables/final_21group_output/ineq_demo_edu_groupsm2_bl_quant"
spt_path_out <- file.path(spt_root, spt_results,
                           fsep = .Platform$file.sep)

# 3. Data file names -----
st_file_demo <- "guangdong_2010_demog_21grps.csv"
st_file_envir <- "pm2p5_gd_2010_1_12.csv"

# 4. Key file names -----
# 4.1 Name of the population key file
st_file_key_popgrp <- "key_21grp_edu_mgrt.csv"
# 4.2 Name of the loc key file:
st_file_key_loc <- "key_loc.csv"
# 4.3 Name of the higher-loc-key file:
st_file_key_loc_agg <- "key_loc.csv"

# 5. Variable names for population groups -----
str_prefix_demo <- "popgrp"
stv_key_demo <- "popgrp_key"
stv_grp_demo <- "edu_groupsm2_bl"
arv_label_demo <- c("edu_groupsm2_bl", "edu_groupsm2")

# 6. Variable names for location names -----
str_prefix_loc <- ""
stv_key_loc <- "location_key"
stv_key_loc_agg <- "Gbcounty"
stv_grp_loc <- "all_locations"
arv_label_loc <- c()

# 7. Time variables -----
str_prefix_time <- "month"

# 8. some additional parameters ------
bl_save_img <- FALSE
bl_save_csv <- TRUE
verbose <- FALSE
verbose_debug <- TRUE

# 9. Percentiles ----
ar_fl_percentiles <- seq(0.1, 0.9, by=0.1)
ar_fl_ratio_upper <- c(0.8, 0.9)
ar_fl_ratio_lower <- c(0.2, 0.1)

# 10. Stats to compute within year ----
snm_prefix <- "ineq"
st_time_stats <- "quantile"
bl_greater <- TRUE
ar_fl_person_quantile <- seq(0.1, 0.9, by=0.1)
it_file_ctr <- 0
for (fl_person_quantile in ar_fl_person_quantile) {
  # fl_inperson_quantile <- 0.5
  it_file_ctr <- it_file_ctr + 1

  snm_new_file_name_prefix <- ffp_demo_file_prefix(
        snm_prefix = snm_prefix,
        st_time_stats = st_time_stats, 
        fl_person_quantile = fl_person_quantile,
        verbose = FALSE)$snm_new_file_name_prefix

  # 12. Run function ----
  df_excburden_percentiles_keys <- ffp_demo_loc_env_inequality(
    spt_path_data, spt_path_out,
    st_file_demo = st_file_demo,
    st_file_envir = st_file_envir,
    st_file_key_popgrp = st_file_key_popgrp,
    st_file_key_loc = st_file_key_loc,
    st_file_key_loc_agg = st_file_key_loc_agg,
    str_prefix_demo = str_prefix_demo,
    stv_key_demo = stv_key_demo,
    stv_grp_demo = stv_grp_demo, arv_label_demo = arv_label_demo,
    str_prefix_loc = str_prefix_loc,
    stv_key_loc = stv_key_loc, stv_key_loc_agg = stv_key_loc_agg,
    stv_grp_loc = stv_grp_loc, arv_label_loc = arv_label_loc,
    str_prefix_time = str_prefix_time,
    snm_new_file_name_prefix = snm_new_file_name_prefix,
    st_time_stats = st_time_stats, 
    fl_person_quantile = fl_person_quantile,
    ar_fl_percentiles = ar_fl_percentiles, 
    ar_fl_ratio_lower = ar_fl_ratio_lower,
    ar_fl_ratio_upper = ar_fl_ratio_upper,
    bl_save_img = bl_save_img, bl_save_csv = bl_save_csv,
    verbose = verbose, verbose_debug = verbose_debug)
  # print output
  print(df_excburden_percentiles_keys)

    # 9. some additional parameters ------
    # Combine existing outputs to aggregate file
    # File name
    snm_new_file_name <- paste(
      snm_new_file_name_prefix,
      stv_grp_demo, stv_grp_loc, sep = "_")

    # Load file
    spn_results_file <- file.path(
      spt_path_out,
      paste0(snm_new_file_name, '.csv'),
      fsep = .Platform$file.sep)
    df_excburden_percentiles_keys <- readr::read_csv(spn_results_file)

    # Add column to file
    df_excburden_percentiles_keys <- df_excburden_percentiles_keys %>%
      mutate(
        st_time_stats = st_time_stats,
        within_person_percentile = fl_person_quantile,
        compute_id = it_file_ctr)      
    
    # Keep only within-group inequality information, drop bottom two overall and across
    # group inequality rows
    df_excburden_percentiles_keys <- df_excburden_percentiles_keys %>%
      drop_na(!!sym(str_prefix_demo))

    if (it_file_ctr == 1) {
      df_quantile_jnt_main <- df_excburden_percentiles_keys
    } else {
      df_quantile_jnt_main <- bind_rows(
        df_quantile_jnt_main, df_excburden_percentiles_keys)
    }        

}

df_quantile_jnt_main_sel <- df_quantile_jnt_main %>%
  select(one_of(    
    str_prefix_demo,
    stv_grp_demo, arv_label_demo, 
    stv_grp_loc, arv_label_loc
  ), within_person_percentile, contains("pm10_p"))

df_quantile_jnt_main_long <- df_quantile_jnt_main_sel %>%
  pivot_longer(cols = starts_with('pm10_p'),
               names_to = c('within_group_percentile'),
               names_pattern = paste0("pm10_p(.*)"),
               values_to = "value") %>%
  mutate(within_person_percentile = within_person_percentile*100)

snm_new_file_name <- paste(
  snm_prefix,
  stv_grp_demo, st_time_stats, "jnt",
  sep = "_")

spn_output_file <- file.path(
  spt_path_out,
  paste0(snm_new_file_name, '.csv'),
  fsep = .Platform$file.sep)

readr::write_csv(df_quantile_jnt_main_long, spn_output_file, na="0")
if (verbose_debug) {
  print(glue::glue(
    "File saved successfully: ", spn_output_file))
}
