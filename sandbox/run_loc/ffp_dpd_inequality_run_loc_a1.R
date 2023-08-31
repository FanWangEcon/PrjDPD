# This file runs the ffp_evd_simu_loc_demo_main() function.
# Demographics only grouping

# 1. Load libraries ------
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
# library(tidyverse)

# 2. Specify path of (1) function (2) data input (3) data output -----
spt_root <- "C:/Users/fan/Documents/Dropbox (UH-ECON)/"
# 2.A Activate program
spt_gpp <- "repos/PrjDPD/sandbox/function/"
spt_path_func <- file.path(spt_root, spt_gpp, "ffp_dpd_inequality_func.R",
                           fsep = .Platform$file.sep)
source(spt_path_func)

# 2.B Data input folder
spt_data <- "PIRE/team/xiuqi_yang/PM2.5_tables/final_21group_input"
spt_path_data <- file.path(spt_root, spt_data,
                           fsep = .Platform$file.sep)

# 2.C Results/data output folder
spt_results <- "PIRE/team/xiuqi_yang/PM2.5_tables/final_21group_output/ineq_loc"
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
stv_grp_demo <- "all_groups"
arv_label_demo <- c()

# 6. Variable names for location names -----
str_prefix_loc <- ""
stv_key_loc <- "location_key"
stv_key_loc_agg <- "Gbcounty"
stv_grp_loc <- "GBCity"
arv_label_loc <- c("Cityname", "region")

# 7. Time variables -----
str_prefix_time <- "month"

# 8. some additional parameters ------
bl_save_img <- TRUE
bl_save_csv <- TRUE
verbose <- FALSE
verbose_debug <- TRUE

# 9. Percentiles ----
ar_fl_percentiles <- seq(0.05, 0.95, length.out=19)
ar_fl_ratio_upper <- c(0.8, 0.9)
ar_fl_ratio_lower <- c(0.2, 0.1)

# 10. Stats to compute within year ----
st_time_stats <- "mean"
bl_greater <- TRUE
ar_fl_temp_bound <- seq(0, 50, by=5)
for (fl_temp_bound in ar_fl_temp_bound) {
  # fl_temp_bound <- 35

  # 11. File names prefix, the rest of name from combining grp_demo and grp_loc ----
  # stv_grp_demo and stv_grp_loc
  if (bl_greater) {
    snm_new_file_name_prefix <- paste0("ineq_", st_time_stats, "_gr", fl_temp_bound)
  } else {
    snm_new_file_name_prefix <- paste0("ineq_", st_time_stats, "_ls", fl_temp_bound)
  }

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
    st_time_stats = st_time_stats, fl_temp_bound = fl_temp_bound, bl_greater = bl_greater,
    ar_fl_percentiles = ar_fl_percentiles, 
    ar_fl_ratio_lower = ar_fl_ratio_lower,
    ar_fl_ratio_upper = ar_fl_ratio_upper,
    bl_save_img = bl_save_img, bl_save_csv = bl_save_csv,
    verbose = verbose, verbose_debug = verbose_debug)
  # print output
  print(df_excburden_percentiles_keys)
}