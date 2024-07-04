library(dplyr)
library(tidyr)
library(readr)
library(tibble)
library(glue)

# 1. Simulation parameters ------------------
it_M_location <- 140
it_N_pop_groups <- 21

fl_meanlog <- 3
fl_sdlog <- 0.5

it_rng_seed_pop_byloc <- 123
it_rng_seed_pop_ofloc <- 123
it_rng_seed_env <- 456

st_env_var <- "pm"
st_loc_var <- "loc"
str_prefix_demo <- "popgroup"
verbose <- TRUE

# 2. Simulate population data --------------------------------
mt_pop_data_frac <- matrix(data = NA, nrow = it_M_location, ncol = it_N_pop_groups)
colnames(mt_pop_data_frac) <- paste0(str_prefix_demo, seq(1, it_N_pop_groups))
rownames(mt_pop_data_frac) <- paste0(st_loc_var, seq(1, it_M_location))
# Share of population per location
set.seed(it_rng_seed_pop_byloc)
ar_p_loc <- dbinom(0:(3 * it_M_location - 1), 3 * it_M_location - 1, 0.5)
it_start <- length(ar_p_loc) / 2 - it_M_location / 2
ar_p_loc <- ar_p_loc[it_start:(it_start + it_M_location - 1)]
ar_p_loc <- ar_p_loc / sum(ar_p_loc)
# Different bernoulli "win" probability for each location
set.seed(it_rng_seed_pop_ofloc)
bl_unif_base <- TRUE
if (bl_unif_base) {
    ar_fl_unif_prob <- sort(runif(it_M_location))
} else {
    ar_fl_unif_prob <- sort(runif(it_M_location)*(0.25)+0.4)
}
# Generate population proportion by locality
for (it_loc in 1:it_M_location) {
    ar_p_pop_condi_loc <- dbinom(
        0:(it_N_pop_groups - 1), it_N_pop_groups - 1, ar_fl_unif_prob[it_loc]
        )
    mt_pop_data_frac[it_loc,] <- ar_p_pop_condi_loc * ar_p_loc[it_loc]
}
# Sum of cells, should equal to 1
if (verbose) {
    print(glue::glue("F-391491, S1"))
    print(paste0('pop frac sum = ', sum(mt_pop_data_frac)))
}
# Convert to tibble
df_pop_data_frac <- as_tibble(mt_pop_data_frac, rownames = st_loc_var)
if (verbose) {
    print(glue::glue("F-39149, S2"))
    print(df_pop_data_frac)
}

# 3. Simulate environmental data ------------------------------------
# draw randomly
set.seed(it_rng_seed_env)
ar_pollution_loc <- rlnorm(it_M_location, meanlog = fl_meanlog, sdlog = fl_sdlog)
# environmental dataframe
# 5 by 3 matrix
# Column Names
ar_st_varnames <- c(st_loc_var, st_env_var)
# Combine to tibble, add name col1, col2, etc.
tb_loc_pollution <- as_tibble(ar_pollution_loc) %>%
    rowid_to_column(var = st_loc_var) %>%
    rename_all(~c(ar_st_varnames)) %>%
    mutate(!!sym(st_loc_var) := paste0(st_loc_var, !!sym(st_loc_var)))
# Display
if (verbose) {
    print(glue::glue("F-39149, S3"))
    print(tb_loc_pollution)
}

# 4. Save file out ----
df_pop_by_loc_demo <- df_pop_data_frac
df_env_by_loc <- tb_loc_pollution
write_csv(df_pop_by_loc_demo, 'data-csv/df_pop_by_loc_demo.csv')
write_csv(df_env_by_loc, 'data-csv/df_env_by_loc.csv')