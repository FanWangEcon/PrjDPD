#' Generate file prefix/suffix
#'
#' @description Common prefix generator for file names.
#'
#' @param snm_new_file_name_prefix string file name prefix for csv and img
#' @param st_time_stats string for type of within year stats to compute, 
#' "quantile", "mean", "idtrmn" or "share"
#' @param bl_temp_bound temperature bound if \input{st_time_stats} is share of days
#' @param bl_quantile within-person quantile to select if \input{st_time_stats} is quantile.
#' @param bl_greater boolean if to compute larger or smaller than \input{fl_temp_bond}
#' @param verbose boolean print progress and key results
#' @author fan wang, \url{http://fanwangecon.github.io}
#'
#' @return an array of tax-liabilities for particular kids count and martial
#'   status along an array of income levels
#' @references
#' \url{https://fanwangecon.github.io/prjenvdemo/articles/fv_rda_simu_loc_demo.html}
#' @export
#' @import readr dplyr tidyr ggplot2
#'
ffp_demo_file_prefix <- function(
    snm_prefix = "ineq",
    st_time_stats = "share", 
    fl_temp_bound = 20, 
    fl_person_quantile = 0.8,
    bl_greater = TRUE,
    verbose = FALSE) {

    if (tolower(st_time_stats) == tolower("quantile")) {
        st_rela <- ""
        fl_quantile_100 <- paste0("q", fl_person_quantile*100)
        st_quantile <- gsub(x = fl_quantile_100,  pattern = "\\.", replacement = "p")
        snm_new_file_name_prefix <- paste(snm_prefix, st_time_stats, st_quantile, sep = "_")

    } else {
        # st_time_stats equals to share, mean, idtrmn
        if (bl_greater) {
            st_rela <- paste0(st_time_stats, "_gr")
            snm_new_file_name_prefix <- paste(snm_prefix, st_rela, fl_temp_bound, sep = "_")
        } else {
            st_rela <- paste0(st_time_stats, "_ls")
            snm_new_file_name_prefix <- paste(snm_prefix, st_rela, fl_temp_bound, sep = "_")
        }

    }

    ls_snm_new_file_name_prefix <- list(
        snm_new_file_name_prefix = snm_new_file_name_prefix,
        st_rela = st_rela
    )

    return(ls_snm_new_file_name_prefix) 
}