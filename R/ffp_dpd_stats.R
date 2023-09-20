#' Compute GINI given discrete distribution
#'
#' @description Discrete distribution-specific GINI. 
#'
#' @param ar_data array sorted array values low to high
#' @param ar_prob_data array probability mass for each element along `ar_data`, sums to 1
#' @return A scalar value for the GINI coefficient, between 0 and < 1
#' @export
#' @examples
#' ffp_dpd_gini_drm(c(1,2,3), c(1/3, 1/3, 1/3))
#' ffp_dpd_gini_drm(c(1e-5, 1e-5, 1e5), c(1/3, 1/3, 1/3))
#' @references
#' \href{https://fanwangecon.github.io/R4Econ/math/func_ineq/htmlpdfr/fs_gini_disc.html}{fs_gini_disc}
#' @seealso [ffp_dpd_cov_drm()] for coefficient of variation and [ffp_dpd_atkinson_drm()] 
#' for the Atkinson Index.
#' @author Fan Wang, \url{http://fanwangecon.github.io}
#'
ffp_dpd_gini_drm <- function(ar_data, ar_prob_data) {
  fl_mean <- sum(ar_data*ar_prob_data)
  ar_mean_cumsum <- cumsum(ar_data*ar_prob_data)
  ar_height <- ar_mean_cumsum/fl_mean
  fl_area_drm <- sum(ar_prob_data*ar_height)
  fl_area_below45 <- sum(ar_prob_data*(cumsum(ar_prob_data)/sum(ar_prob_data)))
  fl_gini_index <- (fl_area_below45-fl_area_drm)/fl_area_below45
  return(fl_gini_index)
}

#' Compute Atkinson (1970) Inequality Index
#'
#' @description Atkinson inequality index
#'
#' @param ar_data array sorted array values
#' @param ar_prob_data array probability mass for each element along `ar_data`, sums to 1
#' @param fl_rho float inequality aversion parameter fl_rho = 1 for planner
#' without inequality aversion. fl_rho = -infinity for fully inequality averse.
#' @return A scalar value for the Atkinson index, between 0 and < 1
#' @export
#' @examples
#' ffp_dpd_atkinson_drm(c(1,2,3), c(1/3, 1/3, 1/3), 0.65)
#' ffp_dpd_atkinson_drm(c(1,2,3), c(1/3, 1/3, 1/3), 0.25)
#' ffp_dpd_atkinson_drm(c(1,2,3), c(1/3, 1/3, 1/3), 0.15)
#' ffp_dpd_atkinson_drm(c(1e-5, 1e-5, 1e5), c(1/3, 1/3, 1/3), 0.65)
#' ffp_dpd_atkinson_drm(c(1e-5, 1e-5, 1e5), c(1/3, 1/3, 1/3), 0.25)
#' ffp_dpd_atkinson_drm(c(1e-5, 1e-5, 1e5), c(1/3, 1/3, 1/3), 0.15)
#' @references
#' \href{https://fanwangecon.github.io/R4Econ/math/func_ineq/htmlpdfr/fs_gini_disc.html}{fs_gini_disc}
#' @seealso [ffp_dpd_gini_drm()] for the Gini coefficient and [ffp_dpd_cov_drm()] 
#' for the coefficient of variation.
#' @author Fan Wang, \url{http://fanwangecon.github.io}
#'
ffp_dpd_atkinson_drm <- function(ar_data, ar_prob_data, fl_rho) {
  #' @param ar_data array sorted array values
  #' @param ar_prob_data array probability mass for each element along `ar_data`, sums to 1
  #' @param fl_rho float inequality aversion parameter fl_rho = 1 for planner
  #' without inequality aversion. fl_rho = -infinity for fully inequality averse.

  fl_mean <- sum(ar_data*ar_prob_data);
  fl_atkinson <- 1 - (sum(ar_prob_data*(ar_data^{fl_rho}))^(1/fl_rho))/fl_mean
  return(fl_atkinson)
}

#' Compute Coefficient of Variation given discrete distribution
#'
#' @description Coefficient of variation. 
#'
#' @param ar_data array array values
#' @param ar_prob_data array probability mass for each element along `ar_data`, sums to 1
#' @return A scalar value for the the coefficient of variation, always positive
#' @export
#' @examples
#' ffp_dpd_cov_drm(c(1,2,3), c(1/3, 1/3, 1/3))
#' ffp_dpd_cov_drm(c(1e-5, 1e-5, 1e5), c(1/3, 1/3, 1/3))
#' @references
#' \href{https://fanwangecon.github.io/R4Econ/math/func_ineq/htmlpdfr/fs_gini_disc.html}{fs_gini_disc}
#' @seealso [ffp_dpd_gini_drm()] for the Gini coefficient and [ffp_dpd_atkinson_drm()] 
#' for the Atkinson Index.
#' @author Fan Wang, \url{http://fanwangecon.github.io}
#'
ffp_dpd_cov_drm <- function(ar_data, ar_prob_data, verbose=FALSE) {

  fl_mean <- sum(ar_data*ar_prob_data)
  fl_std <- sqrt(sum(ar_prob_data*(ar_data - fl_mean)^2))
  fl_coef_of_variation <- fl_std/fl_mean
  
  ls_fl_std_cov <- list("std"=fl_std, "cov"= fl_coef_of_variation, "mean" = fl_mean)

  return(ls_fl_std_cov)
}
