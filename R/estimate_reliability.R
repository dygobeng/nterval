#' Estimate coverage reliability
#'
#' @description Given proximity range and sample size, estimate coverage reliability using Monte
#'   Carlo simulation.
#'
#' @param n Sample size.
#' @param k Number of standard deviations from the mean to be used to calculate interval.
#' @param prox_lo Lower proximity limit expressed as a proportion on the closed interval between 0
#'   and 1.
#' @param prox_hi Upper proximity limit expressed as a proportion on the closed interval between 0
#'   and 1.
#' @param n_sim Number of simulation iterations to generate coverage sampling distribution.
#' @param seed Randomization seed. Defaults to `NA` so that no seed is used.
#' @param save_data If `TRUE`, return vector of coverage calculations for simulated samples.
#'
#' @return Estimated reliability (`reliability_hat`) conditional on proximity limits. If `save_data`
#'   is `TRUE`, return a list with `reliability_hat` and vector of simulated sample coverage values.
#' @export
#'
#' @examples
#' estimate_reliability(
#'   n = 20,
#'   k = 2,
#'   prox_lo = 0.93,
#'   prox_hi = 0.97
#' )
estimate_reliability <- function(n,
                                 k,
                                 prox_lo,
                                 prox_hi,
                                 n_sim = 100000,
                                 seed = NA,
                                 save_data = FALSE) {

  assertthat::assert_that(
    (n %% 1 == 0) & n >= 3,
    msg = "n must be a whole number greater than or equal to 3"
  )

  assertthat::assert_that(
    eval_across(list(k, prox_lo, prox_hi),
                fun = "is.numeric"),
    msg = "k, prox_lo, and prox_hi must be numeric values"
  )

  assertthat::assert_that(
    eval_across(list(prox_lo, prox_hi),
                   fun = "~dplyr::between(.x, 0, 1)"),
    msg = "prox_lo and prox_hi must fall within the closed interval [0, 1]"
  )

  if (!is.na(seed)) {
    set.seed(seed)
  }

  cov_n <- rep(NA, n_sim)
  for (i in 1:n_sim) {
    .data <- rnorm(n)
    cov_n[i] <- calc_coverage_normal(.data, k)
  }

  reliability_hat <- mean(cov_n >= prox_lo & cov_n <= prox_hi)

  if (save_data) {
    return(list(reliability_hat = reliability_hat,
                sim_coverage = cov_n))
  }else{
    return(reliability_hat)
  }
}
