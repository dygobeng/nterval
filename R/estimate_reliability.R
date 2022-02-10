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
#' @param seed Randomization seed.
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
                                 seed = NULL,
                                 save_data = FALSE) {
  assertthat::assert_that(
    eval_across(
      list(n, k, prox_lo, prox_hi),
      fun = "is.numeric"
    ),
    msg = "k, prox_lo, and prox_hi must be numeric values"
  )

  assertthat::assert_that(
    (n %% 1 == 0) & n >= 3,
    msg = "n must be a whole number greater than or equal to 3"
  )

  assertthat::assert_that(
    eval_across(
      list(prox_lo, prox_hi),
      fun = "~dplyr::between(.x, 0, 1)"
    ),
    msg = "prox_lo and prox_hi must fall within the closed interval [0, 1]"
  )

  assertthat::assert_that(
    prox_lo < prox_hi,
    msg = "prox_lo must be less than prox_hi"
  )

  if (!is.null(seed)) {
    set.seed(seed)
  }

  xbar <- rnorm(n_sim, mean = 0, sd = 1 / sqrt(n))
  s <- sqrt(rchisq(n_sim, df = n - 1) / (n - 1))
  z_data <- matrix(xbar + c(-1, 1) * k * s, ncol = 2, byrow = TRUE)

  cov_vec <- calc_coverage_normal(z_lower = z_data[, 1], z_upper = z_data[, 2])
  reliability_hat <- mean(cov_vec >= prox_lo & cov_vec <= prox_hi)

  if (save_data) {
    return(
      list(
        reliability_hat = reliability_hat,
        sim_coverage = cov_vec
      ))
  } else {
    return(reliability_hat)
  }
}
