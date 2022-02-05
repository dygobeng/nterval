#' Title
#'
#' @param n
#' @param k
#' @param prox_lo
#' @param prox_hi
#' @param n_sim
#' @param seed
#' @param save.data
#'
#' @return
#' @export
#'
#' @examples
estimate_reliability <- function(n,
                                 k,
                                 prox_lo,
                                 prox_hi,
                                 n_sim = 100000,
                                 seed = NA) {
  # Estimate the reliability of sample coverage to fall within proximity limits
  # using Monte Carlo simulation
  #
  # Args:
  #   n       : Sample size
  #   n.sim   : Number of Monte Carlo simulations.  Default is 100,000.
  #   seed    : Randomization seed.  Default is NA.
  #
  # Returns:
  #   The estimated reliability expressed in decimal form

  cov_n <- rep(NA, n_sim) # container for sample interval coverages
  if (!is.na(seed)) {
    set.seed(seed)
  }

  for (i in 1:n_sim) {
    .data <- rnorm(n) # simulate random set of n standard normals

    # Calculate coverage for ith iteration
    cov_n[i] <- calc_coverage_normal(.data, k)
  }

  mean(cov_n >= prox_lo & cov_n <= prox_hi)
}
