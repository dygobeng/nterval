#' Find N for k-sigma sample interval
#'
#' @description Find required sample size for "k-sigma sample interval" (i.e., parametric sample
#'   interval applied to a Normal distribution) to achieve targeted reliability using bisection
#'   algorithm
#'
#' @param proximity_range Range within which sample coverage should fall with pre-specified
#'  `reliability`.
#' @param reliability Targeted probability that individual coverage of parametric sample interval
#'   applied to a Normal distribution falls within `proximity_range`.
#' @param k Standard deviation multiplier for parametric sample interval applied to Normal
#'  distribution (typically 2 or 3). If `NULL`, `k` is assigned based on the midpoint of the
#'  `proximity_range`.
#' @param search_interval Initial interval of sample sizes for bisection algorithm. Default is
#'   c(3, 500).
#' @param n_search Number of simulation iterations during initial search for root. If `NA`, will be
#'   set to approximately 10% of `n_confirm` or 1000, whichever is greater.
#' @param n_confirm Number of simulation iterations used to confirm `reliability`.
#' @param tolerance Threshold for difference between targeted and estimated `reliability` that
#'   indicates sufficient proximity of the root-finding function to 0, triggering the end of the
#'   search. Default is 0.001.
#' @param plot If `TRUE`, plot sampling distribution for true interval coverage using sample size
#'   identified through bisection.
#' @param verbose If `TRUE`, print the search interval used for each round of the bisection
#'   algorithm search.
#' @param seed Randomization seed. Defaults to `NULL` so that no seed is used.
#'
#' @return
#' A list with the following elements:
#' \itemize{
#'   \item The estimated sample size required to achieve target reliability.
#'   \item If \code{plot} is `TRUE`, the plot object.
#' }
#' @export
#'
#' @examples
#' find_n_ksigma(
#'   proximity_range = c(0.93, 0.97),
#'   reliability = 0.7,
#'   k = 2
#' )
find_n_ksigma <- function(proximity_range,
                          reliability,
                          k = NULL,
                          search_interval = c(3, 500),
                          n_search = NA,
                          n_confirm = 100000,
                          tolerance = 0.001,
                          plot = FALSE,
                          verbose = FALSE,
                          seed = NULL) {

  assertthat::assert_that(
    eval_across(list(proximity_range,
                     reliability,
                     search_interval),
                fun = "is.numeric"),
    msg = "proximity_range, reliability, k, and search_interval must be numeric"
  )

  assertthat::assert_that(
    eval_across(list(proximity_range, search_interval),
                fun = "~ length(.x) == 2") %>%
      all(),
    msg = "proximity_range and search_interval must be vectors with 2 elements"
  )

  assertthat::assert_that(
    eval_across(list(proximity_range, search_interval),
                fun = "~ .x[1] < .x[2]") %>%
      all(),
    msg = "proximity_range and search_interval elements must be in ascending order"
  )

  if (!is.null(k)) {
    assertthat::assert_that(
      is.numeric(k),
      msg = "k must be numeric"
    )
    k <- abs(k)
  } else {
    k <- -qnorm((1 - mean(proximity_range)) / 2)
  }

  prox_lo <- proximity_range[1]
  prox_hi <- proximity_range[2]
  a <- search_interval[1]
  b <- search_interval[2]

  assertthat::assert_that(
    eval_across(list(reliability, prox_lo, prox_hi),
                fun = "~ dplyr::between(.x, 0, 1)"),
    msg = "prox_lo, prox_hi, and reliability must fall within the closed interval [0, 1]"
  )

  if (is.na(n_search)) {
    n_search <- max(round(n_confirm / 10), 1000)
  }

  found <- FALSE # has a solution been found?
  firstpass <- TRUE # is this the first pass through the bisection algorithm?
  checkrange <- TRUE # does the search interval range include the function root?
  ct <- 1

  if (verbose) {
    usethis::ui_info(paste0("Checking k = ", k))
  }

  while (found == FALSE) {
    if (verbose) {
      usethis::ui_info(
        glue::glue("Round:{ct}  a:{a}  b:{b}")
      )
    }

    # for wider search range, use smaller number of simulation iterations
    n_sim <- ifelse(b - a < 100, n_confirm, n_search)

    if (firstpass | checkrange) {
      fa <- estimate_reliability(a, k, prox_lo, prox_hi, n_sim, seed) - reliability
    }

    if (firstpass | checkrange) {
      fb <- estimate_reliability(b, k, prox_lo, prox_hi, n_sim, seed) - reliability
    }

    # check the search_interval limits before proceeding with the rest of the bisection algorithm
    if (firstpass & (abs(fa) < tolerance | abs(fb) < tolerance)) {
      if (n_sim == n_confirm) {
        found <- TRUE
        n <- ifelse(abs(fa) < abs(fb), a, b)
      } else {
        n <- ifelse(abs(fa) < abs(fb), a, b)
        fn <- estimate_reliability(n, k, prox_lo, prox_hi, n_confirm, seed) - reliability
        if (abs(fn) < tol) found <- TRUE
      }
    } else {
      firstpass <- FALSE

      # if initial search_interval is too low, shift upwards
      if (checkrange & (fa * fb > 0)) {
        a <- b
        b <- 2 * b
      } else {
        checkrange <- FALSE
        n <- ceiling((a + b) / 2)
        fn <- estimate_reliability(n, k, prox_lo, prox_hi, n_sim, seed) - reliability

        # check for difference within tolerance or one-observation wide sample_interval
        if (abs(fn) <= tolerance | (b - a <= 1)) {
          if (n_sim == n_confirm) {
            found <- TRUE
          }else{
            fn <- estimate_reliability(n, k, prox_lo, prox_hi, n_confirm, seed) - reliability
            if (abs(fn) <= tolerance | (b - a <= 1)) found <- TRUE
          }
        }
      }
    }

    if (!any(found, checkrange)) {
      if (fn > 0) {
        b <- n
      }else{
        a <- n
      }
    }

    ct <- ct + 1
  }

  # Generate plot
  if (plot) {
    samplingdist_plot <- plot_reliability(n,
                                          k,
                                          prox_lo,
                                          prox_hi,
                                          n_confirm,
                                          seed)
  }

  if (plot) {
    return(list(sample_size = n,
                k_constant = k,
                reliability_hat = fn + reliability,
                reliability_plot = samplingdist_plot))
  }else{
    return(list(sample_size = n,
                k_constant = k,
                reliability_hat = fn + reliability))
  }
}
