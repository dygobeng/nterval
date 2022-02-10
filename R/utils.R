#' Calculate true coverage for Normal distribution
#'
#' @inheritParams estimate_reliability
#' @param x Data from which to calculate the sample mean and standard deviation.
#' @param z_upper Z-score for upper limit of interval.
#' @param z_lower Z-score for lower limit of interval.
#'
#' @details User must provide either `x` and `k` OR `z_upper` and `z_lower`.
#'
#' @return Proportion of standard Normal distribution enclosed within calculated or provided sample
#'   interval
calc_coverage_normal <- function(x = NULL,
                                 k = NULL,
                                 z_upper = NULL,
                                 z_lower = NULL) {
  assertthat::assert_that(
    !any(is.null(x) & is.null(k)) | !any(is.null(z_upper) & is.null(z_lower)),
    msg = "Must provide either x and k OR z_upper and z_lower"
  )

  if (!is.null(x)) {
    pnorm(mean(x) + k * sd(x)) - pnorm(mean(x) - k * sd(x))
  } else {
    pnorm(z_upper) - pnorm(z_lower)
  }
}


#' Evaluate function across list elements
#'
#' @param list_object List of objects.
#' @param fun String representing function that returns `TRUE` or `FALSE` to be evaluated across
#'   `list_object` elements.
#'
#' @return `TRUE` if all elements of `list_object` return `TRUE` when evaluated by `fun`; otherwise,
#'   `FALSE`
eval_across <- function(list_object,
                        fun) {
  fun_value <- parse(text = fun)
  purrr::map_lgl(
    .x = list_object,
    .f = eval(fun_value)
  ) %>%
    all()
}
