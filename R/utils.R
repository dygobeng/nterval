#' Calculate true coverage for Normal distribution
#'
#' @inheritParams n_ksigma
#' @param .data
#' @param z_upper Z-score for upper limit of interval
#' @param z_lower Z-score for lower limit of interval
#'
#' @return Proportion of standard Normal distribution enclosed within calculated or provided sample
#'   interval
#' @export
#'
#' @examples
calc_coverage_normal <- function(.data,
                                 k,
                                 z_upper = NULL,
                                 z_lower = NULL) {
  if (!is.null(.data)) {
    pnorm(mean(.data) + k * sd(.data)) - pnorm(mean(.data) - k * sd(.data))
  } else {
    pnorm(z_upper) - pnorm(z_lower)
  }
}
