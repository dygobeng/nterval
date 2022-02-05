#' Find N for k-sigma sample interval
#'
#' @description Find required sample size for "k-sigma sample interval" (i.e., parametric sample
#'   interval applied to a Normal distribution) to achieve targeted reliability using bisection
#'   algorithm
#'
#' @param proximity_range Range within which sample coverage should fall with pre-specified
#'  \code{reliability}.
#' @param reliability Targeted probability that individual coverage of parametric sample interval
#'   applied to a Normal distribution falls within \code{proximity_range}.
#' @param k Standard deviation multiplier for parametric sample interval applied to Normal
#'  distribution (typically 2 or 3). If `NULL`, \code{k} is assigned based on the midpoint of the
#'  \code{proximity_range}.
#' @param search_interval Initial interval of sample sizes for bisection algorithm. Default is
#'   c(3, 500).
#' @param n_search Number of simulation iterations during initial search for root.
#' @param n_confirm Number of simulation iterations used to confirm \code{reliability}.
#' @param tolerance Threshold for difference between targeted and estimated \code{reliability} that
#'   indicates sufficient proximity of the root-finding function to 0, triggering the end of the
#'   search.
#' @param plot If `TRUE`, plot sampling distribution for true interval coverage using sample size
#'   identified through bisection.
#' @param verbose If `TRUE`, print the search interval used for each round of the bisection
#'   algorithm search.
#'
#' @return
#' A list with the following elements:
#' \itemize{
#'   \item The estimated sample size required to achieve target reliability
#'   \item If \code{plot} is `TRUE`, the plot object
#' }
#' @export
#'
#' @examples
n_ksigma <- function(proximity_range,
                          reliability,
                          k = NULL,
                          search_interval = c(3, 500),
                          n_search = NA,
                          n_confirm = 100000,
                          tol = 0.001,
                          plot = TRUE,
                          verbose = TRUE) {

  require(RColorBrewer)

  orange <- colorRampPalette(brewer.pal(8, "Dark2"))(8)[2]
  purple <- colorRampPalette(brewer.pal(8, "Purples"))(9)[7]

  # Error handling
  if (prox_lo <= 0 | prox_lo > 1) { # lower proximity limit outside 0 - 1
    stop("Invalid lower proximity limit: ", prox_lo)
  }

  if (prox_hi <= 0 | prox_hi > 1 | prox_hi <= prox_lo) { # upper proximity limit outside 0 - 1 or less than lower proximity limit
    stop("Invalid upper proximity limit: ", prox_hi)
  }

  if (is.na(n.search)) n.search <- max(round(n.confirm/10), 1000)

  found <- F # indicator for solution found
  first.pass <- T # indicator for first pass through bisection method
  range.check <- T # indicator for correct search range
  ct <- 1
  if (verbose) print(paste0("Checking k = ", k))
  while (found == F) {
    if (verbose) {
      print(paste("Round: ", ct,
                  " a: ", a,
                  " b: ", b, sep = ""))
    }

    # for wider search range, use smaller number of simulation iterations
    n.sim <- ifelse(b - a < 100, n.confirm, n.search)

    if (first.pass | range.check) {
      fa <- getReli(a, k, prox_lo, prox_hi, n.sim) - reli # reliability at n = a
    }

    if (first.pass | range.check) {
      fb <- getReli(b, k, prox_lo, prox_hi, n.sim) - reli # reliability at n = b
    }

    if (first.pass & (abs(fa) < tol | abs(fb) < tol)) { # check limits for n
      if (n.sim == n.confirm) {
        found <- T
        xn <- ifelse(abs(fa) < abs(fb), a, b)
      }else{
        xn <- ifelse(abs(fa) < abs(fb), a, b)
        fn <- getReli(xn, k, prox_lo, prox_hi, n.confirm) - reli
        if (abs(fn) < tol) found <- T
      }
    }else{
      first.pass <- F
      if (range.check & (fa * fb > 0)) {
        a <- b
        b <- 2 * b
      }else{
        range.check <- F
        xn <- ceiling((a + b)/2)
        fn <- getReli(xn, k, prox_lo, prox_hi, n.sim) - reli

        if (abs(fn) <= tol | (b - a <= 1)) {
          if (n.sim == n.confirm) {
            found <- T
          }else{
            fn <- getReli(xn, k, prox_lo, prox_hi, n.confirm) - reli
            if (abs(fn) <= tol | (b - a <= 1)) found <- T
          }
        }
      }
    }

    if (found == F & range.check == F) {
      if (fn > 0) {
        b <- xn
      }else{
        a <- xn
      }
    }

    ct <- ct + 1
  }

  # Generate plot
  if (plot) {
    plot.data <- getReli(xn, k, prox_lo, prox_hi, n.confirm, save.data = T)
    cov.plot <- ggplot() +
      scale_x_continuous(expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) +
      geom_histogram(data.frame(cov = plot.data$cov.data),
                     mapping = aes(x = cov),
                     fill    = "darkgray",
                     color   = "black") +
      labs(title = paste("Estimated N = ", xn,
                         "  Estimated Reliability = ",
                         round(100 * plot.data$est.reli,2),"%", sep = ""),
           x = "Actual Sample Interval Coverage",
           y = "Count")

    plot.info <- ggplot_build(cov.plot)$data[[1]]
    ymax <- max(plot.info$count) * 1.1

    final.plot <- cov.plot + coord_cartesian(y = c(0, ymax)) + # 500 x 400
      geom_rect(aes(xmin = prox_lo, xmax = prox_hi, ymin = 0, ymax = ymax),
                alpha = 0.5,
                fill = purple) +
      geom_vline(xintercept = c(prox_lo, prox_hi),
                 color      = orange,
                 size       = 1.3,
                 linetype   = "dashed")
  }

  if (plot) {
    return(list(xn, final.plot))
  }else{
    return(xn)
  }
}
