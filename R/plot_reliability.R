#' Plot sample coverage sampling distribution
#'
#' @inheritParams estimate_reliability
#' @param color_list List object of package colors for plotting.
#'
#' @return ggplot2 histogram with highlighted acceptable coverage region
plot_reliability <- function(n,
                             k,
                             prox_lo,
                             prox_hi,
                             n_sim,
                             seed,
                             color_list = color_palette) {
  rlang::check_installed("ggplot2", reason = "to use `plot_reliability()`")

  # determine histogram breaks to ensure proximity limits fall between bins and desired resolution
  # of bins within proximity range is achieved
  RESOLUTION <- 10
  step <- (prox_hi - prox_lo) / RESOLUTION
  all_break_values <- seq(0, 1, by = step)
  delta <- prox_lo - all_break_values
  shift <- delta[which(abs(delta) == min(abs(delta)))]
  all_break_values <- all_break_values + shift

  reliability_list <-
    estimate_reliability(
      n,
      k,
      prox_lo,
      prox_hi,
      n_sim,
      seed,
      save_data = TRUE
    )
  plot_data <- data.frame(cov = reliability_list[["sim_coverage"]])
  break_values <- c(all_break_values[which(all_break_values > min(plot_data$cov))], 1)

  ggplot2::ggplot() +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::geom_histogram(
      plot_data,
      mapping = ggplot2::aes(x = cov),
      breaks = break_values,
      fill = "white",
      color = "black",
      size = 0.1
    ) +
    ggplot2::geom_histogram(
      subset(
        plot_data,
        cov >= prox_lo & cov <= prox_hi
      ),
      mapping = ggplot2::aes(x = cov),
      breaks = break_values,
      fill = color_list[["lightblue"]],
      color = "black",
      size = 0.1
    ) +
    ggplot2::geom_vline(
      xintercept = c(
        calc_coverage_normal(z_upper = k, z_lower = -k),
        prox_lo,
        prox_hi
      ),
      color = c(
        color_list[["teal"]],
        color_list[["darkblue"]],
        color_list[["darkblue"]]
      ),
      size = 1,
      linetype = c("solid", "dashed", "dashed")
    ) +
    ggplot2::labs(
      title = paste0(
        "k = ", k,
        "  Estimated N = ", n,
        "  Estimated Reliability = ",
        round(100 * reliability_list[["reliability_hat"]], 2), "%"
      ),
      x = "True Population Coverage Based on Sample Intervals",
      y = "Count"
    ) +
    ggplot2::theme_bw()
}
