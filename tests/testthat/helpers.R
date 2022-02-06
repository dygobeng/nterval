`%>%` <- magrittr::`%>%` # nolint

expect_all_messages <- function(obs_message, exp_message) {
  exp_message <- stringr::str_replace_all(exp_message, "\\(", "\\\\(")
  exp_message <- stringr::str_replace_all(exp_message, "\\)", "\\\\)")

  purrr::map2_lgl(
    .x = obs_message,
    .y = exp_message,
    .f = ~ stringr::str_detect(.x, .y)
  ) %>%
    all(.) %>%
    testthat::expect_true(.)
}
