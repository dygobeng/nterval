dummy_seed <- 12345
dummy_prox <- c(0.93, 0.97)
dummy_n <- 69
dummy_k <- 2
dummy_nsim <- 1000

expected_result_names <- c("reliability_hat",
                           "sim_coverage")

expected_reliability <- 0.7

test_that("estimate_reliability returns correct output", {
  test_result <- estimate_reliability(n = dummy_n,
                                      k = dummy_k,
                                      prox_lo = dummy_prox[1],
                                      prox_hi = dummy_prox[2],
                                      n_sim = dummy_nsim)

  test_result_save <- estimate_reliability(n = dummy_n,
                                      k = dummy_k,
                                      prox_lo = dummy_prox[1],
                                      prox_hi = dummy_prox[2],
                                      n_sim = dummy_nsim,
                                      save_data = TRUE)

  expect_equal(round(test_result, 1), expected_reliability)
  expect_equal(names(test_result_save), expected_result_names)
})

test_that("estimate_reliability seed returns identical results", {
  test_result1 <- estimate_reliability(n = dummy_n,
                                       k = dummy_k,
                                       prox_lo = dummy_prox[1],
                                       prox_hi = dummy_prox[2],
                                       n_sim = dummy_nsim,
                                       save_data = TRUE,
                                       seed = dummy_seed)

  test_result2 <- estimate_reliability(n = dummy_n,
                                       k = dummy_k,
                                       prox_lo = dummy_prox[1],
                                       prox_hi = dummy_prox[2],
                                       n_sim = dummy_nsim,
                                       save_data = TRUE,
                                       seed = dummy_seed)

  expect_identical(test_result1, test_result2)
})

test_that("estimate_reliability correctly errors for non-valid sample sizes", {
  expect_error(
    estimate_reliability(n = 20.5,
                         k = 2,
                         prox_lo = 0.93,
                         prox_hi = 0.97),
    "* must be a whole number greater than or equal to 3"
  )

  expect_error(
    estimate_reliability(n = 2,
                         k = 2,
                         prox_lo = 0.93,
                         prox_hi = 0.97),
    "* must be a whole number greater than or equal to 3"
  )
})

test_that("estimate_reliability correctly errors for non-numeric arguments", {
  expect_error(
    estimate_reliability(n = "69",
                         k = 2,
                         prox_lo = 0.93,
                         prox_hi = 0.97),
    "* must be numeric"
  )

  expect_error(
    estimate_reliability(n = 69,
                         k = "2",
                         prox_lo = 0.93,
                         prox_hi = 0.97),
    "* must be numeric"
  )

  expect_error(
    estimate_reliability(n = 69,
                         k = 2,
                         prox_lo = "0.93",
                         prox_hi = 0.97),
    "* must be numeric"
  )

  expect_error(
    estimate_reliability(n = 69,
                         k = 2,
                         prox_lo = 0.93,
                         prox_hi = "0.97"),
    "* must be numeric"
  )
})

test_that("estimate_reliability correctly errors for proximity_ranges outside [0,1]", {
  expect_error(
    estimate_reliability(n = 69,
                         k = 2,
                         prox_lo = -0.93,
                         prox_hi = 0.97),
    "* must fall within the closed interval \\[0, 1\\]"
  )

  expect_error(
    estimate_reliability(n = 69,
                         k = 2,
                         prox_lo = 0.93,
                         prox_hi = 1.97),
    "* must fall within the closed interval \\[0, 1\\]"
  )
})

test_that("estimate_reliability correctly errors for prox_lo greater than or equal to prox_hi", {
  expect_error(
    estimate_reliability(n = 69,
                         k = 2,
                         prox_lo = 0.97,
                         prox_hi = 0.93),
    "prox_lo must be less than prox_hi"
  )
})
