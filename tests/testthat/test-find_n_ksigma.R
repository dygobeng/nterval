dummy_seed <- 12345
dummy_prox <- c(0.93, 0.97)
dummy_reli <- 0.7
dummy_k <- 2
dummy_nsim <- 1000

expected_result_names <- c("sample_size",
                           "k_constant",
                           "reliability_hat")
expected_result_names_plot <- c(expected_result_names,
                                "reliability_plot")

test_that("find_n_ksigma returns sample size, k, and reliability", {
  test_result <- find_n_ksigma(proximity_range = dummy_prox,
                               reliability = dummy_reli,
                               k = dummy_k,
                               n_confirm = dummy_nsim)

  expect_equal(names(test_result), expected_result_names)
})

test_that("find_n_ksigma additionally returns plot when plot = TRUE", {
  test_result_plot <- find_n_ksigma(proximity_range = dummy_prox,
                                    reliability = dummy_reli,
                                    k = dummy_k,
                                    n_confirm = dummy_nsim,
                                    plot = TRUE)

  expect_equal(names(test_result_plot), expected_result_names_plot)
})

test_that("find_n_ksigma returns messaging when verbose = TRUE", {
  test_result_verbose <- capture_messages(
    find_n_ksigma(proximity_range = dummy_prox,
                  reliability = dummy_reli,
                  k = dummy_k,
                  n_confirm = dummy_nsim,
                  verbose = TRUE)
  )[1:2]

  expect_all_messages(test_result_verbose,
                      c("Checking k = 2",
                        "Round:1  a:3  b:500"))
})

test_that("find_n_ksigma seed returns identical results", {
  test_result1 <- find_n_ksigma(proximity_range = dummy_prox,
                                reliability = dummy_reli,
                                k = dummy_k,
                                n_confirm = dummy_nsim,
                                seed = dummy_seed)

  test_result2 <- find_n_ksigma(proximity_range = dummy_prox,
                                reliability = dummy_reli,
                                k = dummy_k,
                                n_confirm = dummy_nsim,
                                seed = dummy_seed)

  expect_identical(test_result1, test_result2)
})

test_that("find_n_ksigma correctly errors for non-numeric arguments", {
  expect_error(
    find_n_ksigma(proximity_range = c("0.93", 0.97),
                  reliability = 0.7),
    "* must be numeric"
  )

  expect_error(
    find_n_ksigma(proximity_range = c(0.93, 0.97),
                  reliability = "0.7"),
    "* must be numeric"
  )

  expect_error(
    find_n_ksigma(proximity_range = c(0.93, 0.97),
                  reliability = 0.7,
                  search_interval = c("3", 500)),
    "* must be numeric"
  )

  expect_error(
    find_n_ksigma(proximity_range = c(0.93, 0.97),
                  reliability = 0.7,
                  k = "2"),
    "* must be numeric"
  )
})

test_that("find_n_ksigma correctly errors for vector arguments of incorrect length", {
  expect_error(
    find_n_ksigma(proximity_range = 0.93,
                  reliability = 0.7),
    "* must be vectors with 2 elements"
  )

  expect_error(
    find_n_ksigma(proximity_range = c(0.93, 0.97),
                  reliability = 0.7,
                  search_interval = 500),
    "* must be vectors with 2 elements"
  )
})

test_that("find_n_ksigma correctly errors for proximity_ranges or reliability outside [0,1]", {
  expect_error(
    find_n_ksigma(proximity_range = c(0.9, 1.1),
                  reliability = 0.7),
    "* must fall within the closed interval \\[0, 1\\]"
  )

  expect_error(
    find_n_ksigma(proximity_range = c(0.93, 0.97),
                  reliability = -0.5),
    "* must fall within the closed interval \\[0, 1\\]"
  )
})

test_that("find_n_ksigma correctly errors for proximity_ranges or search_interval in desc order", {
  expect_error(
    find_n_ksigma(proximity_range = c(0.97, 0.93),
                  reliability = 0.7,
                  search_interval = c(3, 500)),
    "* must be in ascending order"
  )

  expect_error(
    find_n_ksigma(proximity_range = c(0.93, 0.97),
                  reliability = 0.7,
                  search_interval = c(500, 3)),
    "* must be in ascending order"
  )
})
