# twdtw_knn() does not ignore arguments that it does not know.

train <- function(...) {
  twdtw_knn(x = example_cube(),
            y = example_samples(),
            ...,
            cycle_length = 'year',
            time_scale = 'day',
            time_weight = c(steepness = 0.1, midpoint = 50))
}

test_that("the additional arguments of the TWDTW distance are kept in the model", {
  m <- train()
  expect_null(m$twdtw_args$origin)
  expect_equal(m$twdtw_args$max_elapsed, Inf)

  m <- train(max_elapsed = 30)
  expect_equal(m$twdtw_args$max_elapsed, 30)
})

test_that("arguments of version 1.0.0 are an error", {
  # They were replaced by resampling_fun and resampling_freq
  expect_error(train(formula = band ~ s(time)),
               "Unknown arguments in twdtw_knn: 'formula'", fixed = TRUE)
  expect_error(train(formula = band ~ s(time)),
               "'formula' and 'sampling_freq' were replaced by 'resampling_fun' and 'resampling_freq'", fixed = TRUE)
  expect_error(train(sampling_freq = 16),
               "Unknown arguments in twdtw_knn: 'sampling_freq'", fixed = TRUE)

  # The version of the twdtw routine is no longer an argument
  expect_error(train(version = 'f90'), "'version' is no longer used", fixed = TRUE)
})

test_that("misspelled and unnamed arguments are an error", {
  expect_error(train(max_elasped = 30),
               "Unknown arguments in twdtw_knn: 'max_elasped'. The additional arguments are 'origin' and 'max_elapsed'.",
               fixed = TRUE)

  # An argument without name after all arguments of the function
  expect_error(train(NULL, NULL, 'start_date', 'end_date', 'label', 5),
               "Unknown arguments in twdtw_knn: an argument without name", fixed = TRUE)
})
