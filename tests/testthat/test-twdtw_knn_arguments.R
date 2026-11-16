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

test_that("keep_models = FALSE keeps the root mean squared error of the models and not the models", {
  mean_fun <- function(data) lm(y ~ factor(x), data = data)
  m <- train(resampling_fun = mean_fun)
  m_small <- train(resampling_fun = mean_fun, keep_models = FALSE)

  # The models of all labels and bands, or only their root mean squared error
  expect_named(m$smooth_models, m$data$label)
  expect_null(m_small$smooth_models)
  expect_identical(dimnames(m$rmse), list(names(example_cube()), m$data$label))
  expect_identical(m_small$rmse, m$rmse)
  expect_lt(as.numeric(object.size(m_small)), as.numeric(object.size(m)) / 10)

  # The same temporal patterns and the same map
  expect_identical(m_small$data, m$data)
  dc <- example_cube()[, 1:6, 1:5, ]
  expect_equal(predict(m_small, newdata = dc), predict(m, newdata = dc))

  # The function of earlier versions passes the argument on
  m_knn1 <- suppressMessages(twdtw_knn1(x = example_cube(), y = example_samples(), resampling_fun = mean_fun,
                                        keep_models = FALSE, cycle_length = 'year', time_scale = 'day',
                                        time_weight = c(steepness = 0.1, midpoint = 50)))
  expect_null(m_knn1$smooth_models)
  expect_identical(m_knn1$data, m$data)

  expect_error(train(resampling_fun = mean_fun, keep_models = NA), "'keep_models' must be TRUE or FALSE", fixed = TRUE)
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
               "Unknown arguments in twdtw_knn: 'max_elasped'. The additional arguments are 'origin', 'max_elapsed', and 'keep_models'.",
               fixed = TRUE)

  # The number of nearest neighbours is an argument of predict
  expect_error(train(k = 3), "Unknown arguments in twdtw_knn: 'k'", fixed = TRUE)

  # An argument without name after all arguments of the function
  expect_error(train(NULL, NULL, 'start_date', 'end_date', 'label', 5),
               "Unknown arguments in twdtw_knn: an argument without name", fixed = TRUE)
})
