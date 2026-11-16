# twdtw_knn1() and models of class 'twdtw_knn1' are from earlier versions of the package.
# twdtw_knn1() is deprecated, and models of this class are classified with k = 1.

# The messages about them are given once per session
reset_messages <- function() {
  session <- get(".dtwSat_session", envir = asNamespace("dtwSat"))
  rm(list = ls(session), envir = session)
}

test_that("twdtw_knn1 creates the model of twdtw_knn and gives a message once per session", {
  dc <- example_cube()
  samples <- example_samples()
  mean_fun <- function(data) lm(y ~ factor(x), data = data)

  reset_messages()
  expect_message(m_old <- twdtw_knn1(x = dc,
                                     y = samples,
                                     resampling_fun = mean_fun,
                                     cycle_length = 'year',
                                     time_scale = 'day',
                                     time_weight = c(steepness = 0.1, midpoint = 50)),
                 "twdtw_knn1() is deprecated and will be removed in a future release of dtwSat. Use twdtw_knn()",
                 fixed = TRUE)

  m <- twdtw_knn(x = dc,
                 y = samples,
                 resampling_fun = mean_fun,
                 cycle_length = 'year',
                 time_scale = 'day',
                 time_weight = c(steepness = 0.1, midpoint = 50))

  # The same model, with the class of this version
  expect_identical(class(m_old), "twdtw_knn")
  expect_equal(m_old$data, m$data)
  expect_equal(m_old$twdtw_args, m$twdtw_args)
  expect_identical(m_old$call[[1]], as.name("twdtw_knn1"))
  expect_identical(m$call[[1]], as.name("twdtw_knn"))

  # The message is not repeated, and the arguments have the order of twdtw_knn()
  expect_no_message(m_position <- twdtw_knn1(dc, samples, c(steepness = 0.1, midpoint = 50), 'year', 'day', mean_fun))
  expect_equal(m_position$data, m$data)

  # twdtw_knn1() is a function for data cubes
  expect_error(twdtw_knn1(x = data.frame(a = 1),
                          y = samples,
                          cycle_length = 'year',
                          time_scale = 'day',
                          time_weight = c(steepness = 0.1, midpoint = 50)),
               "x must be a three-dimensional stars object with a 'time' dimension", fixed = TRUE)
  reset_messages()
})

test_that("a model of class twdtw_knn1 is classified with k = 1 and a message", {
  dc <- example_cube()
  m <- twdtw_knn(x = dc,
                 y = example_samples(),
                 resampling_fun = function(data) lm(y ~ factor(x), data = data),
                 cycle_length = 'year',
                 time_scale = 'day',
                 time_weight = c(steepness = 0.1, midpoint = 50))
  dc_part <- dc[, 1:4, 1:4, ]

  # Model with the class of version 1.0.0
  m_old <- m
  class(m_old) <- "twdtw_knn1"

  # The message is given once per session
  reset_messages()
  expect_message(lu_old <- predict(m_old, newdata = dc_part),
                 "predict.twdtw_knn1() will be deprecated in a future release of dtwSat",
                 fixed = TRUE)
  expect_no_message(predict(m_old, newdata = dc_part))
  expect_equal(lu_old, predict(m, newdata = dc_part))

  # stars::predict() finds the method for this class
  expect_equal(suppressMessages(predict(dc_part, model = m_old)), lu_old)

  # The model created by this version does not give the message
  reset_messages()
  expect_no_message(predict(m, newdata = dc_part))

  # Other values of k need a model of class twdtw_knn
  expect_error(predict(m_old, newdata = dc_part, k = 2),
               "predict() uses k = 1 for a model of class 'twdtw_knn1'", fixed = TRUE)

  # The model is printed and plotted as a model of this version
  out <- capture.output(printed <- print(m_old))
  expect_identical(printed, m_old)
  expect_match(out, "Model of class 'twdtw_knn'", fixed = TRUE, all = FALSE)
  expect_s3_class(plot(m_old), "ggplot")
  reset_messages()
})

test_that("a model created with version 1.0.0 gives the map of version 1.0.0", {
  # A model created with dtwSat 1.0.0 and twdtw 1.0-1 from the example data, with the
  # labels that this version predicted for a part of the example data cube
  old <- readRDS(test_path("model_dtwSat_1.0.0.rds"))
  expect_identical(class(old$model), "twdtw_knn1")
  expect_named(old$model, c("call", "formula", "data", "twdtw_args"))

  dc_part <- example_cube()[, old$x, old$y, ]

  reset_messages()
  expect_message(lu <- predict(old$model, newdata = dc_part),
                 "predict.twdtw_knn1() will be deprecated in a future release of dtwSat",
                 fixed = TRUE)
  expect_named(lu, c("prediction", "distance"))
  expect_identical(as.character(lu$prediction), old$prediction)
  expect_equal(suppressMessages(predict(dc_part, model = old$model)), lu)

  # The model is printed and plotted
  expect_match(capture.output(print(old$model)), "Model of class 'twdtw_knn'", fixed = TRUE, all = FALSE)
  expect_s3_class(ggplot_build(plot(old$model)), "ggplot_built")
  reset_messages()
})
