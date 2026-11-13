# twdtw_knn1() must say what is wrong with its input.

test_that("x must be a three-dimensional stars object with a time dimension", {
  dc <- example_cube()
  samples <- example_samples()
  expected <- "x must be a three-dimensional stars object with a 'time' dimension"

  train <- function(x) {
    twdtw_knn1(x = x,
               y = samples,
               cycle_length = 'year',
               time_scale = 'day',
               time_weight = c(steepness = 0.1, midpoint = 50))
  }

  # The third dimension has another name
  expect_error(train(st_set_dimensions(dc, 3, names = "date")), expected, fixed = TRUE)

  # Four dimensions: the bands are a dimension and not attributes
  expect_error(train(merge(dc)), expected, fixed = TRUE)

  # Two dimensions: a single image
  expect_error(train(dc[, , , 1, drop = TRUE]), expected, fixed = TRUE)

  # Not a stars object
  expect_error(train(data.frame(a = 1)), expected, fixed = TRUE)
})
