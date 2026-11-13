# A data cube with a single band. After the time dimension is split, the names of
# the time series have only the date and not the name of the band
# (https://github.com/vwmaus/twdtw/issues/7).

train <- function(x, y, ...) {
  twdtw_knn1(x = x,
             y = y,
             ...,
             cycle_length = 'year',
             time_scale = 'day',
             time_weight = c(steepness = 0.1, midpoint = 50))
}

test_that("a model is trained with a data cube that has a single band", {
  dc <- example_cube()
  samples <- example_samples()

  m <- train(dc["EVI"], samples)
  m_all_bands <- train(dc, samples)

  # The time series have the name of the band of the data cube
  expect_equal(m$data$label, samples$label)
  for (i in c(1, 100, nrow(samples))) {
    expect_named(m$data$observations[[i]], c("time", "EVI"))
    expect_equal(m$data$observations[[i]]$time, m_all_bands$data$observations[[i]]$time)
    expect_equal(m$data$observations[[i]]$EVI, m_all_bands$data$observations[[i]]$EVI)
  }

  # Temporal patterns: the mean of the samples on each date
  mean_fun <- function(data) lm(y ~ factor(x), data = data)
  m_pattern <- train(dc["EVI"], samples, resampling_fun = mean_fun)
  m_pattern_all_bands <- train(dc, samples, resampling_fun = mean_fun)

  expect_equal(m_pattern$data$label, m_pattern_all_bands$data$label)
  for (i in seq_along(m_pattern$data$label)) {
    expect_named(m_pattern$data$observations[[i]], c("time", "EVI"))
    expect_equal(m_pattern$data$observations[[i]]$EVI, m_pattern_all_bands$data$observations[[i]]$EVI)
  }

  expect_s3_class(ggplot_build(plot(m_pattern)), "ggplot_built")
})

test_that("a data cube with a single band is classified", {
  dc <- example_cube()
  dc_evi <- dc["EVI"]
  m <- train(dc_evi, example_samples(), resampling_fun = function(data) lm(y ~ factor(x), data = data))

  # A part of the data cube keeps the test fast
  dc_part <- dc_evi[, 1:6, 1:6, ]
  lu <- predict(m, newdata = dc_part)
  lu_stars <- suppressMessages(predict(dc_part, model = m))

  expect_named(lu, c("prediction", "distance"))
  expect_equal(dim(lu), c(x = 6, y = 6))
  expect_false(anyNA(lu$prediction))
  expect_equal(lu_stars, lu)

  # Distance of the first pixel to each pattern, calculated with twdtw()
  ts <- data.frame(time = st_get_dimension_values(dc_part, "time"), EVI = dc_part$EVI[1, 1, ])
  distance <- vapply(m$data$observations, function(pattern) {
    as.numeric(twdtw(x = ts,
                     y = as.data.frame(pattern),
                     cycle_length = 'year',
                     time_scale = 'day',
                     time_weight = c(steepness = 0.1, midpoint = 50)))
  }, numeric(1))
  expect_equal(as.character(lu$prediction[1, 1]), m$data$label[which.min(distance)])
  expect_equal(lu$distance[1, 1], min(distance))
})

test_that("band names with a hyphen give the same map with both ways to classify", {
  dc <- example_cube()
  samples <- example_samples()
  mean_fun <- function(data) lm(y ~ factor(x), data = data)

  # Two bands
  dc_two <- setNames(dc[c("EVI", "NDVI")], c("EVI-16", "NDVI-16"))
  m <- train(dc_two, samples, resampling_fun = mean_fun)
  dc_part <- dc_two[, 1:6, 1:6, ]
  expect_equal(predict(m, newdata = dc_part), suppressMessages(predict(dc_part, model = m)))

  # The same map as with the bands without a hyphen
  m_plain <- train(dc[c("EVI", "NDVI")], samples, resampling_fun = mean_fun)
  lu_plain <- predict(m_plain, newdata = dc[c("EVI", "NDVI")][, 1:6, 1:6, ])
  expect_equal(as.vector(predict(m, newdata = dc_part)$prediction), as.vector(lu_plain$prediction))

  # A single band
  dc_one <- setNames(dc["EVI"], "EVI-16")
  m <- train(dc_one, samples, resampling_fun = mean_fun)
  dc_part <- dc_one[, 1:6, 1:6, ]
  expect_equal(predict(m, newdata = dc_part), suppressMessages(predict(dc_part, model = m)))
})

test_that("time series with only the date in their names need a model with a single band", {
  dc <- example_cube()
  samples <- example_samples()
  m <- train(dc, samples, resampling_fun = function(data) lm(y ~ factor(x), data = data))

  # Time series of a single band, as stars passes them to predict
  newdata <- as.data.frame(split(dc["EVI"][, 1:3, 1:2, ], "time"))

  # The model has six bands
  expect_error(predict(m, newdata = newdata),
               "newdata has no columns named with a band and a date",
               fixed = TRUE)
})
