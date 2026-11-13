# Train TWDTW 1-NN models with the example data and classify the example data cube.

# Train a model with the twdtw arguments used in the examples of the package
train <- function(x, y, ..., time_weight = c(steepness = 0.1, midpoint = 50)) {
  twdtw_knn1(x = x,
             y = y,
             ...,
             cycle_length = 'year',
             time_scale = 'day',
             time_weight = time_weight)
}

# Classify a data cube. The message that is suppressed comes from stars: it first tries
# to predict with the three-dimensional data cube and then splits the time dimension.
classify <- function(cube, model, ...) {
  suppressMessages(predict(cube, model = model, ...))
}

# Share of the samples for which the map has the label of the sample
agreement <- function(map, samples) {
  mean(as.character(st_extract(map, samples)$prediction) == samples$label)
}

# A land use map has the spatial dimensions of the data cube and labels of the samples
expect_land_use_map <- function(map, cube, samples) {
  expect_s3_class(map, "stars")
  expect_equal(dim(map), dim(cube)[c("x", "y")])
  expect_s3_class(map$prediction, "factor")
  expect_true(all(levels(map$prediction) %in% samples$label))
}

test_that("a model without resampling keeps the time series of each sample", {
  dc <- example_cube()
  samples <- example_samples()

  m <- train(dc, samples)

  expect_s3_class(m, "twdtw_knn1")
  expect_null(m$smooth_models)
  expect_equal(m$data$label, samples$label)

  # Each time series has the 23 images of the data cube and all bands
  expect_true(all(vapply(m$data$observations, nrow, integer(1)) == 23))
  expect_true(all(vapply(m$data$observations, function(ts) {
    identical(names(ts), c("time", names(dc)))
  }, logical(1))))

  # The summary returns the model
  out <- capture.output(printed <- print(m))
  expect_identical(printed, m)
  expect_match(out, "Model of class 'twdtw_knn1'", fixed = TRUE, all = FALSE)

  # The plot has one line for each sample and band
  p <- plot(m)
  expect_s3_class(p, "ggplot")
  expect_s3_class(ggplot_build(p), "ggplot_built")
  expect_equal(length(unique(p$data$id)), nrow(samples))
  expect_setequal(p$data$band, names(dc))

  # The plot has only the bands that are given
  p <- plot(m, bands = c('EVI', 'NDVI'))
  expect_setequal(p$data$band, c('EVI', 'NDVI'))
})

test_that("each sample of a model without resampling gets its own label", {
  dc <- example_cube()
  samples <- example_samples()

  # Two samples of each label keep the test fast
  two_of_each <- unlist(lapply(split(seq_len(nrow(samples)), samples$label), head, 2))
  samples <- samples[two_of_each, ]

  m <- train(dc, samples)
  lu <- classify(dc, m)

  expect_land_use_map(lu, dc, samples)

  # The nearest neighbour of the pixel of a sample is the sample itself
  expect_equal(as.character(st_extract(lu, samples)$prediction), samples$label)
})

test_that("a resampling function gives one temporal pattern for each label", {
  skip_if_not_installed("mgcv")

  dc <- example_cube()
  samples <- example_samples()

  m <- train(dc, samples, resampling_fun = function(data) mgcv::gam(y ~ s(x), data = data))

  # One pattern for each label, from one fitted model for each band
  expect_equal(sort(m$data$label), sort(unique(samples$label)))
  expect_named(m$smooth_models, m$data$label)
  for (band_models in m$smooth_models) {
    expect_named(band_models, names(dc))
  }

  # The patterns have the dates of the images, moved to start in 1970
  pattern_dates <- shift_dates(st_get_dimension_values(dc, "time"))
  for (pattern in m$data$observations) {
    expect_named(pattern, c("time", names(dc)))
    expect_equal(pattern$time, pattern_dates)
    expect_false(anyNA(pattern))
  }

  # The summary has the error of the fitted models of each band
  out <- capture.output(print(m))
  expect_match(out, "Root Mean Squared Error (RMSE) of smooth models", fixed = TRUE, all = FALSE)
  expect_match(out, "^EVI +[0-9.]+", all = FALSE)

  # The plot has one line for each label and band
  p <- plot(m)
  expect_s3_class(ggplot_build(p), "ggplot_built")
  expect_equal(length(unique(p$data$id)), nrow(m$data))

  # The map has the label of the sample for 97% of the samples
  lu <- classify(dc, m)
  expect_land_use_map(lu, dc, samples)
  expect_gt(agreement(lu, samples), 0.9)
})

test_that("resampling_freq sets the days between the observations of the patterns", {
  skip_if_not_installed("mgcv")

  dc <- example_cube()
  samples <- example_samples()

  m <- train(dc, samples,
             resampling_fun = function(data) mgcv::gam(y ~ s(x), data = data),
             resampling_freq = 60)

  # The images cover 349 days: six observations that are 60 days apart
  for (pattern in m$data$observations) {
    expect_equal(pattern$time, as.Date("1970-09-14") + 60 * 0:5)
  }

  # Time series with 23 observations are compared to patterns with 6 observations.
  # This crashed R with twdtw 1.0-1 (https://github.com/r-spatial/dtwSat/issues/63).
  # The map has the label of the sample for 86% of the samples.
  lu <- classify(dc, m)
  expect_land_use_map(lu, dc, samples)
  expect_gt(agreement(lu, samples), 0.75)
})

test_that("a custom resampling function gives the mean of the samples on each date", {
  dc <- example_cube()
  samples <- example_samples()

  m <- train(dc, samples, resampling_fun = function(data) lm(y ~ factor(x), data = data))
  m_samples <- train(dc, samples)

  # All samples have the same dates, so a pattern is the mean of the samples of its label
  for (i in seq_along(m$data$label)) {
    ts_label <- m_samples$data$observations[m_samples$data$label == m$data$label[i]]
    for (band in names(dc)) {
      mean_band <- rowMeans(sapply(ts_label, function(ts) ts[[band]]))
      expect_equal(m$data$observations[[i]][[band]], mean_band)
    }
  }

  # The map has the label of the sample for 97% of the samples
  lu <- classify(dc, m)
  expect_land_use_map(lu, dc, samples)
  expect_gt(agreement(lu, samples), 0.9)
})

test_that("twdtw arguments given to predict replace the arguments of the model", {
  dc <- example_cube()
  samples <- example_samples()
  mean_fun <- function(data) lm(y ~ factor(x), data = data)
  other_weight <- c(steepness = 0.5, midpoint = 20)

  m <- train(dc, samples, resampling_fun = mean_fun)
  m_other <- train(dc, samples, resampling_fun = mean_fun, time_weight = other_weight)

  # The time weight is kept in the model and has no effect on the patterns
  expect_equal(m_other$twdtw_args$time_weight, other_weight)
  expect_equal(m_other$data, m$data)

  # A part of the data cube is enough to compare the maps
  dc_part <- dc[, 1:10, 1:10, ]
  lu <- classify(dc_part, m)
  lu_other <- classify(dc_part, m_other)

  # The two time weights give different maps
  expect_false(identical(lu, lu_other))

  # The first model with the other time weight gives the map of the other model
  expect_equal(classify(dc_part, m, time_weight = other_weight), lu_other)
})
