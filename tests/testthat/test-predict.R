# predict() gives each time series the label of the nearest temporal pattern
# and the TWDTW distance to this pattern.

# Model with one pattern for each label: the mean of the samples on each date
example_model <- function(dc, samples, ..., time_weight = c(steepness = 0.1, midpoint = 50)) {
  twdtw_knn(x = dc,
            y = samples,
            resampling_fun = function(data) lm(y ~ factor(x), data = data),
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

# Distance of a time series to each pattern of a model, calculated with twdtw()
distance_to_patterns <- function(ts, model, ...) {
  vapply(model$data$observations, function(pattern) {
    as.numeric(twdtw(x = ts,
                     y = as.data.frame(pattern),
                     ...,
                     cycle_length = 'year',
                     time_scale = 'day',
                     time_weight = c(steepness = 0.1, midpoint = 50)))
  }, numeric(1))
}

test_that("the map has the label of the nearest pattern and the distance to it", {
  dc <- example_cube()
  m <- example_model(dc, example_samples())

  # A part of the data cube keeps the test fast
  dc_part <- dc[, 1:6, 1:6, ]
  lu <- classify(dc_part, m)

  expect_s3_class(lu, "stars")
  expect_named(lu, c("prediction", "distance"))
  expect_equal(dim(lu), dim(dc_part)[c("x", "y")])

  # Distance of each pixel to each pattern, calculated one pair at a time
  image_dates <- st_get_dimension_values(dc_part, "time")
  expected_label <- character()
  expected_distance <- numeric()
  for (j in 1:6) {
    for (i in 1:6) {
      ts <- data.frame(time = image_dates, lapply(dc_part, function(band) band[i, j, ]))
      distance <- distance_to_patterns(ts, m)
      expected_label <- c(expected_label, m$data$label[which.min(distance)])
      expected_distance <- c(expected_distance, min(distance))
    }
  }

  expect_equal(as.character(lu$prediction), expected_label)
  expect_equal(as.vector(lu$distance), expected_distance)
})

test_that("a time weight function gives the map of the logistic time weight", {
  dc <- example_cube()
  samples <- example_samples()
  dc_part <- dc[, 1:6, 1:6, ]

  m <- example_model(dc, samples)
  m_function <- example_model(dc, samples, time_weight = function(dist, el) {
    dist + 1 / (1 + exp(-0.1 * (el - 50)))
  })

  expect_equal(classify(dc_part, m_function), classify(dc_part, m))
})

test_that("max_elapsed limits the time between the observations that are compared", {
  dc <- example_cube()
  samples <- example_samples()
  dc_part <- dc[, 1:6, 1:6, ]

  m <- example_model(dc, samples)
  m_elapsed <- example_model(dc, samples, max_elapsed = 30)

  lu <- classify(dc_part, m)
  lu_elapsed <- classify(dc_part, m_elapsed)

  # The limit changes the distances
  expect_false(isTRUE(all.equal(lu_elapsed$distance, lu$distance)))

  # Distance of the first pixel, calculated one pair at a time
  ts <- data.frame(time = st_get_dimension_values(dc_part, "time"),
                   lapply(dc_part, function(band) band[1, 1, ]))
  expect_equal(lu_elapsed$distance[1, 1], min(distance_to_patterns(ts, m, max_elapsed = 30)))

  # The limit can also be given to predict
  expect_equal(classify(dc_part, m, max_elapsed = 30), lu_elapsed)
})

test_that("the spatial dimensions of the data cube can have other names", {
  dc <- example_cube()
  m <- example_model(dc, example_samples())
  dc_part <- dc[, 1:6, 1:6, ]

  lu <- classify(dc_part, m)
  lu_lon_lat <- classify(st_set_dimensions(dc_part, names = c("lon", "lat", "time")), m)

  expect_equal(dim(lu_lon_lat), c(lon = 6, lat = 6))
  expect_equal(as.vector(lu_lon_lat$prediction), as.vector(lu$prediction))
  expect_equal(as.vector(lu_lon_lat$distance), as.vector(lu$distance))
})

test_that("observations with missing values are not used", {
  dc <- example_cube()
  m <- example_model(dc, example_samples())

  # Three time series in a data frame with one column for each band and date,
  # as stars passes them to predict: the first three pixels of the first row
  newdata <- as.data.frame(split(dc[, 1:3, 1:2, ], "time"))[1:3, ]
  image_dates <- st_get_dimension_values(dc, "time")
  bands <- names(dc)

  # The first time series has a missing value in one band on the fifth date,
  # and the second time series has no observation of this band
  newdata[1, paste0("BLUE.", image_dates[5])] <- NA
  newdata[2, paste0("BLUE.", image_dates)] <- NA

  pr <- predict(m, newdata = newdata)

  expect_s3_class(pr, "data.frame")
  expect_named(pr, c("prediction", "distance"))
  expect_equal(nrow(pr), 3)

  # The first time series is compared without its fifth observation
  ts <- data.frame(time = image_dates, lapply(dc, function(band) band[1, 1, ]))
  distance <- distance_to_patterns(ts[-5, ], m)
  expect_equal(as.character(pr$prediction[1]), m$data$label[which.min(distance)])
  expect_equal(pr$distance[1], min(distance))

  # The second time series has no complete observation
  expect_true(is.na(pr$prediction[2]))
  expect_true(is.na(pr$distance[2]))

  # The third time series is complete
  ts <- data.frame(time = image_dates, lapply(dc, function(band) band[3, 1, ]))
  distance <- distance_to_patterns(ts, m)
  expect_equal(as.character(pr$prediction[3]), m$data$label[which.min(distance)])
  expect_equal(pr$distance[3], min(distance))
})

test_that("the time series must have the bands of the model", {
  dc <- example_cube()
  m <- example_model(dc, example_samples())
  newdata <- as.data.frame(split(dc[, 1:3, 1:2, ], "time"))

  # A band of the model is missing
  expect_error(predict(m, newdata = newdata[, !grepl("^BLUE", names(newdata))]),
               "The bands of newdata (EVI, NDVI, RED, NIR, MIR) are not the bands of the model",
               fixed = TRUE)

  # No column has a band and a date in its name
  expect_error(predict(m, newdata = newdata[, c("x", "y")]),
               "'newdata' has no columns named with a band and a date",
               fixed = TRUE)

  # A data cube with other bands
  expect_error(predict(m, newdata = dc[c("EVI", "NDVI")]),
               "The bands of newdata (EVI, NDVI) are not the bands of the model (EVI, NDVI, RED, BLUE, NIR, MIR)",
               fixed = TRUE)
  expect_error(predict(m, newdata = setNames(dc, paste0("B", 1:6))),
               "The bands of newdata (B1, B2, B3, B4, B5, B6) are not the bands of the model",
               fixed = TRUE)
})
