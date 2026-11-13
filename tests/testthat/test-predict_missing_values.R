# predict(model, newdata = cube) classifies pixels with missing values using their
# remaining observations. predict(cube, model = model) calls stars, which does not
# classify pixels with missing values.

# Model with one pattern for each label: the mean of the samples on each date
example_model <- function(dc, samples) {
  twdtw_knn1(x = dc,
             y = samples,
             resampling_fun = function(data) lm(y ~ factor(x), data = data),
             cycle_length = 'year',
             time_scale = 'day',
             time_weight = c(steepness = 0.1, midpoint = 50))
}

# Distance of a time series to each pattern of a model, calculated with twdtw()
distance_to_patterns <- function(ts, model) {
  vapply(model$data$observations, function(pattern) {
    as.numeric(twdtw(x = ts,
                     y = as.data.frame(pattern),
                     cycle_length = 'year',
                     time_scale = 'day',
                     time_weight = c(steepness = 0.1, midpoint = 50)))
  }, numeric(1))
}

test_that("pixels with missing values are classified if the data cube is passed to the model", {
  dc <- example_cube()
  m <- example_model(dc, example_samples())

  lu <- predict(m, newdata = dc)
  lu_stars <- suppressMessages(predict(dc, model = m))

  # The same kind of map
  expect_s3_class(lu, "stars")
  expect_named(lu, c("prediction", "distance"))
  expect_equal(st_dimensions(lu), st_dimensions(lu_stars))
  expect_equal(levels(lu$prediction), levels(lu_stars$prediction))

  # The example data cube has nine pixels with a missing value
  has_missing <- Reduce(`|`, lapply(dc, function(band) apply(is.na(band), c(1, 2), any)))
  expect_equal(sum(has_missing), 9)

  # stars does not classify them
  expect_equal(is.na(lu_stars$prediction), has_missing, ignore_attr = TRUE)
  expect_equal(is.na(lu_stars$distance), has_missing, ignore_attr = TRUE)

  # All pixels are classified if the data cube is passed to the model
  expect_false(anyNA(lu$prediction))
  expect_false(anyNA(lu$distance))

  # The pixels without missing values have the same result
  expect_equal(as.vector(lu$prediction)[!has_missing], as.vector(lu_stars$prediction)[!has_missing])
  expect_equal(as.vector(lu$distance)[!has_missing], as.vector(lu_stars$distance)[!has_missing])

  # The pixels with a missing value are classified with their complete observations
  image_dates <- st_get_dimension_values(dc, "time")
  for (pixel in which(has_missing)[c(1, 9)]) {
    ij <- arrayInd(pixel, dim(has_missing))
    ts <- data.frame(time = image_dates, lapply(dc, function(band) band[ij[1], ij[2], ]))
    expect_equal(sum(complete.cases(ts)), length(image_dates) - 1)

    distance <- distance_to_patterns(ts[complete.cases(ts), ], m)
    expect_equal(as.character(lu$prediction[ij[1], ij[2]]), m$data$label[which.min(distance)])
    expect_equal(lu$distance[ij[1], ij[2]], min(distance))
  }
})

test_that("a pixel without complete observations has no prediction", {
  dc <- example_cube()
  m <- example_model(dc, example_samples())

  dc_part <- dc[, 1:6, 1:6, ]
  lu <- predict(m, newdata = dc_part)

  # The second pixel has no observation of one band
  dc_part$EVI[2, 1, ] <- NA
  lu_missing <- predict(m, newdata = dc_part)

  expect_true(is.na(lu_missing$prediction[2, 1]))
  expect_true(is.na(lu_missing$distance[2, 1]))
  expect_equal(as.vector(lu_missing$prediction)[-2], as.vector(lu$prediction)[-2])
  expect_equal(as.vector(lu_missing$distance)[-2], as.vector(lu$distance)[-2])
})

test_that("the dimensions of the data cube can have another order and other names", {
  dc <- example_cube()
  m <- example_model(dc, example_samples())

  dc_part <- dc[, 1:6, 1:5, ]
  lu <- predict(m, newdata = dc_part)
  expect_equal(dim(lu), c(x = 6, y = 5))

  # Time is the first dimension
  lu_time_first <- predict(m, newdata = aperm(dc_part, c("time", "x", "y")))
  expect_equal(lu_time_first, lu)

  # The spatial dimensions are called lon and lat
  lu_lon_lat <- predict(m, newdata = st_set_dimensions(dc_part, names = c("lon", "lat", "time")))
  expect_equal(dim(lu_lon_lat), c(lon = 6, lat = 5))
  expect_equal(as.vector(lu_lon_lat$prediction), as.vector(lu$prediction))
  expect_equal(as.vector(lu_lon_lat$distance), as.vector(lu$distance))
})

test_that("twdtw arguments can be given with a data cube", {
  dc <- example_cube()
  m <- example_model(dc, example_samples())
  dc_part <- dc[, 1:6, 1:6, ]

  lu <- predict(m, newdata = dc_part, max_elapsed = 30)
  lu_stars <- suppressMessages(predict(dc_part, model = m, max_elapsed = 30))

  expect_equal(lu, lu_stars)
  expect_false(isTRUE(all.equal(lu$distance, predict(m, newdata = dc_part)$distance)))
})

test_that("a data cube that cannot be classified is an error", {
  dc <- example_cube()
  m <- example_model(dc, example_samples())
  dc_part <- dc[, 1:6, 1:6, ]
  expected <- "newdata must be a three-dimensional stars object with a 'time' dimension"

  # A single image has no time dimension
  expect_error(predict(m, newdata = dc_part[, , , 1, drop = TRUE]), expected, fixed = TRUE)

  # The bands are a dimension and not attributes
  expect_error(predict(m, newdata = merge(dc_part)), expected, fixed = TRUE)

  # The time dimension has another name
  expect_error(predict(m, newdata = st_set_dimensions(dc_part, 3, names = "date")), expected, fixed = TRUE)

  # A band of the model is missing
  expect_error(predict(m, newdata = dc_part[c("EVI", "NDVI")]),
               "The bands of newdata (EVI, NDVI) are not the bands of the model",
               fixed = TRUE)
})
