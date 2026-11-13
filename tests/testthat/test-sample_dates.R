# The label of a training sample is valid from its start date to its end date.
# twdtw_knn1() must use only the observations within this time interval.

train <- function(...) {
  twdtw_knn1(...,
             cycle_length = 'year',
             time_scale = 'day',
             time_weight = c(steepness = 0.1, midpoint = 50))
}

observation_dates <- function(model) {
  lapply(model$data$observations, function(ts) ts$time)
}

test_that("samples that cover all images keep all observations", {
  dc <- example_cube()
  samples <- example_samples()
  image_dates <- st_get_dimension_values(dc, "time")

  m <- train(x = dc, y = samples)

  expect_equal(m$data$label, samples$label)
  expect_equal(unique(observation_dates(m)), list(image_dates))
})

test_that("samples keep only the observations between their start and end dates", {
  dc <- example_cube()
  samples <- example_samples()
  image_dates <- st_get_dimension_values(dc, "time")

  # Every second sample gets a shorter time interval, which starts and ends on the date of an image
  short <- seq(1, nrow(samples), by = 2)
  samples$start_date[short] <- as.Date("2011-11-01")
  samples$end_date[short] <- as.Date("2012-03-05")

  m <- train(x = dc, y = samples)

  # The images on the start and end dates are included
  expect_equal(range(image_dates[4:12]), as.Date(c("2011-11-01", "2012-03-05")))
  expect_equal(unique(observation_dates(m)[short]), list(image_dates[4:12]))
  expect_equal(unique(observation_dates(m)[-short]), list(image_dates))
  expect_equal(m$data$label, samples$label)
})

test_that("start and end dates can be dates, date-times, or character strings", {
  dc <- example_cube()
  samples <- example_samples()

  samples$start_date <- as.Date("2011-11-01")
  samples$end_date <- as.Date("2012-03-05")
  m_date <- train(x = dc, y = samples)

  # In UTC these date-times are on the day before, which would exclude the image on the end date
  samples$start_date <- as.POSIXct("2011-11-01 00:00:00", tz = "Asia/Tokyo")
  samples$end_date <- as.POSIXct("2012-03-05 00:00:00", tz = "Asia/Tokyo")
  m_datetime <- train(x = dc, y = samples)

  samples$start_date <- "2011-11-01"
  samples$end_date <- "2012-03-05"
  m_character <- train(x = dc, y = samples)

  expect_equal(unique(lengths(observation_dates(m_date))), 9)
  expect_equal(m_datetime$data, m_date$data)
  expect_equal(m_character$data, m_date$data)
})

test_that("samples without observations in their time interval are removed", {
  dc <- example_cube()
  samples <- example_samples()

  # The first three samples are from the year after the last image
  samples$start_date[1:3] <- as.Date("2012-09-01")
  samples$end_date[1:3] <- as.Date("2013-08-31")

  expect_warning(m <- train(x = dc, y = samples), "were removed: 3 of 291")
  expect_equal(m$data$label, samples$label[-(1:3)])

  # No sample has observations
  samples$start_date <- as.Date("2012-09-01")
  samples$end_date <- as.Date("2013-08-31")

  expect_error(train(x = dc, y = samples), "No sample in y has observations")
})

test_that("missing start or end dates are an error", {
  dc <- example_cube()
  samples <- example_samples()

  samples$end_date[5] <- NA

  expect_error(train(x = dc, y = samples), "cannot have missing values")
})

test_that("samples from different years of a longer data cube give patterns of one year", {
  dc <- example_cube(years = 2)
  samples <- example_samples()

  # Every second sample is from the second year of the data cube
  second_year <- seq(2, nrow(samples), by = 2)
  samples$start_date[second_year] <- as.Date("2012-09-01")
  samples$end_date[second_year] <- as.Date("2013-08-31")

  # Model with all samples: each sample has the 23 images of its own year
  m <- train(x = dc, y = samples)

  expect_equal(unique(lengths(observation_dates(m))), 23)
  expect_true(all(do.call(c, observation_dates(m)[-second_year]) <= as.Date("2012-08-31")))
  expect_true(all(do.call(c, observation_dates(m)[second_year]) >= as.Date("2012-09-01")))

  # Model with one pattern for each label: the samples of both years are aligned
  m <- train(x = dc, y = samples, resampling_fun = function(data) lm(y ~ factor(x), data = data))

  expect_equal(sort(m$data$label), sort(unique(samples$label)))
  expect_equal(unique(lengths(observation_dates(m))), 23)
})
