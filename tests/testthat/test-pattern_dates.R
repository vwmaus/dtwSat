# The observations of a temporal pattern must be in time order.

test_that("patterns from samples of different years are in time order", {
  # The images of the second year are on the same days of the year as the images of
  # the first year, as in MOD13Q1. From the last image of December to the first image
  # of January there are 13 days, or 14 days in a leap year. So, counted from their
  # first observation, the samples of the second year are one day later from January on.
  dc <- example_cube(years = 2, same_day_of_year = TRUE)
  samples <- example_samples()

  second_year <- seq(2, nrow(samples), by = 2)
  samples$start_date[second_year] <- as.Date("2012-09-01")
  samples$end_date[second_year] <- as.Date("2013-08-31")

  train <- function(...) {
    twdtw_knn1(x = dc,
               y = samples,
               ...,
               cycle_length = 'year',
               time_scale = 'day',
               time_weight = c(steepness = 0.1, midpoint = 50))
  }

  # One pattern for each label with the mean of the samples on each date
  m <- train(resampling_fun = function(data) lm(y ~ factor(x), data = data))

  for (pattern in m$data$observations) {
    # 23 dates of the first year and 16 dates of the second year that are one day later
    expect_equal(nrow(pattern), 39)
    expect_false(is.unsorted(pattern$time, strictly = TRUE))
  }

  # The same mean calculated from the samples, with the dates of each sample
  # moved to start in 1970 as in twdtw_knn1()
  m_samples <- train()
  label <- m$data$label[1]
  ts_label <- m_samples$data$observations[m_samples$data$label == label]
  ts_label <- do.call(rbind, lapply(ts_label, function(ts) {
    data.frame(time = shift_dates(ts$time), EVI = ts$EVI)
  }))
  mean_evi <- tapply(ts_label$EVI, ts_label$time, mean)

  pattern <- m$data$observations[[1]]
  expect_equal(pattern$time, as.Date(names(mean_evi)))
  expect_equal(pattern$EVI, as.vector(mean_evi))
})
