# shift_dates() moves a sequence of dates to the origin year.

test_that("shifted dates start on the same day of the year and keep the days between them", {
  x <- as.Date(c("2011-09-14", "2011-09-30", "2012-01-01", "2012-08-28"))

  shifted <- shift_dates(x)

  expect_equal(shifted[1], as.Date("1970-09-14"))
  expect_equal(diff(shifted), diff(x))
})

test_that("a sequence that starts on day 366 of a leap year is shifted", {
  x <- as.Date(c("2012-12-31", "2013-01-16", "2013-02-01"))

  # 1970 has 365 days: the sequence starts on its last day
  expect_warning(shifted <- shift_dates(x), NA)
  expect_equal(shifted[1], as.Date("1970-12-31"))
  expect_equal(diff(shifted), diff(x))

  # 1972 has 366 days: the sequence starts on day 366
  expect_equal(shift_dates(x, origin = "1972-01-01")[1], as.Date("1972-12-31"))
})
