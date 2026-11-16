# The labels of the training samples come from the column given in 'label_column'.

test_that("the labels are read from the column given in label_column", {
  dc <- example_cube()
  samples <- example_samples()

  # The labels are in a column that does not have the default name
  samples$land_use <- samples$label
  samples$label <- NULL

  train <- function(...) {
    twdtw_knn(x = dc,
              y = samples,
              ...,
              cycle_length = 'year',
              time_scale = 'day',
              time_weight = c(steepness = 0.1, midpoint = 50))
  }

  m <- train(label_column = "land_use")
  expect_equal(m$data$label, samples$land_use)

  # Without the name of the column, the default column 'label' is missing
  expect_error(train(), "Missing required columns in y: label", fixed = TRUE)

  # The argument was called 'label_colum' in version 1.0.0. Calls with the old
  # name still work, because R completes the names of arguments.
  m_old <- train(label_colum = "land_use")
  expect_equal(m_old$data$label, samples$land_use)
})
