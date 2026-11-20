# A data cube that does not fit in memory is a stars proxy object, which reads the images
# when their values are needed. predict() reads and classifies it in blocks of rows, and
# twdtw_knn() reads the blocks that have samples.

band_names <- c("EVI", "NDVI", "RED", "BLUE", "NIR", "MIR")

# The example images as a stars proxy object, with one file for each date. The files have
# seven bands. The bands are a dimension until the data are read: they then become
# attributes, and the first six are kept
proxy_cube <- function(...) {
  tif_files <- dir(system.file("mato_grosso_brazil", package = "dtwSat"), pattern = "\\.tif$", full.names = TRUE)
  tif_names <- basename(tif_files)
  acquisition_date <- as.Date(regmatches(tif_names, regexpr("[0-9]{8}", tif_names)), format = "%Y%m%d")
  dc <- read_stars(tif_files, proxy = TRUE, along = "time", ...)
  dc <- st_set_dimensions(dc, "time", values = acquisition_date)
  dc <- st_set_dimensions(dc, "band", values = c(band_names, "B7")[seq_len(dim(dc)[["band"]])])
  split(dc, c("band"))[seq_along(band_names)]
}

# A data cube as a stars proxy object with one file for each band and the dates as layers
proxy_cube_by_band <- function(dc) {
  tif_dir <- tempfile("bands")
  dir.create(tif_dir)
  tif_files <- vapply(names(dc), function(band) {
    tif_file <- file.path(tif_dir, paste0(band, ".tif"))
    write_stars(dc[band], tif_file)
    tif_file
  }, character(1))
  dc_proxy <- setNames(read_stars(tif_files, proxy = TRUE), names(dc))
  st_set_dimensions(dc_proxy, 3, values = st_get_dimension_values(dc, "time"), names = "time")
}

train <- function(dc, ...) {
  twdtw_knn(x = dc,
            y = example_samples(),
            ...,
            cycle_length = 'year',
            time_scale = 'day',
            time_weight = c(steepness = 0.1, midpoint = 50))
}

mean_fun <- function(data) lm(y ~ factor(x), data = data)

# Two maps have the same pixels, the same position, and the same values. The dimensions of
# a map also have the block sizes of the files of its data cube, which are not compared
expect_same_map <- function(actual, expected) {
  expect_s3_class(actual, "stars")
  expect_false(inherits(actual, "stars_proxy"))
  expect_identical(names(actual), names(expected))
  expect_identical(dim(actual), dim(expected))
  expect_equal(st_bbox(actual), st_bbox(expected))
  expect_equal(st_crs(actual), st_crs(expected))
  for (v in names(expected)) {
    expect_equal(actual[[v]], expected[[v]])
  }
}

test_that("the rows of a data cube are divided into blocks of at least two rows", {
  expect_equal(unname(row_blocks(27, 10)), cbind(c(1, 11, 21), c(10, 20, 27)))
  expect_equal(unname(row_blocks(27, 27)), cbind(1, 27))
  expect_equal(unname(row_blocks(27, 100)), cbind(1, 27))

  # A last block with a single row is added to the block before it
  expect_equal(unname(row_blocks(27, 13)), cbind(c(1, 14), c(13, 27)))
  expect_equal(unname(row_blocks(5, 1)), cbind(c(1, 3), c(2, 5)))
  expect_equal(unname(row_blocks(1, 10)), cbind(1, 1))

  # Rows that are read at a time: all rows of a data cube in memory, and about 50 million
  # values of a stars proxy object
  dc <- example_cube()
  expect_equal(chunk_rows(dc, 27), 27)
  expect_equal(chunk_rows(dc, 27, 5.9), 5)
  expect_equal(chunk_rows(proxy_cube(), 27), floor(5e7 / (37 * 23 * 7)))
  expect_equal(chunk_rows(proxy_cube_by_band(dc), 27), floor(5e7 / (37 * 23 * 6)))
})

test_that("rows of a stars proxy object are read as the rows of the data cube in memory", {
  dc <- example_cube()

  for (dc_proxy in list(proxy_cube(), proxy_cube_by_band(dc))) {
    expect_s3_class(dc_proxy, "stars_proxy")
    for (rows in list(c(1, 2), c(10, 18), c(26, 27), c(1, 27))) {
      part <- read_rows(dc_proxy, rows)
      expect_equal(dim(part), c(x = 37, y = rows[2] - rows[1] + 1, time = 23))
      expect_named(part, band_names)
      expect_equal(st_get_dimension_values(part, "time"), st_get_dimension_values(dc, "time"))
      for (band in band_names) {
        expect_equal(unclass(part[[band]]), unclass(dc[[band]])[, rows[1]:rows[2], , drop = FALSE],
                     ignore_attr = TRUE)
      }
    }
  }
})

test_that("a stars proxy object is classified in blocks of rows", {
  dc <- example_cube()
  m <- train(dc, resampling_fun = mean_fun)
  lu <- predict(m, newdata = dc)

  # The example data cube has nine pixels with a missing value, which are classified
  expect_false(anyNA(lu$prediction))

  # One file for each date
  dc_proxy <- proxy_cube()
  for (chunk_size in list(NULL, 2, 10, 13, 100)) {
    expect_same_map(predict(m, newdata = dc_proxy, chunk_size = chunk_size), lu)
  }

  # One file for each band
  dc_proxy <- proxy_cube_by_band(dc)
  for (chunk_size in list(NULL, 5, 26)) {
    expect_same_map(predict(m, newdata = dc_proxy, chunk_size = chunk_size), lu)
  }

  # The k nearest samples and the other types of prediction
  m_samples <- train(dc)
  for (type in c("class", "prob", "neighbors")) {
    expect_same_map(predict(m_samples, newdata = dc_proxy, k = 3, vote = "weighted", type = type, chunk_size = 9),
                    predict(m_samples, newdata = dc, k = 3, vote = "weighted", type = type))
  }

  # Arguments of the TWDTW distance replace the arguments of the model
  expect_same_map(predict(m, newdata = dc_proxy, chunk_size = 9, max_elapsed = 30),
                  predict(m, newdata = dc, max_elapsed = 30))
})

test_that("a data cube in memory can be classified in blocks of rows", {
  dc <- example_cube()
  m <- train(dc, resampling_fun = mean_fun)
  lu <- predict(m, newdata = dc)

  for (chunk_size in c(1, 2, 10, 26, 27, 100)) {
    expect_identical(predict(m, newdata = dc, chunk_size = chunk_size), lu)
  }

  # Spatial dimensions with other names and in another order
  dc_named <- st_set_dimensions(aperm(dc, c(3, 2, 1)), names = c("time", "row", "column"))
  lu_named <- predict(m, newdata = dc_named)
  expect_equal(dim(lu_named), c(row = 27, column = 37))
  expect_identical(predict(m, newdata = dc_named, chunk_size = 7), lu_named)

  for (chunk_size in list(0, -1, NA, "10", c(5, 5))) {
    expect_error(predict(m, newdata = dc, chunk_size = chunk_size),
                 "'chunk_size' must be a number of rows", fixed = TRUE)
  }
})

test_that("a stars proxy object created with RasterIO is an error if it is read in blocks", {
  dc <- example_cube()
  m <- train(dc, resampling_fun = mean_fun)

  # stars reads the whole images for each block
  dc_proxy <- suppressWarnings(proxy_cube(RasterIO = list(bands = 1:6)))
  expect_error(suppressWarnings(predict(m, newdata = dc_proxy, chunk_size = 9)),
               "stars did not read rows 1 to 9 of the data cube, but 27 rows", fixed = TRUE)
  expect_error(suppressWarnings(predict(m, newdata = dc_proxy, chunk_size = 9)),
               "select the bands after split() instead", fixed = TRUE)

  # A single block is read as a whole
  expect_same_map(suppressWarnings(predict(m, newdata = dc_proxy)), predict(m, newdata = dc))
})

test_that("a stars proxy object that does not give a data cube is an error", {
  m <- train(example_cube(), resampling_fun = mean_fun)
  tif_files <- dir(system.file("mato_grosso_brazil", package = "dtwSat"), pattern = "\\.tif$", full.names = TRUE)

  # The bands are a dimension and not attributes
  dc_proxy <- read_stars(tif_files, proxy = TRUE, along = "time")
  expect_error(predict(m, newdata = dc_proxy),
               "The stars proxy object must give a three-dimensional data cube with a 'time' dimension",
               fixed = TRUE)
  expect_error(train(dc_proxy), "The stars proxy object must give a three-dimensional data cube", fixed = TRUE)

  # 'time' is the time dimension of the data cube
  expect_error(predict(m, newdata = proxy_cube(), time = Sys.Date()),
               "The argument 'time' cannot be used with a data cube", fixed = TRUE)
})

test_that("a model is created with the samples read from a stars proxy object", {
  dc <- example_cube()
  samples <- example_samples()
  m <- train(dc, resampling_fun = mean_fun)
  m_samples <- train(dc)

  for (dc_proxy in list(proxy_cube(), proxy_cube_by_band(dc))) {
    expect_equal(train(dc_proxy, resampling_fun = mean_fun)$data, m$data)
    expect_equal(train(dc_proxy)$data, m_samples$data)

    # The time series of the samples, read in blocks of rows
    for (rows in c(2, 4, 13)) {
      expect_equal(sample_array_proxy(dc_proxy, samples, rows = rows), sample_array(dc, samples))
    }
  }

  # Samples in a few rows, and samples outside of the data cube, which are removed
  dc_proxy <- proxy_cube()
  few <- samples[c(1, 50, 120, 200, 291), ]
  expect_equal(sample_array_proxy(dc_proxy, few, rows = 2), sample_array(dc, few))

  outside <- few
  st_geometry(outside)[c(2, 4)] <- st_geometry(outside)[c(2, 4)] + c(1e5, 1e5)
  st_crs(outside) <- st_crs(samples)
  expect_equal(sample_array_proxy(dc_proxy, outside, rows = 2), sample_array(dc, outside))
  expect_true(all(is.na(sample_array_proxy(dc_proxy, outside, rows = 2)$values[c(2, 4), , ])))
  expect_warning(m_outside <- twdtw_knn(x = dc_proxy, y = outside, cycle_length = 'year', time_scale = 'day',
                                        time_weight = c(steepness = 0.1, midpoint = 50)),
                 "Samples without observations in x between their start and end dates were removed: 2 of 5",
                 fixed = TRUE)
  expect_equal(nrow(m_outside$data), 3)

  all_outside <- outside[c(2, 4), ]
  expect_equal(sample_array_proxy(dc_proxy, all_outside, rows = 2), sample_array(dc, all_outside))

  # The samples must have the coordinate reference system of the data cube
  expect_error(twdtw_knn(x = dc_proxy, y = st_transform(samples, 4326), cycle_length = 'year', time_scale = 'day',
                         time_weight = c(steepness = 0.1, midpoint = 50)),
               "x and y must have the same coordinate reference system", fixed = TRUE)
})
