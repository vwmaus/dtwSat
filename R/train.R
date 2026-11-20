#' Train a TWDTW k-nearest neighbour model with a data cube
#'
#' This is the method of \link[twdtw]{twdtw_knn} for satellite image time series. It takes the
#' time series of the training samples from a data cube and creates a k-nearest neighbour (KNN)
#' model with the Time-Weighted Dynamic Time Warping (TWDTW) algorithm.
#' \link[dtwSat:twdtw_knn_predict.stars]{predict} classifies a data cube with the model.
#'
#' @param x A three-dimensional stars object (x, y, time) with bands as attributes. It can be a
#' stars proxy object, i.e. a data cube that is not in memory. Only the rows of the images that
#' have training samples are then read.
#' @param y An sf object with the coordinates of the training points.
#' @param time_weight A numeric vector with length two (steepness and midpoint of logistic weight) or a function.
#' See details in \link[twdtw]{twdtw}.
#' @param cycle_length The length of the cycle, e.g. phenological cycles. Details in \link[twdtw]{twdtw}.
#' @param time_scale Specifies the time scale for the observations. Details in \link[twdtw]{twdtw}.
#' @param resampling_fun A function that creates one temporal pattern for each label from all samples
#' with this label. If `NULL` (default), the model keeps the time series of all samples as temporal
#' patterns. The function receives a data frame and returns a model. See details.
#' @param resampling_freq The time between the observations of the temporal patterns created with
#' `resampling_fun`, e.g. `16` for 16 days. If `NULL` (default), the patterns have the dates of the
#' observations of the samples, in time order.
#' @param start_column Name of the column in y with the start date of each training sample. Default is 'start_date'.
#' @param end_column Name of the column in y with the end date of each training sample. Default is 'end_date'.
#' @param label_column Name of the column in y containing land use labels. Default is 'label'.
#' @param ... Additional arguments of \link[twdtw]{twdtw_knn}: `origin` and `max_elapsed` of the
#' TWDTW distance, see \link[twdtw]{twdtw}, and `keep_models`, see details.
#' Any other argument is an error.
#'
#' @details The label of a training sample is valid from its start date to its end date.
#' For each sample, the model uses only the observations of `x` within this time interval,
#' including observations on the start and end dates. Samples without observations in their
#' time interval are removed with a warning.
#'
#' If `resampling_fun` is `NULL`, the model keeps the time series of all samples.
#' Such a model has several temporal patterns for each label and can be classified with more
#' than one nearest neighbour, see the argument `k` of \link[twdtw]{predict.twdtw_knn}.
#'
#' If `resampling_fun` is a function, the model has one temporal pattern for each label.
#' This reduces the processing time of the classification, because the TWDTW distance is
#' calculated to the patterns of the labels and not to the time series of all samples.
#' The function is called for each label and band with a data frame that has two columns:
#' \itemize{
#'   \item `x`, the dates of the observations of all samples with this label, as numbers.
#'   \item `y`, the values of the band on these dates.
#' }
#' It returns a model that predicts the values of the temporal pattern, e.g.
#' `function(data) mgcv::gam(y ~ s(x), data = data)`. The dates of samples from different
#' years are shifted to the same year before the function is called.
#' With samples from different years, set `resampling_freq`, so that the temporal patterns of
#' all labels have observations at the same interval. See the details of \link[twdtw]{twdtw_knn}.
#'
#' The model keeps the models returned by `resampling_fun`, which usually contain the time series
#' of all samples. With `keep_models = FALSE` it keeps only their root mean squared error. This makes
#' the model much smaller, e.g. to save it to a file, and the land use map is the same.
#'
#' The model is created by \link[twdtw]{twdtw_knn} of the package twdtw. This function takes
#' the time series of the samples from the data cube.
#'
#' `twdtw_knn1()` is the name of this function in earlier versions of the package. It is
#' deprecated: it calls `twdtw_knn()` with the same arguments and gives a message once per session.
#'
#' @return A model of class 'twdtw_knn', see \link[twdtw]{twdtw_knn}.
#' In earlier versions of the package the class of the model was 'twdtw_knn1'.
#'
#' @seealso \link[dtwSat]{twdtw_knn_predict.stars}, \link[dtwSat]{plot.twdtw_knn},
#' \link[twdtw]{twdtw_knn}, \link[twdtw]{predict.twdtw_knn}
#'
#' @examples
#' \dontrun{
#'
#' # Read training samples
#' samples_path <-
#'   system.file("mato_grosso_brazil/samples.gpkg", package = "dtwSat")
#'
#' samples <- st_read(samples_path, quiet = TRUE)
#'
#' # Get satellite image time series files
#' tif_path <- system.file("mato_grosso_brazil", package = "dtwSat")
#' tif_files <- dir(tif_path, pattern = "\\.tif$", full.names = TRUE)
#'
#' # Get acquisition dates from the file names
#' tif_names <- basename(tif_files)
#' acquisition_date <- regmatches(tif_names, regexpr("[0-9]{8}", tif_names))
#' acquisition_date <- as.Date(acquisition_date, format = "%Y%m%d")
#'
#' # Create a 3D datacube
#' dc <- read_stars(tif_files,
#'                  proxy = FALSE,
#'                  along = list(time = acquisition_date),
#'                  RasterIO = list(bands = 1:6))
#' dc <- st_set_dimensions(dc, 3, c("EVI", "NDVI", "RED", "BLUE", "NIR", "MIR"))
#' dc <- split(dc, c("band"))
#'
#' # Create a model with one temporal pattern for each label
#' m <- twdtw_knn(
#'  x = dc,
#'  y = samples,
#'  resampling_fun = function(data) mgcv::gam(y ~ s(x), data = data),
#'  cycle_length = 'year',
#'  time_scale = 'day',
#'  time_weight = c(steepness = 0.1, midpoint = 50))
#'
#' print(m)
#'
#' # Visualize model patterns
#' plot(m)
#'
#' # Classify satellite images
#' system.time(lu <- predict(m, newdata = dc))
#'
#' # Visualise land use classification
#' ggplot() +
#'   geom_stars(data = lu["prediction"]) +
#'   theme_minimal()
#'
#' # Create a model with a custom resampling function:
#' # the mean of the samples on each date
#' m <- twdtw_knn(x = dc,
#'  y = samples,
#'  resampling_fun = function(data) lm(y ~ factor(x), data = data),
#'  cycle_length = 'year',
#'  time_scale = 'day',
#'  time_weight = c(steepness = 0.1, midpoint = 50))
#'
#' plot(m)
#'
#' # Create a model that keeps the time series of all samples
#' # and classify with the three nearest samples
#' m <- twdtw_knn(x = dc,
#'  y = samples,
#'  cycle_length = 'year',
#'  time_scale = 'day',
#'  time_weight = c(steepness = 0.1, midpoint = 50))
#'
#' lu <- predict(m, newdata = dc, k = 3)
#'
#' # The share of the votes of each label among the three nearest samples
#' lu_share <- predict(m, newdata = dc, k = 3, type = "prob")
#'
#' # A data cube that does not fit in memory: a stars proxy object, which reads the
#' # images when their values are needed. The files have seven bands. The first six
#' # are selected after split(), and not with the argument RasterIO
#' dc_proxy <- read_stars(tif_files, proxy = TRUE, along = "time")
#' dc_proxy <- st_set_dimensions(dc_proxy, "time", values = acquisition_date)
#' dc_proxy <- st_set_dimensions(dc_proxy, "band",
#'                               values = c("EVI", "NDVI", "RED", "BLUE", "NIR", "MIR", "B7"))
#' dc_proxy <- split(dc_proxy, c("band"))[1:6]
#'
#' # The model reads the rows of the images that have samples
#' m <- twdtw_knn(x = dc_proxy,
#'  y = samples,
#'  resampling_fun = function(data) mgcv::gam(y ~ s(x), data = data),
#'  cycle_length = 'year',
#'  time_scale = 'day',
#'  time_weight = c(steepness = 0.1, midpoint = 50))
#'
#' # The images are read and classified in blocks of 10 rows
#' lu <- predict(m, newdata = dc_proxy, chunk_size = 10)
#'
#' }
#' @export
twdtw_knn.stars <- function(x, y, time_weight, cycle_length, time_scale,
                            resampling_fun = NULL, resampling_freq = NULL,
                            start_column = 'start_date', end_column = 'end_date',
                            label_column = 'label', ...) {

  # Check if x is a stars object with a time dimension. A stars proxy object has its
  # dimensions after the data are read
  is_proxy <- inherits(x, "stars_proxy")
  if (!inherits(x, "stars") || (!is_proxy && (length(dim(x)) != 3 || !'time' %in% names(dim(x))))) {
    stop("x must be a three-dimensional stars object with a 'time' dimension")
  }

  # Arguments that this function does not know are not ignored
  check_twdtw_args(list(...))

  # Check if y is an sf object with point geometry
  if (!inherits(y, "sf") || !all(st_is(y, "POINT"))) {
    stop("y must be an sf object with point geometry")
  }

  # Check for required columns in y
  required_columns <- c(start_column, end_column, label_column)
  missing_columns <- setdiff(required_columns, names(y))
  if (length(missing_columns) > 0) {
    stop(paste("Missing required columns in y:", paste(missing_columns, collapse = ", ")))
  }

  # Time interval in which the label of each sample is valid
  start_date <- to_date(to_date_time(y[[start_column]]))
  end_date <- to_date(to_date_time(y[[end_column]]))
  if (anyNA(start_date) || anyNA(end_date)) {
    stop(paste0("The columns '", start_column, "' and '", end_column, "' of y cannot have missing values"))
  }

  # Time series of the samples, with all dates of the data cube
  ts_data <- if (is_proxy) sample_array_proxy(x, y) else sample_array(x, y)

  # A sample needs an observation without missing values in its time interval
  ts_day <- as.numeric(ts_data$time)
  in_interval <- outer(as.numeric(start_date), ts_day, `<=`) & outer(as.numeric(end_date), ts_day, `>=`)
  is_complete <- rowSums(is.na(ts_data$values), dims = 2) == 0
  has_observations <- rowSums(in_interval & is_complete) > 0

  if (!any(has_observations)) {
    stop("No sample in y has observations in x between its start and end dates")
  }

  if (!all(has_observations)) {
    warning(paste0("Samples without observations in x between their start and end dates were removed: ",
                   sum(!has_observations), " of ", length(has_observations)))
  }

  # Create the model with the time series of the samples in their time intervals
  model <- twdtw_knn(x = ts_data$values[has_observations, , , drop = FALSE],
                     y = y[[label_column]][has_observations],
                     time_weight = time_weight,
                     cycle_length = cycle_length,
                     time_scale = time_scale,
                     resampling_fun = resampling_fun,
                     resampling_freq = resampling_freq,
                     start = start_date[has_observations],
                     end = end_date[has_observations],
                     time = ts_data$time,
                     ...)

  # The call of this function, and not the call with the time series
  model$call <- match.call()
  model$call[[1L]] <- as.name("twdtw_knn")

  return(model)

}


#' Time Series of the Samples from a stars Data Cube
#'
#' This function takes the time series of the pixels at the coordinates of the training samples
#' from a data cube, as an array with the dimensions sample, time, and band, which is an input
#' of \link[twdtw]{twdtw_knn}. Missing values remain missing.
#'
#' @param x A three-dimensional stars object with a 'time' dimension and bands as attributes.
#' @param y An sf object with the coordinates of the training points.
#'
#' @return A list with `values`, a numeric array with the dimensions sample, time, and band,
#' and `time`, the dates of the time dimension of `x`. The band names are the names of the
#' attributes of `x`.
#'
#' @noRd
#' @keywords internal
sample_array <- function(x, y) {

  # A stars object with the samples and the time as dimensions
  ts <- st_extract(x, y)

  ts_time <- to_date(st_get_dimension_values(ts, 'time'))
  ts_bands <- names(ts)
  time_dim <- which(names(dim(ts)) == 'time')

  values <- array(NA_real_,
                  dim = c(nrow(y), length(ts_time), length(ts_bands)),
                  dimnames = list(NULL, NULL, ts_bands))

  for (i in seq_along(ts_bands)) {
    # Position the time dimension last, so that each row has the time series of a sample
    values[, , i] <- as.numeric(aperm(unclass(ts[[i]]), c(seq_along(dim(ts))[-time_dim], time_dim)))
  }

  return(list(values = values, time = ts_time))

}


#' Time Series of the Samples from a stars Proxy Object
#'
#' This function takes the time series of the training samples from a data cube that is not in
#' memory. The data cube is read in blocks of rows, and only the blocks that have samples are
#' read. The result is the result of \code{sample_array} for the data cube in memory.
#'
#' @param x A stars proxy object that gives a three-dimensional data cube with a 'time'
#' dimension and bands as attributes.
#' @param y An sf object with the coordinates of the training points.
#' @param rows The number of rows of a block. If `NULL` (default), a block has about 50 million
#' values.
#'
#' @return A list with `values`, a numeric array with the dimensions sample, time, and band,
#' and `time`, the dates of the time dimension of `x`. Samples outside of the data cube have
#' missing values.
#'
#' @noRd
#' @keywords internal
sample_array_proxy <- function(x, y, rows = NULL) {

  if (st_crs(x) != st_crs(y)) {
    stop("x and y must have the same coordinate reference system")
  }

  # Row of the data cube with each sample, or NA for a sample outside of the data cube
  xy <- raster_dims(x)
  n_row <- dim(x)[[xy[2]]]
  sample_row <- ceiling(st_cells(x, y) / dim(x)[[xy[1]]])

  blocks <- row_blocks(n_row, chunk_rows(x, n_row, rows))
  in_block <- lapply(seq_len(nrow(blocks)), function(b) {
    which(!is.na(sample_row) & sample_row >= blocks[b, 1] & sample_row <= blocks[b, 2])
  })

  # Without samples in the data cube, the first block gives the dates and the bands
  with_samples <- which(lengths(in_block) > 0)
  if (length(with_samples) == 0) {
    block <- read_rows(x, blocks[1, ])
    values <- array(NA_real_,
                    dim = c(nrow(y), dim(block)[['time']], length(block)),
                    dimnames = list(NULL, NULL, names(block)))
    return(list(values = values, time = to_date(st_get_dimension_values(block, 'time'))))
  }

  values <- NULL

  for (b in with_samples) {

    block_ts <- sample_array(read_rows(x, blocks[b, ]), y[in_block[[b]], ])

    if (is.null(values)) {
      ts_time <- block_ts$time
      values <- array(NA_real_,
                      dim = c(nrow(y), dim(block_ts$values)[2:3]),
                      dimnames = list(NULL, NULL, dimnames(block_ts$values)[[3]]))
    }

    values[in_block[[b]], , ] <- block_ts$values

  }

  return(list(values = values, time = ts_time))

}


#' Convert dates and date-times to calendar dates
#'
#' Date-times are converted in their own time zone, so that the result is the
#' calendar date that is printed for the date-time. `as.Date()` would convert
#' them in UTC, which can be the day before or the day after.
#'
#' @param x A vector of class Date or POSIXt.
#'
#' @return A vector of class Date.
#'
#' @noRd
#' @keywords internal
to_date <- function(x) {

  if (inherits(x, "POSIXt")) {
    return(as.Date(format(x, "%Y-%m-%d")))
  }

  return(as.Date(x))

}


#' Check the additional arguments of twdtw_knn for a data cube
#'
#' The additional arguments are the arguments `origin` and `max_elapsed` of the TWDTW
#' distance and `keep_models`, which are passed to twdtw. Any other argument is an error,
#' so that it is not ignored without a message, for example an argument of a previous
#' version of the package.
#'
#' @param args A list with the additional arguments.
#'
#' @return Invisible NULL. The function stops if an argument is not known.
#'
#' @noRd
#' @keywords internal
check_twdtw_args <- function(args) {

  arg_names <- names(args)
  if (is.null(arg_names)) {
    arg_names <- rep("", length(args))
  }

  unknown <- setdiff(arg_names, c('origin', 'max_elapsed', 'keep_models'))
  if (length(unknown) == 0) {
    return(invisible(NULL))
  }

  msg <- paste0("Unknown arguments in twdtw_knn: ",
                paste(ifelse(unknown == "", "an argument without name", paste0("'", unknown, "'")), collapse = ", "),
                ". The additional arguments are 'origin', 'max_elapsed', and 'keep_models'.")

  # Arguments of previous versions
  if (any(c('formula', 'sampling_freq') %in% unknown)) {
    msg <- paste(msg, "'formula' and 'sampling_freq' were replaced by 'resampling_fun' and 'resampling_freq'.")
  }
  if ('version' %in% unknown) {
    msg <- paste(msg, "'version' is no longer used.")
  }
  if (any(c('start', 'end', 'time') %in% unknown)) {
    msg <- paste(msg, "The dates come from the data cube and from the columns 'start_column' and 'end_column' of y.")
  }

  stop(msg)

}
