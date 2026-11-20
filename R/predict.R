#' Classify a data cube with a TWDTW k-nearest neighbour model
#'
#' This is the method for data cubes of \link[twdtw]{twdtw_knn_predict}, the function that
#' \link[twdtw:predict.twdtw_knn]{predict} calls to classify time series with a model created by
#' \link[dtwSat:twdtw_knn.stars]{twdtw_knn}. It classifies the time series of the pixels of a data cube
#' and returns a map. To classify a data cube `x` with a model `m`, call `predict(m, newdata = x)`.
#'
#' @param newdata A three-dimensional stars object (x, y, time) with bands as attributes. It can be
#' a stars proxy object, i.e. a data cube that is not in memory, which is read and classified in
#' parts. See details.
#' @param object A model of class `twdtw_knn`, created by \link[dtwSat:twdtw_knn.stars]{twdtw_knn}.
#' @param ... Arguments of \link[twdtw]{predict.twdtw_knn}, which are passed by `predict()`:
#' `k`, the number of nearest temporal patterns used for each pixel (default 1); `vote`,
#' `"majority"` (default) or `"weighted"`; `type`, `"class"` (default), `"prob"`, or `"neighbors"`;
#' and the arguments of the TWDTW distance that replace the arguments of the model:
#' `time_weight`, `cycle_length`, `time_scale`, `origin`, and `max_elapsed`.
#' @param chunk_size The number of rows of the data cube that are classified at a time, e.g.
#' `predict(m, newdata = x, chunk_size = 100)`. If `NULL` (default), a data cube in memory is
#' classified as a whole, and a stars proxy object in blocks of rows with about 50 million values.
#' See details.
#'
#' @details There are two ways to classify a data cube `x`, i.e. a three-dimensional stars
#' object (x, y, time) with bands as attributes, with a model `m`. Both return a stars object
#' with the spatial dimensions of the data cube. They differ in the pixels with missing values:
#'
#' \itemize{
#'   \item `predict(m, newdata = x)` calls this function and classifies all pixels. An
#'   observation with a missing value in any band is not used, and the pixel is classified
#'   with its remaining observations. Only a pixel without any complete observation has `NA`
#'   in the map.
#'   \item `predict(x, model = m)` calls \link[stars]{predict.stars}, as for other models,
#'   and does not classify pixels with missing values. stars replaces the missing values with
#'   zeros, passes the time series to \link[twdtw]{predict.twdtw_knn} as a data frame, and then
#'   sets the result of every pixel that had a missing value in any band or date to `NA`. The
#'   zeros are therefore not part of the result, but a single missing value is enough for a
#'   pixel to have no prediction.
#' }
#'
#' The arguments `k`, `vote`, and `type` can be used in both ways, e.g.
#' `predict(m, newdata = x, k = 3)` and `predict(x, model = m, k = 3)`. They are described in
#' \link[twdtw]{predict.twdtw_knn}: `k` sets how many of the nearest temporal patterns are used
#' for each pixel, `vote` how their votes are counted, and `type` what the map shows. A model
#' created without `resampling_fun` keeps the time series of all samples and has several
#' patterns for each label, so that the votes of the `k` nearest patterns can differ. With one
#' pattern for each label, the prediction is the label of the nearest pattern for any `k`.
#'
#' The time series must have the bands of the model, i.e. the attributes of the data cube must
#' have the names of the attributes of the data cube that was used to create the model.
#'
#' A data cube that does not fit in memory can be classified as a stars proxy object, which
#' \link[stars]{read_stars} creates with `proxy = TRUE` and which reads the images only when their
#' values are needed. `predict(m, newdata = x)` reads and classifies a proxy object in blocks of
#' rows and returns the complete map, which has only a few values for each pixel. `chunk_size`
#' sets the number of rows of a block. By default a block has about 50 million values, i.e.
#' pixels times dates times bands, and its classification needs about one gigabyte of memory.
#' A block has at least two rows. The proxy object must be created without the argument
#' `RasterIO` of `read_stars()`, with which stars reads the whole images for each block: select
#' the bands after `split()`, as in the examples. `chunk_size` can also be used with a data cube
#' that is in memory, to use less memory during the classification.
#'
#' @return A stars object with the spatial dimensions of `newdata`. Its attributes depend on `type`:
#' \itemize{
#'   \item `"class"`: `prediction`, a factor with the predicted label, and `distance`, the TWDTW
#'   distance to the nearest temporal pattern with this label.
#'   \item `"prob"`: one attribute for each label of the model, with the share of the votes of
#'   the label among the `k` nearest patterns.
#'   \item `"neighbors"`: `prediction_1`, ..., `prediction_k` and `distance_1`, ..., `distance_k`,
#'   the labels and the distances of the `k` nearest patterns, where 1 is the nearest.
#' }
#'
#' @seealso \link[dtwSat:twdtw_knn.stars]{twdtw_knn}, \link[twdtw]{predict.twdtw_knn}, \link[stars]{predict.stars}
#'
#' @inherit twdtw_knn.stars examples
#'
#' @export
twdtw_knn_predict.stars <- function(newdata, object, ..., chunk_size = NULL) {

  is_proxy <- inherits(newdata, "stars_proxy")

  # A stars proxy object has its dimensions after the data are read
  if (!is_proxy && (length(dim(newdata)) != 3 || !'time' %in% names(dim(newdata)))) {
    stop("newdata must be a three-dimensional stars object with a 'time' dimension")
  }

  if ('time' %in% names(list(...))) {
    stop("The argument 'time' cannot be used with a data cube, which has the dates in its time dimension")
  }

  # The two spatial dimensions of the map. The rows of the second are classified in blocks
  space_dims <- if (is_proxy) raster_dims(newdata) else names(dim(newdata))[names(dim(newdata)) != 'time']
  map_dim <- dim(newdata)[space_dims]
  blocks <- row_blocks(map_dim[[2]], chunk_rows(newdata, map_dim[[2]], chunk_size))

  result <- NULL

  for (b in seq_len(nrow(blocks))) {

    # Time series of the pixels of the block as an array with the dimensions pixel, time,
    # and band. The missing values remain missing
    if (is_proxy) {
      newdata_ts <- data_cube_array(read_rows(newdata, blocks[b, ]))
    } else {
      newdata_ts <- data_cube_array(newdata, rows = blocks[b, ])
    }

    # Classify the time series: the result has one row for each pixel of the block
    block_result <- twdtw_knn_predict(newdata_ts$values, object, time = newdata_ts$time, ...)

    if (nrow(blocks) == 1) {
      result <- block_result
      break
    }

    # The result of all pixels, which is filled block by block. The pixels of a block
    # follow each other, because the first spatial dimension varies fastest
    if (is.null(result)) {
      result <- lapply(block_result, function(v) rep(v[NA_integer_], prod(map_dim)))
    }

    pixels <- ((blocks[b, 1] - 1) * map_dim[[1]] + 1):(blocks[b, 2] * map_dim[[1]])
    for (v in names(result)) {
      result[[v]][pixels] <- block_result[[v]]
    }

  }

  # A map with the spatial dimensions of the data cube
  return(st_as_stars(lapply(result, function(v) structure(v, dim = map_dim)),
                     dimensions = st_dimensions(newdata)[space_dims]))

}


#' Prepare an Array of Time Series from a stars Data Cube
#'
#' This function reshapes a three-dimensional stars object (x, y, time) with bands as attributes
#' into an array with the dimensions time series, time, and band, which is an input of
#' \link[twdtw]{twdtw_knn_predict}. Missing values remain missing.
#'
#' @param x A three-dimensional stars object with a 'time' dimension, in memory.
#' @param rows A vector with the first and the last row of the second spatial dimension that
#' are used. If `NULL` (default), all rows are used.
#'
#' @return A list with `values`, a numeric array with the dimensions time series, time, and band,
#' and `time`, the values of the time dimension of `x`. Each pixel is a time series, in the order
#' of the two spatial dimensions of `x`. The band names are the names of the attributes of `x`.
#'
#' @noRd
#' @keywords internal
data_cube_array <- function(x, rows = NULL) {

  time_dim <- which(names(dim(x)) == 'time')
  space_dims <- seq_along(dim(x))[-time_dim]

  if (is.null(rows)) {
    rows <- c(1, dim(x)[[space_dims[2]]])
  }
  all_rows <- rows[1] == 1 && rows[2] == dim(x)[[space_dims[2]]]

  # Index of the values of the rows in an array of a band
  index <- rep(list(TRUE), 3)
  index[[space_dims[2]]] <- rows[1]:rows[2]

  ts_time <- st_get_dimension_values(x, 'time')
  ts_bands <- names(x)

  values <- array(NA_real_,
                  dim = c(dim(x)[[space_dims[1]]] * (rows[2] - rows[1] + 1), length(ts_time), length(ts_bands)),
                  dimnames = list(NULL, NULL, ts_bands))

  for (i in seq_along(ts_bands)) {
    band <- unclass(x[[i]])
    if (!all_rows) {
      band <- do.call(`[`, c(list(band), index, list(drop = FALSE)))
    }
    # Position the time dimension last, so that each row has the time series of a pixel
    values[, , i] <- as.numeric(aperm(band, c(space_dims, time_dim)))
  }

  return(list(values = values, time = ts_time))

}
