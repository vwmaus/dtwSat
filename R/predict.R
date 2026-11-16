#' Classify a data cube with a TWDTW k-nearest neighbour model
#'
#' This is the method for data cubes of \link[twdtw]{twdtw_knn_predict}, the function that
#' \link[twdtw:predict.twdtw_knn]{predict} calls to classify time series with a model created by
#' \link[dtwSat:twdtw_knn.stars]{twdtw_knn}. It classifies the time series of the pixels of a data cube
#' and returns a map. To classify a data cube `x` with a model `m`, call `predict(m, newdata = x)`.
#'
#' @param newdata A three-dimensional stars object (x, y, time) with bands as attributes.
#' @param object A model of class `twdtw_knn`, created by \link[dtwSat:twdtw_knn.stars]{twdtw_knn}.
#' @param ... Arguments of \link[twdtw]{predict.twdtw_knn}, which are passed by `predict()`:
#' `k`, the number of nearest temporal patterns used for each pixel (default 1); `vote`,
#' `"majority"` (default) or `"weighted"`; `type`, `"class"` (default), `"prob"`, or `"neighbors"`;
#' and the arguments of the TWDTW distance that replace the arguments of the model:
#' `time_weight`, `cycle_length`, `time_scale`, `origin`, and `max_elapsed`.
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
twdtw_knn_predict.stars <- function(newdata, object, ...) {

  if (length(dim(newdata)) != 3 || !'time' %in% names(dim(newdata))) {
    stop("newdata must be a three-dimensional stars object with a 'time' dimension")
  }

  if ('time' %in% names(list(...))) {
    stop("The argument 'time' cannot be used with a data cube, which has the dates in its time dimension")
  }

  # Time series of the pixels as an array with the dimensions pixel, time, and band.
  # The missing values remain missing
  newdata_ts <- data_cube_array(newdata)

  # Classify the time series: the result has one row for each pixel
  result <- twdtw_knn_predict(newdata_ts$values, object, time = newdata_ts$time, ...)

  # A map with the spatial dimensions of the data cube
  space_dims <- which(names(dim(newdata)) != 'time')
  map_dim <- dim(newdata)[space_dims]

  return(st_as_stars(lapply(result, function(v) structure(v, dim = map_dim)),
                     dimensions = st_dimensions(newdata)[space_dims]))

}


#' Prepare an Array of Time Series from a stars Data Cube
#'
#' This function reshapes a three-dimensional stars object (x, y, time) with bands as attributes
#' into an array with the dimensions time series, time, and band, which is an input of
#' \link[twdtw]{twdtw_knn_predict}. Missing values remain missing.
#'
#' @param x A three-dimensional stars object with a 'time' dimension.
#'
#' @return A list with `values`, a numeric array with the dimensions time series, time, and band,
#' and `time`, the values of the time dimension of `x`. Each pixel is a time series, in the order
#' of the two spatial dimensions of `x`. The band names are the names of the attributes of `x`.
#'
#' @noRd
#' @keywords internal
data_cube_array <- function(x) {

  # Read the data of a stars proxy object
  x <- st_as_stars(x)

  time_dim <- which(names(dim(x)) == 'time')
  space_dims <- seq_along(dim(x))[-time_dim]

  ts_time <- st_get_dimension_values(x, 'time')
  ts_bands <- names(x)

  values <- array(NA_real_,
                  dim = c(prod(dim(x)[space_dims]), length(ts_time), length(ts_bands)),
                  dimnames = list(NULL, NULL, ts_bands))

  for (i in seq_along(ts_bands)) {
    # Position the time dimension last, so that each row has the time series of a pixel
    values[, , i] <- as.numeric(aperm(unclass(x[[i]]), c(space_dims, time_dim)))
  }

  return(list(values = values, time = ts_time))

}
