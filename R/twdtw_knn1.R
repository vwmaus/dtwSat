#' @rdname twdtw_knn.stars
#' @export
twdtw_knn1 <- function(x, y, time_weight, cycle_length, time_scale,
                       resampling_fun = NULL, resampling_freq = NULL,
                       start_column = 'start_date', end_column = 'end_date',
                       label_column = 'label', ...) {

  message_once("twdtw_knn1",
               "twdtw_knn1() is deprecated and will be removed in a future release of dtwSat. ",
               "Use twdtw_knn(), which has the same arguments and creates the same model.")

  # twdtw_knn1() was a function for data cubes
  if (!inherits(x, "stars")) {
    stop("x must be a three-dimensional stars object with a 'time' dimension")
  }

  model <- twdtw_knn.stars(x = x,
                           y = y,
                           time_weight = time_weight,
                           cycle_length = cycle_length,
                           time_scale = time_scale,
                           resampling_fun = resampling_fun,
                           resampling_freq = resampling_freq,
                           start_column = start_column,
                           end_column = end_column,
                           label_column = label_column,
                           ...)

  model$call <- match.call()

  return(model)

}


#' Models of class twdtw_knn1
#'
#' Models created with version 1.0.0 of the package have the class `twdtw_knn1`. These methods
#' classify and print them as models of class `twdtw_knn`, which is the class of the models
#' created with this version. See \link[dtwSat]{plot.twdtw_knn} for the plot.
#'
#' @param object,x A model of class `twdtw_knn1`.
#' @param newdata The time series to classify: a three-dimensional stars object (x, y, time)
#' with bands as attributes, or another form of time series of \link[twdtw]{predict.twdtw_knn}.
#' @param ... Arguments of the TWDTW distance that replace the arguments of the model, see
#' \link[twdtw]{predict.twdtw_knn}. `k` cannot be used.
#'
#' @details `predict()` classifies with the nearest temporal pattern (`k = 1`), as version 1.0.0,
#' and gives a message once per session, because this method will be deprecated in a future
#' release. Create the model again with \link[dtwSat:twdtw_knn.stars]{twdtw_knn} to get a model of
#' class `twdtw_knn`, which can be classified with other values of `k`.
#'
#' The models of version 1.0.0 have a period in place of each hyphen of a band name, so the
#' hyphens in the band names of `newdata` are replaced in the same way.
#'
#' @return `predict()` returns the result of \link[twdtw]{predict.twdtw_knn}: a stars object for
#' a data cube, see \link[dtwSat]{twdtw_knn_predict.stars}. `print()` returns the model, invisibly.
#'
#' @seealso \link[dtwSat:twdtw_knn.stars]{twdtw_knn}, \link[dtwSat]{twdtw_knn_predict.stars}
#'
#' @export
predict.twdtw_knn1 <- function(object, newdata, ...) {

  if ('k' %in% names(list(...))) {
    stop(paste("predict() uses k = 1 for a model of class 'twdtw_knn1'. Create the model again with",
               "twdtw_knn() to get a model of class 'twdtw_knn', which can be classified with other values of k."))
  }

  message_once("predict_knn1",
               "predict.twdtw_knn1() will be deprecated in a future release of dtwSat. ",
               "It is used for models of class 'twdtw_knn1' and classifies with k = 1. ",
               "Create the model again with twdtw_knn() to get a model of class 'twdtw_knn', ",
               "which is classified with predict() and its argument k.")

  # The band names of these models have periods in place of hyphens
  if (inherits(newdata, c("stars", "data.frame"))) {
    names(newdata) <- gsub("-", ".", names(newdata), fixed = TRUE)
  }

  predict(as_twdtw_knn(object), newdata, k = 1, ...)

}


#' @rdname predict.twdtw_knn1
#' @export
print.twdtw_knn1 <- function(x, ...) {

  print(as_twdtw_knn(x), ...)

  invisible(x)

}


#' Change the class of a model of version 1.0.0 to the class of this version
#'
#' @param object A model of class `twdtw_knn1`.
#'
#' @return The model with the class `twdtw_knn`.
#'
#' @noRd
#' @keywords internal
as_twdtw_knn <- function(object) {

  class(object) <- "twdtw_knn"

  return(object)

}


# State of the R session, used to give a message only once
.dtwSat_session <- new.env(parent = emptyenv())


#' Give a message once per R session
#'
#' The messages about deprecated functions are given once per session, so that they are
#' not repeated for each call in a script.
#'
#' @param id A name for the message.
#' @param ... The text of the message.
#'
#' @return Invisible NULL.
#'
#' @noRd
#' @keywords internal
message_once <- function(id, ...) {

  if (isTRUE(.dtwSat_session[[id]])) {
    return(invisible(NULL))
  }

  .dtwSat_session[[id]] <- TRUE

  message(..., " This message is given once per session.")

  return(invisible(NULL))

}
