#' Plot the temporal patterns of a twdtw_knn model
#'
#' This function visualizes the temporal patterns of a \code{"twdtw_knn"} model.
#' It produces a multi-faceted plot, where each facet has the temporal patterns of a
#' label of the model. Within each facet, different bands or indices (attributes)
#' are plotted as distinct lines, differentiated by color.
#'
#' @param x A model of class \code{"twdtw_knn"}, created by \link[dtwSat:twdtw_knn.stars]{twdtw_knn}
#' or by \link[twdtw]{twdtw_knn}. Models of class \code{"twdtw_knn1"}, created with earlier versions
#' of the package, are plotted in the same way.
#'
#' @param bands A character vector specifying the bands or indices to plot.
#' If NULL (default), all available bands or indices in the data will be plotted.
#'
#' @param ... Additional arguments passed to \code{\link[ggplot2]{ggplot}}. Currently not used.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object displaying the temporal patterns.
#'
#' @seealso \link[dtwSat:twdtw_knn.stars]{twdtw_knn}
#'
#' @inherit twdtw_knn.stars examples
#'
#' @export
plot.twdtw_knn <- function(x, bands = NULL, ...) {

  patterns <- lapply(x$data$observations, as.data.frame)
  labels <- as.character(x$data$label)

  # Select bands
  if (is.null(bands)) {
    bands <- setdiff(names(patterns[[1]]), 'time')
  }

  missing_bands <- setdiff(bands, names(patterns[[1]]))
  if (length(missing_bands) > 0) {
    stop(paste("The model does not have the bands:", paste(missing_bands, collapse = ", ")))
  }

  # Arrange the patterns in a long-format data.frame for ggplot2,
  # with one row for each pattern, band, and observation
  df <- do.call(rbind, lapply(seq_along(patterns), function(i) {
    pattern <- patterns[[i]]
    data.frame(id = i,
               label = labels[i],
               time = rep(pattern$time, times = length(bands)),
               band = rep(bands, each = nrow(pattern)),
               value = unlist(pattern[bands], use.names = FALSE),
               stringsAsFactors = FALSE)
  }))

  # Construct the ggplot
  gp <- ggplot(df, aes(x = .data$time, y = .data$value, colour = .data$band, group = interaction(.data$id, .data$band))) +
    geom_line() +
    facet_wrap(~label) +
    theme(legend.position = "bottom") +
    guides(colour = guide_legend(title = "Bands")) +
    ylab("Value") +
    xlab("Time")

  return(gp)

}


#' @rdname plot.twdtw_knn
#' @export
plot.twdtw_knn1 <- function(x, bands = NULL, ...) {

  plot.twdtw_knn(x, bands = bands, ...)

}
