#' Prepare a Time Series Tibble from a 2D stars Object with Bands and Time Attributes
#'
#' This function reshapes a data frame, which has been converted from a stars object, into a nested wide tibble format.
#' The stars object conversion often results in columns named in formats like "band.YYYY.MM.DD", "XYYYY.MM.DD.band", or "YYYY.MM.DD.band".
#'
#' @param x A data frame derived from a stars object containing time series data in wide format.
#' The column names should adhere to one of the following formats: "band.YYYY.MM.DD", "XYYYY.MM.DD.band", or "YYYY.MM.DD.band".
#'
#' @return A nested tibble in wide format. Each row of the tibble corresponds to a unique 'ts_id' that maintains the order from the original stars object.
#' The nested structure contains observations (time series) for each 'ts_id', including the 'time' of each observation, and individual bands are presented as separate columns.
#'
#' @noRd
#' @keywords internal
prepare_time_series <- function(x) {

  # Remove the 'geom' column if it exists
  x$geom <- NULL
  x$x <- NULL
  x$y <- NULL
  var_names <- names(x)
  var_names <- var_names[!var_names %in% 'label']

  # Extract date and band information from the column names
  date_band <- get_date_band(var_names)

  # Construct tiem sereis
  ns <- nrow(x)
  x$ts_id <- 1:ns
  if (!'label' %in% names(x)) {
    x$label <- NA
  }
  x <- pivot_longer(x, !c('ts_id', 'label'), names_to = 'band_date', values_to = 'value')
  x$band <- rep(date_band$band, ns)
  x$time <- rep(date_band$time, ns)
  x$band_date <- NULL
  result_df <- pivot_wider(x, id_cols = c('ts_id', 'label', 'time'), names_from = 'band', values_from = 'value')
  result_df <- nest(result_df, .by = c('ts_id', 'label'), .key = 'observations')

  return(result_df)

}


#' Get the Date and the Band from the Names of the Columns of a Time Series Data Frame
#'
#' The column names of a data frame derived from a stars object are in formats like
#' "band.YYYY.MM.DD", "XYYYY.MM.DD.band", or "YYYY.MM.DD.band".
#'
#' @param var_names A character vector with the column names.
#' @param strict If TRUE, a name in another format is an error. If FALSE, it has
#' a missing date and a missing band.
#'
#' @return A data frame with one row for each name and the columns `time` and `band`.
#'
#' @noRd
#' @keywords internal
get_date_band <- function(var_names, strict = TRUE) {

  date_band <- do.call(rbind, lapply(var_names, function(name) {

    # Replace any hyphen with periods for consistent processing
    name <- gsub("-", "\\.", name)

    # Extract date and band info based on different name patterns
    if (grepl("^.+\\.\\d{4}\\.\\d{2}\\.\\d{2}$", name)) {
      date_str <- gsub("^.*?(\\d{4}\\.\\d{2}\\.\\d{2})$", "\\1", name)
      band_str <- gsub("^(.+)\\.\\d{4}\\.\\d{2}\\.\\d{2}$", "\\1", name)
    }
    else if (grepl("^X\\d{4}\\.\\d{2}\\.\\d{2}\\..+$", name)) {
      date_str <- gsub("^X(\\d{4}\\.\\d{2}\\.\\d{2})\\..+$", "\\1", name)
      band_str <- gsub("^X\\d{4}\\.\\d{2}\\.\\d{2}\\.(.+)$", "\\1", name)
    }
    else if (grepl("^\\d{4}\\.\\d{2}\\.\\d{2}\\..+$", name)) {
      date_str <- gsub("^(\\d{4}\\.\\d{2}\\.\\d{2})\\..+$", "\\1", name)
      band_str <- gsub("^\\d{4}\\.\\d{2}\\.\\d{2}\\.(.+)$", "\\1", name)
    }
    else if (strict) {
      stop(paste("Unrecognized format in:", name))
    }
    else {
      return(data.frame(time = as.Date(NA), band = NA_character_))
    }

    # Convert the date string to Date format
    date <- to_date_time(gsub("\\.", "-", date_str))

    return(data.frame(time = date, band = band_str))
  }))

  return(date_band)

}


#' Prepare an Array of Time Series from a Data Frame Derived from a stars Object
#'
#' This function reshapes a data frame in wide format into an array with the dimensions
#' time series, time, and band, which is the input of \link[twdtw]{twdtw_knn}.
#'
#' @param x A data frame with one row for each time series and one column for each band and date.
#' The column names adhere to the formats of [get_date_band()]. Other columns, such as the
#' coordinates of the pixels, are not used.
#'
#' @return A list with `values`, a numeric array with the dimensions time series, time, and band,
#' and `time`, the dates of the time dimension in the order of the columns of `x`.
#'
#' @noRd
#' @keywords internal
time_series_array <- function(x) {

  x <- as.data.frame(x)

  # Columns that are not observations do not have a band and a date in their name
  date_band <- get_date_band(names(x), strict = FALSE)
  is_observation <- !is.na(date_band$time) & !is.na(date_band$band)
  if (!any(is_observation)) {
    stop("newdata has no columns named with a band and a date, e.g. 'EVI.2011-09-14'")
  }

  ts_time <- unique(date_band$time[is_observation])
  ts_bands <- unique(date_band$band[is_observation])

  values <- array(NA_real_,
                  dim = c(nrow(x), length(ts_time), length(ts_bands)),
                  dimnames = list(NULL, NULL, ts_bands))

  for (j in which(is_observation)) {
    values[, match(date_band$time[j], ts_time), match(date_band$band[j], ts_bands)] <- as.numeric(x[[j]])
  }

  return(list(values = values, time = ts_time))

}


#' Prepare an Array of Time Series from a stars Data Cube
#'
#' This function reshapes a three-dimensional stars object (x, y, time) with bands as attributes
#' into an array with the dimensions time series, time, and band, which is the input of
#' \link[twdtw]{twdtw_knn}. Missing values remain missing.
#'
#' @param x A three-dimensional stars object with a 'time' dimension.
#'
#' @return A list with `values`, a numeric array with the dimensions time series, time, and band,
#' and `time`, the values of the time dimension of `x`. Each pixel is a time series, in the order
#' of the two spatial dimensions of `x`.
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

  for (band in ts_bands) {
    # Position the time dimension last, so that each row has the time series of a pixel
    values[, , band] <- as.numeric(aperm(unclass(x[[band]]), c(space_dims, time_dim)))
  }

  return(list(values = values, time = ts_time))

}
