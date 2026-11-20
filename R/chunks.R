#' Names of the two raster dimensions of a stars proxy object
#'
#' A stars proxy object has the data cube in files and reads them only when the values are needed.
#' The first raster dimension has the columns and the second has the rows of the images.
#'
#' @param x A stars proxy object.
#'
#' @return A character vector with the names of the two raster dimensions.
#'
#' @noRd
#' @keywords internal
raster_dims <- function(x) {

  xy <- attr(st_dimensions(x), "raster")$dimensions

  if (length(xy) != 2 || anyNA(xy) || !all(xy %in% names(dim(x)))) {
    stop("A data cube that is a stars proxy object must have two raster dimensions")
  }

  return(xy)

}


#' Blocks of rows in which a data cube is read
#'
#' The rows of a data cube are divided into blocks of `rows` rows. A block has at least two
#' rows, because stars cannot read a single row of some proxy objects: a last block with one
#' row is added to the block before it.
#'
#' @param n_row The number of rows of the data cube.
#' @param rows The number of rows of a block.
#'
#' @return A matrix with one row for each block and the columns `from` and `to`.
#'
#' @noRd
#' @keywords internal
row_blocks <- function(n_row, rows) {

  rows <- max(2, min(rows, n_row))

  from <- seq(1, n_row, by = rows)
  to <- pmin(from + rows - 1, n_row)

  n <- length(from)
  if (n > 1 && from[n] == to[n]) {
    to[n - 1] <- to[n]
    from <- from[-n]
    to <- to[-n]
  }

  return(cbind(from = from, to = to))

}


#' Number of rows of a data cube that are read at a time
#'
#' @param x A stars object.
#' @param n_row The number of rows of the data cube.
#' @param chunk_size The number of rows given by the user, or `NULL`. Without it, a stars proxy
#' object is read in blocks of rows with about 50 million values, i.e. about 400 megabytes,
#' and a data cube that is in memory is used as a whole.
#'
#' @return The number of rows.
#'
#' @noRd
#' @keywords internal
chunk_rows <- function(x, n_row, chunk_size = NULL) {

  if (is.null(chunk_size)) {
    if (!inherits(x, "stars_proxy")) {
      return(n_row)
    }
    values_in_row <- prod(dim(x)) * length(x) / n_row
    return(max(2, floor(5e7 / values_in_row)))
  }

  if (!is.numeric(chunk_size) || length(chunk_size) != 1 || is.na(chunk_size) || chunk_size < 1) {
    stop("'chunk_size' must be a number of rows, e.g. chunk_size = 100")
  }

  return(floor(chunk_size))

}


#' Read rows of a stars proxy object into memory
#'
#' The rows are read as `write_stars()` of the stars package reads the parts of a proxy object:
#' only the window with these rows is read from the files, and the operations that are pending
#' for the proxy object are then applied to it, e.g. `split()` to get the bands as attributes.
#'
#' @param x A stars proxy object.
#' @param rows A vector with the first and the last row.
#'
#' @return A three-dimensional stars object with the rows of `x`, with a 'time' dimension and
#' bands as attributes.
#'
#' @noRd
#' @keywords internal
read_rows <- function(x, rows) {

  xy <- raster_dims(x)
  n_col <- dim(x)[[xy[1]]]
  n_row <- dim(x)[[xy[2]]]

  if (rows[1] > 1 || rows[2] < n_row) {
    dims <- st_dimensions(x)
    first_row <- dims[[xy[2]]]$from
    dims[[xy[2]]]$from <- first_row + rows[1] - 1
    dims[[xy[2]]]$to <- first_row + rows[2] - 1
    x <- structure(x, dimensions = dims)
  }

  part <- tryCatch(st_as_stars(x), error = function(e) {
    stop("Rows ", rows[1], " to ", rows[2], " of the data cube could not be read from the stars proxy object: ",
         conditionMessage(e), call. = FALSE)
  })

  if (length(dim(part)) != 3 || !'time' %in% names(dim(part))) {
    stop("The stars proxy object must give a three-dimensional data cube with a 'time' dimension and bands as attributes")
  }

  if (!identical(names(dim(part))[names(dim(part)) != 'time'], xy)) {
    stop("The stars proxy object must give a data cube with the dimensions '", xy[1], "' and '", xy[2], "'")
  }

  # stars reads the whole images if the proxy object was created with the argument RasterIO
  if (dim(part)[[xy[1]]] != n_col || dim(part)[[xy[2]]] != rows[2] - rows[1] + 1) {
    stop("stars did not read rows ", rows[1], " to ", rows[2], " of the data cube, but ", dim(part)[[xy[2]]],
         " rows. This happens if the stars proxy object was created with the argument 'RasterIO': ",
         "select the bands after split() instead, e.g. split(x, 'band')[1:6]", call. = FALSE)
  }

  return(part)

}
