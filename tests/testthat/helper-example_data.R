# Example data that comes with the package

# Training samples: points with a land use label and the start and end dates of the label
example_samples <- function() {
  st_read(system.file("mato_grosso_brazil/samples.gpkg", package = "dtwSat"), quiet = TRUE)
}

# Data cube (x, y, time) with six bands as attributes and 23 images per year.
# With 'years = 2' the images are repeated one year later. This gives a data cube
# that is longer than the time interval of the samples.
example_cube <- function(years = 1) {
  tif_files <- dir(system.file("mato_grosso_brazil", package = "dtwSat"), pattern = "\\.tif$", full.names = TRUE)
  tif_names <- basename(tif_files)
  acquisition_date <- as.Date(regmatches(tif_names, regexpr("[0-9]{8}", tif_names)), format = "%Y%m%d")
  days_later <- rep(365 * (seq_len(years) - 1), each = length(tif_files))
  dc <- read_stars(rep(tif_files, years),
                   proxy = FALSE,
                   along = list(time = rep(acquisition_date, years) + days_later),
                   RasterIO = list(bands = 1:6))
  dc <- st_set_dimensions(dc, 3, c("EVI", "NDVI", "RED", "BLUE", "NIR", "MIR"))
  split(dc, c("band"))
}
