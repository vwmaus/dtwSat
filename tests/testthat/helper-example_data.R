# Example data that comes with the package

# Training samples: points with a land use label and the start and end dates of the label
example_samples <- function() {
  st_read(system.file("mato_grosso_brazil/samples.gpkg", package = "dtwSat"), quiet = TRUE)
}

# Dates moved to start on the same day of the year in 1970, keeping the days between
# them. The dates of each sample are moved in this way before a resampling function
# creates the temporal patterns
shifted_dates <- function(dates) {
  first <- as.Date(paste(1970, format(dates[1], "%j")), format = "%Y %j")
  dates - as.numeric(dates[1] - first)
}

# Data cube (x, y, time) with six bands as attributes and 23 images per year.
# With 'years = 2' the images are repeated in the next year. This gives a data cube
# that is longer than the time interval of the samples. The images of the next year
# are 365 days later, or on the same days of the year as in the MOD13Q1 product
# if 'same_day_of_year = TRUE'.
example_cube <- function(years = 1, same_day_of_year = FALSE) {
  tif_files <- dir(system.file("mato_grosso_brazil", package = "dtwSat"), pattern = "\\.tif$", full.names = TRUE)
  tif_names <- basename(tif_files)
  acquisition_date <- as.Date(regmatches(tif_names, regexpr("[0-9]{8}", tif_names)), format = "%Y%m%d")
  image_date <- do.call(c, lapply(seq_len(years) - 1, function(i) {
    if (same_day_of_year) {
      year <- as.integer(format(acquisition_date, "%Y")) + i
      as.Date(paste(year, format(acquisition_date, "%j")), format = "%Y %j")
    } else {
      acquisition_date + 365 * i
    }
  }))
  dc <- read_stars(rep(tif_files, years),
                   proxy = FALSE,
                   along = list(time = image_date),
                   RasterIO = list(bands = 1:6))
  dc <- st_set_dimensions(dc, 3, c("EVI", "NDVI", "RED", "BLUE", "NIR", "MIR"))
  split(dc, c("band"))
}
