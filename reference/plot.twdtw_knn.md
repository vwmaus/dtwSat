# Plot the temporal patterns of a twdtw_knn model

This function visualizes the temporal patterns of a `"twdtw_knn"` model.
It produces a multi-faceted plot, where each facet has the temporal
patterns of a label of the model. Within each facet, different bands or
indices (attributes) are plotted as distinct lines, differentiated by
color.

## Usage

``` r
# S3 method for class 'twdtw_knn'
plot(x, bands = NULL, ...)

# S3 method for class 'twdtw_knn1'
plot(x, bands = NULL, ...)
```

## Arguments

- x:

  A model of class `"twdtw_knn"`, created by
  [twdtw_knn](http://r-spatial.github.io/dtwSat/reference/twdtw_knn.stars.md)
  or by [twdtw_knn](https://rdrr.io/pkg/twdtw/man/twdtw_knn.html).
  Models of class `"twdtw_knn1"`, created with earlier versions of the
  package, are plotted in the same way.

- bands:

  A character vector specifying the bands or indices to plot. If NULL
  (default), all available bands or indices in the data will be plotted.

- ...:

  Additional arguments passed to
  [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html).
  Currently not used.

## Value

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html) object
displaying the temporal patterns.

## See also

[twdtw_knn](http://r-spatial.github.io/dtwSat/reference/twdtw_knn.stars.md)

## Examples

``` r
if (FALSE) { # \dontrun{

# Read training samples
samples_path <-
  system.file("mato_grosso_brazil/samples.gpkg", package = "dtwSat")

samples <- st_read(samples_path, quiet = TRUE)

# Get satellite image time series files
tif_path <- system.file("mato_grosso_brazil", package = "dtwSat")
tif_files <- dir(tif_path, pattern = "\\.tif$", full.names = TRUE)

# Get acquisition dates from the file names
tif_names <- basename(tif_files)
acquisition_date <- regmatches(tif_names, regexpr("[0-9]{8}", tif_names))
acquisition_date <- as.Date(acquisition_date, format = "%Y%m%d")

# Create a 3D datacube
dc <- read_stars(tif_files,
                 proxy = FALSE,
                 along = list(time = acquisition_date),
                 RasterIO = list(bands = 1:6))
dc <- st_set_dimensions(dc, 3, c("EVI", "NDVI", "RED", "BLUE", "NIR", "MIR"))
dc <- split(dc, c("band"))

# Create a model with one temporal pattern for each label
m <- twdtw_knn(
 x = dc,
 y = samples,
 resampling_fun = function(data) mgcv::gam(y ~ s(x), data = data),
 cycle_length = 'year',
 time_scale = 'day',
 time_weight = c(steepness = 0.1, midpoint = 50))

print(m)

# Visualize model patterns
plot(m)

# Classify satellite images
system.time(lu <- predict(m, newdata = dc))

# Visualise land use classification
ggplot() +
  geom_stars(data = lu["prediction"]) +
  theme_minimal()

# Create a model with a custom resampling function:
# the mean of the samples on each date
m <- twdtw_knn(x = dc,
 y = samples,
 resampling_fun = function(data) lm(y ~ factor(x), data = data),
 cycle_length = 'year',
 time_scale = 'day',
 time_weight = c(steepness = 0.1, midpoint = 50))

plot(m)

# Create a model that keeps the time series of all samples
# and classify with the three nearest samples
m <- twdtw_knn(x = dc,
 y = samples,
 cycle_length = 'year',
 time_scale = 'day',
 time_weight = c(steepness = 0.1, midpoint = 50))

lu <- predict(m, newdata = dc, k = 3)

# The share of the votes of each label among the three nearest samples
lu_share <- predict(m, newdata = dc, k = 3, type = "prob")

# A data cube that does not fit in memory: a stars proxy object, which reads the
# images when their values are needed. The files have seven bands. The first six
# are selected after split(), and not with the argument RasterIO
dc_proxy <- read_stars(tif_files, proxy = TRUE, along = "time")
dc_proxy <- st_set_dimensions(dc_proxy, "time", values = acquisition_date)
dc_proxy <- st_set_dimensions(dc_proxy, "band",
                              values = c("EVI", "NDVI", "RED", "BLUE", "NIR", "MIR", "B7"))
dc_proxy <- split(dc_proxy, c("band"))[1:6]

# The model reads the rows of the images that have samples
m <- twdtw_knn(x = dc_proxy,
 y = samples,
 resampling_fun = function(data) mgcv::gam(y ~ s(x), data = data),
 cycle_length = 'year',
 time_scale = 'day',
 time_weight = c(steepness = 0.1, midpoint = 50))

# The images are read and classified in blocks of 10 rows
lu <- predict(m, newdata = dc_proxy, chunk_size = 10)

} # }
```
