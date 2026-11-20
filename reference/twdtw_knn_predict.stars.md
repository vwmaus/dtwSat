# Classify a data cube with a TWDTW k-nearest neighbour model

This is the method for data cubes of
[twdtw_knn_predict](https://rdrr.io/pkg/twdtw/man/twdtw_knn_predict.html),
the function that
[predict](https://rdrr.io/pkg/twdtw/man/predict.twdtw_knn.html) calls to
classify time series with a model created by
[twdtw_knn](http://r-spatial.github.io/dtwSat/reference/twdtw_knn.stars.md).
It classifies the time series of the pixels of a data cube and returns a
map. To classify a data cube `x` with a model `m`, call
`predict(m, newdata = x)`.

## Usage

``` r
# S3 method for class 'stars'
twdtw_knn_predict(newdata, object, ..., chunk_size = NULL)
```

## Arguments

- newdata:

  A three-dimensional stars object (x, y, time) with bands as
  attributes. It can be a stars proxy object, i.e. a data cube that is
  not in memory, which is read and classified in parts. See details.

- object:

  A model of class `twdtw_knn`, created by
  [twdtw_knn](http://r-spatial.github.io/dtwSat/reference/twdtw_knn.stars.md).

- ...:

  Arguments of
  [predict.twdtw_knn](https://rdrr.io/pkg/twdtw/man/predict.twdtw_knn.html),
  which are passed by
  [`predict()`](https://rdrr.io/r/stats/predict.html): `k`, the number
  of nearest temporal patterns used for each pixel (default 1); `vote`,
  `"majority"` (default) or `"weighted"`; `type`, `"class"` (default),
  `"prob"`, or `"neighbors"`; and the arguments of the TWDTW distance
  that replace the arguments of the model: `time_weight`,
  `cycle_length`, `time_scale`, `origin`, and `max_elapsed`.

- chunk_size:

  The number of rows of the data cube that are classified at a time,
  e.g. `predict(m, newdata = x, chunk_size = 100)`. If `NULL` (default),
  a data cube in memory is classified as a whole, and a stars proxy
  object in blocks of rows with about 50 million values. See details.

## Value

A stars object with the spatial dimensions of `newdata`. Its attributes
depend on `type`:

- `"class"`: `prediction`, a factor with the predicted label, and
  `distance`, the TWDTW distance to the nearest temporal pattern with
  this label.

- `"prob"`: one attribute for each label of the model, with the share of
  the votes of the label among the `k` nearest patterns.

- `"neighbors"`: `prediction_1`, ..., `prediction_k` and `distance_1`,
  ..., `distance_k`, the labels and the distances of the `k` nearest
  patterns, where 1 is the nearest.

## Details

There are two ways to classify a data cube `x`, i.e. a three-dimensional
stars object (x, y, time) with bands as attributes, with a model `m`.
Both return a stars object with the spatial dimensions of the data cube.
They differ in the pixels with missing values:

- `predict(m, newdata = x)` calls this function and classifies all
  pixels. An observation with a missing value in any band is not used,
  and the pixel is classified with its remaining observations. Only a
  pixel without any complete observation has `NA` in the map.

- `predict(x, model = m)` calls
  [predict.stars](https://r-spatial.github.io/stars/reference/predict.stars.html),
  as for other models, and does not classify pixels with missing values.
  stars replaces the missing values with zeros, passes the time series
  to
  [predict.twdtw_knn](https://rdrr.io/pkg/twdtw/man/predict.twdtw_knn.html)
  as a data frame, and then sets the result of every pixel that had a
  missing value in any band or date to `NA`. The zeros are therefore not
  part of the result, but a single missing value is enough for a pixel
  to have no prediction.

The arguments `k`, `vote`, and `type` can be used in both ways, e.g.
`predict(m, newdata = x, k = 3)` and `predict(x, model = m, k = 3)`.
They are described in
[predict.twdtw_knn](https://rdrr.io/pkg/twdtw/man/predict.twdtw_knn.html):
`k` sets how many of the nearest temporal patterns are used for each
pixel, `vote` how their votes are counted, and `type` what the map
shows. A model created without `resampling_fun` keeps the time series of
all samples and has several patterns for each label, so that the votes
of the `k` nearest patterns can differ. With one pattern for each label,
the prediction is the label of the nearest pattern for any `k`.

The time series must have the bands of the model, i.e. the attributes of
the data cube must have the names of the attributes of the data cube
that was used to create the model.

A data cube that does not fit in memory can be classified as a stars
proxy object, which
[read_stars](https://r-spatial.github.io/stars/reference/read_stars.html)
creates with `proxy = TRUE` and which reads the images only when their
values are needed. `predict(m, newdata = x)` reads and classifies a
proxy object in blocks of rows and returns the complete map, which has
only a few values for each pixel. `chunk_size` sets the number of rows
of a block. By default a block has about 50 million values, i.e. pixels
times dates times bands, and its classification needs about one gigabyte
of memory. A block has at least two rows. The proxy object must be
created without the argument `RasterIO` of `read_stars()`, with which
stars reads the whole images for each block: select the bands after
[`split()`](https://rdrr.io/r/base/split.html), as in the examples.
`chunk_size` can also be used with a data cube that is in memory, to use
less memory during the classification.

## See also

[twdtw_knn](http://r-spatial.github.io/dtwSat/reference/twdtw_knn.stars.md),
[predict.twdtw_knn](https://rdrr.io/pkg/twdtw/man/predict.twdtw_knn.html),
[predict.stars](https://r-spatial.github.io/stars/reference/predict.stars.html)

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
