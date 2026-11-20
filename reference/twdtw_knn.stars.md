# Train a TWDTW k-nearest neighbour model with a data cube

This is the method of
[twdtw_knn](https://rdrr.io/pkg/twdtw/man/twdtw_knn.html) for satellite
image time series. It takes the time series of the training samples from
a data cube and creates a k-nearest neighbour (KNN) model with the
Time-Weighted Dynamic Time Warping (TWDTW) algorithm.
[predict](http://r-spatial.github.io/dtwSat/reference/twdtw_knn_predict.stars.md)
classifies a data cube with the model.

## Usage

``` r
# S3 method for class 'stars'
twdtw_knn(
  x,
  y,
  time_weight,
  cycle_length,
  time_scale,
  resampling_fun = NULL,
  resampling_freq = NULL,
  start_column = "start_date",
  end_column = "end_date",
  label_column = "label",
  ...
)

twdtw_knn1(
  x,
  y,
  time_weight,
  cycle_length,
  time_scale,
  resampling_fun = NULL,
  resampling_freq = NULL,
  start_column = "start_date",
  end_column = "end_date",
  label_column = "label",
  ...
)
```

## Arguments

- x:

  A three-dimensional stars object (x, y, time) with bands as
  attributes. It can be a stars proxy object, i.e. a data cube that is
  not in memory. Only the rows of the images that have training samples
  are then read.

- y:

  An sf object with the coordinates of the training points.

- time_weight:

  A numeric vector with length two (steepness and midpoint of logistic
  weight) or a function. See details in
  [twdtw](https://rdrr.io/pkg/twdtw/man/twdtw.html).

- cycle_length:

  The length of the cycle, e.g. phenological cycles. Details in
  [twdtw](https://rdrr.io/pkg/twdtw/man/twdtw.html).

- time_scale:

  Specifies the time scale for the observations. Details in
  [twdtw](https://rdrr.io/pkg/twdtw/man/twdtw.html).

- resampling_fun:

  A function that creates one temporal pattern for each label from all
  samples with this label. If `NULL` (default), the model keeps the time
  series of all samples as temporal patterns. The function receives a
  data frame and returns a model. See details.

- resampling_freq:

  The time between the observations of the temporal patterns created
  with `resampling_fun`, e.g. `16` for 16 days. If `NULL` (default), the
  patterns have the dates of the observations of the samples, in time
  order.

- start_column:

  Name of the column in y with the start date of each training sample.
  Default is 'start_date'.

- end_column:

  Name of the column in y with the end date of each training sample.
  Default is 'end_date'.

- label_column:

  Name of the column in y containing land use labels. Default is
  'label'.

- ...:

  Additional arguments of
  [twdtw_knn](https://rdrr.io/pkg/twdtw/man/twdtw_knn.html): `origin`
  and `max_elapsed` of the TWDTW distance, see
  [twdtw](https://rdrr.io/pkg/twdtw/man/twdtw.html), and `keep_models`,
  see details. Any other argument is an error.

## Value

A model of class 'twdtw_knn', see
[twdtw_knn](https://rdrr.io/pkg/twdtw/man/twdtw_knn.html). In earlier
versions of the package the class of the model was 'twdtw_knn1'.

## Details

The label of a training sample is valid from its start date to its end
date. For each sample, the model uses only the observations of `x`
within this time interval, including observations on the start and end
dates. Samples without observations in their time interval are removed
with a warning.

If `resampling_fun` is `NULL`, the model keeps the time series of all
samples. Such a model has several temporal patterns for each label and
can be classified with more than one nearest neighbour, see the argument
`k` of
[predict.twdtw_knn](https://rdrr.io/pkg/twdtw/man/predict.twdtw_knn.html).

If `resampling_fun` is a function, the model has one temporal pattern
for each label. This reduces the processing time of the classification,
because the TWDTW distance is calculated to the patterns of the labels
and not to the time series of all samples. The function is called for
each label and band with a data frame that has two columns:

- `x`, the dates of the observations of all samples with this label, as
  numbers.

- `y`, the values of the band on these dates.

It returns a model that predicts the values of the temporal pattern,
e.g. `function(data) mgcv::gam(y ~ s(x), data = data)`. The dates of
samples from different years are shifted to the same year before the
function is called. With samples from different years, set
`resampling_freq`, so that the temporal patterns of all labels have
observations at the same interval. See the details of
[twdtw_knn](https://rdrr.io/pkg/twdtw/man/twdtw_knn.html).

The models returned by `resampling_fun`, one for each label and band,
are only used to create the temporal patterns. The model keeps the
patterns and the root mean squared error of each fitted model, and not
the fitted models, which usually contain the time series of all samples.
With `keep_models = TRUE` the fitted models are kept in `smooth_models`,
e.g. to plot them. This makes the model much larger, and the land use
map is the same.

The model is created by
[twdtw_knn](https://rdrr.io/pkg/twdtw/man/twdtw_knn.html) of the package
twdtw. This function takes the time series of the samples from the data
cube.

`twdtw_knn1()` is the name of this function in earlier versions of the
package. It is deprecated: it calls `twdtw_knn()` with the same
arguments and gives a message once per session.

## See also

[twdtw_knn_predict.stars](http://r-spatial.github.io/dtwSat/reference/twdtw_knn_predict.stars.md),
[plot.twdtw_knn](http://r-spatial.github.io/dtwSat/reference/plot.twdtw_knn.md),
[twdtw_knn](https://rdrr.io/pkg/twdtw/man/twdtw_knn.html),
[predict.twdtw_knn](https://rdrr.io/pkg/twdtw/man/predict.twdtw_knn.html)

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
