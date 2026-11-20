# 2. Accuracy and processing time of TWDTW, DTW, random forest, and SVM

This vignette **compares implementations** available in R for
Time-Weighted Dynamic Time Warping (TWDTW) (Maus et al. 2016, 2019) with
Dynamic Time Warping (DTW), with random forest, and with a support
vector machine (SVM), using the example data that come with `dtwSat`. It
gives an idea of the accuracy and of the processing time of each method.
The data set is small, so the results are an illustration and not an
assessment of the methods.W

## Data

We read the training samples and the satellite image time series as in
the [first
vignette](http://r-spatial.github.io/dtwSat/articles/landuse-mapping.md).
There are 291 samples of five land-use classes, and the data-cube has
999 pixels, 23 images, and six bands.

``` r

library(dtwSat)

samples <- st_read(system.file("mato_grosso_brazil/samples.gpkg", package = "dtwSat"), quiet = TRUE)

tif_files <- dir(system.file("mato_grosso_brazil", package = "dtwSat"), pattern = "\\.tif$", full.names = TRUE)
tif_names <- basename(tif_files)
acquisition_date <- as.Date(regmatches(tif_names, regexpr("[0-9]{8}", tif_names)), format = "%Y%m%d")

dc <- read_stars(tif_files,
                 along = list(time = acquisition_date),
                 RasterIO = list(bands = 1:6))
dc <- st_set_dimensions(dc, 3, c("EVI", "NDVI", "RED", "BLUE", "NIR", "MIR"))
dc <- split(dc, c("band"))
```

To give all methods the same input, we take the time series out of the
data-cube as two three-dimensional arrays, one for the samples and one
for all pixels. The first dimension of an array has the time series, the
second the 23 dates, and the third the six bands. For example,
`sample_ts[1, , ]` is the time series of the first sample, with one row
for each date and one column for each band.
[`twdtw_knn()`](https://rdrr.io/pkg/twdtw/man/twdtw_knn.html) and
[`predict()`](https://rdrr.io/r/stats/predict.html) work with these
arrays in the same way as with the data-cube.

``` r

bands <- names(dc)
labels <- samples$label

sample_cube <- st_extract(dc, samples)
sample_ts <- sapply(bands, function(band) matrix(sample_cube[[band]], ncol = length(acquisition_date)), simplify = "array")
pixel_ts <- sapply(bands, function(band) matrix(dc[[band]], ncol = length(acquisition_date)), simplify = "array")

dim(sample_ts)
## [1] 291  23   6
dim(pixel_ts)
## [1] 999  23   6
```

## Models

We compare five models:

- **TWDTW, GAM patterns**: one temporal pattern for each land-use class,
  created with a Generalized Additive Model (GAM), as in the [first
  vignette](http://r-spatial.github.io/dtwSat/articles/landuse-mapping.md).
- **TWDTW, all samples**: the time series of all training samples are
  kept as temporal patterns.
- **DTW, GAM patterns**: Dynamic Time Warping without a time weight,
  calculated with the package
  [dtw](https://CRAN.R-project.org/package=dtw) for the same GAM
  patterns. It matches a time series and a pattern from their first to
  their last observation.
- **Random forest**: the package
  [randomForest](https://CRAN.R-project.org/package=randomForest), with
  the values of all bands on all dates as predictors.
- **SVM**: the package
  [e1071](https://CRAN.R-project.org/package=e1071), with the same
  predictors.

TWDTW has the arguments of the [first
vignette](http://r-spatial.github.io/dtwSat/articles/landuse-mapping.md),
and the other packages have their default settings. Each model has a
function that trains it with the samples `id`, and `classify()` returns
the labels that a model gives to an array of time series.

``` r

gam_fun <- function(data) mgcv::gam(y ~ s(x), data = data)

train_twdtw <- function(id, resampling_fun = NULL) {
  twdtw_knn(x = sample_ts[id, , ],
            y = labels[id],
            time = acquisition_date,
            resampling_fun = resampling_fun,
            cycle_length = 'year',
            time_scale = 'day',
            time_weight = c(steepness = 0.1, midpoint = 50))
}

# The GAM patterns as matrices for the package dtw
train_dtw <- function(id) {
  model <- train_twdtw(id, gam_fun)
  structure(list(labels = model$data$label,
                 patterns = lapply(model$data$observations, function(pattern) as.matrix(pattern[bands]))),
            class = "dtw_patterns")
}

# One row for each time series, with the values of all bands on all dates
predictors <- function(ts) matrix(ts, nrow = dim(ts)[1])

train <- list(
  "TWDTW, GAM patterns" = function(id) train_twdtw(id, gam_fun),
  "TWDTW, all samples"  = function(id) train_twdtw(id),
  "DTW, GAM patterns"   = train_dtw,
  "Random forest"       = function(id) randomForest::randomForest(predictors(sample_ts[id, , ]), factor(labels[id])),
  "SVM"                 = function(id) e1071::svm(predictors(sample_ts[id, , ]), factor(labels[id])))

classify <- function(model, ts) {
  if (inherits(model, "twdtw_knn")) {
    predict(model, newdata = ts, time = acquisition_date)$prediction
  } else if (inherits(model, "dtw_patterns")) {
    # The label of the nearest pattern: dtw() is called for each time series and pattern
    nearest <- apply(ts, 1, function(x) {
      which.min(sapply(model$patterns, function(pattern) dtw::dtw(x, pattern, distance.only = TRUE)$distance))
    })
    model$labels[nearest]
  } else {
    predict(model, predictors(ts))
  }
}
```

## Accuracy

We assess the accuracy with a 5-fold cross-validation: the samples are
divided into five groups, and each group is used once to test a model
that was trained with the other four groups. The samples are pixels of a
few fields, and a pixel is similar to its neighbours. With random
groups, most test samples would be next to a training sample of the same
field, and the accuracy would be too optimistic. We therefore divide the
samples of each land-use class into five groups of neighbouring samples,
using k-means clustering of their coordinates.

``` r

set.seed(1)

group <- integer(nrow(samples))
for (label in unique(labels)) {
  id <- which(labels == label)
  group[id] <- kmeans(st_coordinates(samples)[id, ], centers = 5, nstart = 20)$cluster
}

accuracy <- sapply(train, function(train_model) {
  sapply(1:5, function(k) {
    model <- train_model(which(group != k))
    test <- which(group == k)
    mean(classify(model, sample_ts[test, , ]) == labels[test])
  })
})
```

The table has the share of test samples with the correct label in
percent: the mean of the five groups, and the lowest and the highest
value of a single group.

``` r

accuracy_table <- data.frame(Mean = colMeans(accuracy),
                             Lowest = apply(accuracy, 2, min),
                             Highest = apply(accuracy, 2, max))

knitr::kable(100 * accuracy_table, digits = 1)
```

|                     | Mean | Lowest | Highest |
|:--------------------|-----:|-------:|--------:|
| TWDTW, GAM patterns | 96.3 |   85.2 |     100 |
| TWDTW, all samples  | 95.6 |   85.2 |     100 |
| DTW, GAM patterns   | 95.6 |   85.2 |     100 |
| Random forest       | 94.8 |   85.2 |     100 |
| SVM                 | 94.3 |   85.2 |     100 |

All five models classify about 95% of the test samples correctly. The
differences between the models are small compared with the differences
between the groups.

## Processing time

To measure the processing time, we train each model with all samples and
classify the time series of the pixels. The data-cube has only 999
pixels, so we classify them ten times to get times that can be compared.
Nine pixels have a missing value in one image. TWDTW classifies such
pixels with their remaining observations, as shown in the [first
vignette](http://r-spatial.github.io/dtwSat/articles/landuse-mapping.md),
but random forest and SVM need a value for each predictor. We therefore
leave these nine pixels out.

``` r

complete <- which(complete.cases(predictors(pixel_ts)))
many_ts <- pixel_ts[rep(complete, times = 10), , ]

processing_time <- t(sapply(train, function(train_model) {
  training <- system.time(model <- train_model(seq_along(labels)))[["elapsed"]]
  prediction <- system.time(classify(model, many_ts))[["elapsed"]]
  c(Training = training, Prediction = prediction, Total = training + prediction)
}))

knitr::kable(processing_time, digits = 2)
```

|                     | Training | Prediction | Total |
|:--------------------|---------:|-----------:|------:|
| TWDTW, GAM patterns |     0.24 |       0.08 |  0.32 |
| TWDTW, all samples  |     0.01 |       3.06 |  3.07 |
| DTW, GAM patterns   |     0.24 |      20.72 | 20.96 |
| Random forest       |     0.32 |       0.28 |  0.60 |
| SVM                 |     0.02 |       0.20 |  0.22 |

The table has the time in seconds. The models use their time in
different ways:

- TWDTW with GAM patterns uses most of its time for training, because a
  GAM is fitted for each land-use class and band. The prediction
  compares each time series with only five patterns, and takes about as
  long as the prediction of random forest and SVM.
- TWDTW with all samples has almost no training, but the prediction
  compares each time series with 291 patterns and takes much longer.
- DTW has the training of the GAM patterns. Its prediction calls
  [`dtw::dtw()`](https://rdrr.io/pkg/dtw/man/dtw.html) from R for each
  time series and pattern, which takes most of its time. This says more
  about how the function is called than about the algorithm.

## Limitations

- The data set is small and covers one year of a small area. The five
  groups are parts of the same fields, so the accuracy for another
  region would be lower for all models.
- The models are used without tuning of their settings.
- The processing time was measured once, on the computer that built this
  vignette.

## References

Maus, Victor, Gilberto Camara, Marius Appel, and Edzer Pebesma. 2019.
“dtwSat: Time-Weighted Dynamic Time Warping for Satellite Image Time
Series Analysis in R.” *Journal of Statistical Software* 88 (5): 1–31.
<https://doi.org/10.18637/jss.v088.i05>.

Maus, Victor, Gilberto Camara, Ricardo Cartaxo, Alber Sanchez, Fernando
M. Ramos, and Gilberto R. de Queiroz. 2016. “A Time-Weighted Dynamic
Time Warping Method for Land-Use and Land-Cover Mapping.” *IEEE Journal
of Selected Topics in Applied Earth Observations and Remote Sensing* 9
(8): 3729–39. <https://doi.org/10.1109/JSTARS.2016.2517118>.
