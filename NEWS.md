# dtwSat v1.0-1

## The model is created with `twdtw_knn()`:

* `twdtw_knn()` replaces `twdtw_knn1()`. The k-nearest neighbour model and its `predict()` method are now in the twdtw package, for any time series. dtwSat adds the methods for satellite image time series: `twdtw_knn()` takes the time series of the training samples from a data cube, and `predict()` returns the land use map. `twdtw_knn1()` is deprecated: it calls `twdtw_knn()` with the same arguments and gives a message once per session.

* The class of the model is now `twdtw_knn`, because `predict()` is no longer limited to one nearest neighbour. The methods for the class `twdtw_knn1` remain for models created with version 1.0.0.

* New arguments for temporal patterns: The arguments `formula` and `sampling_freq` were replaced by `resampling_fun` and `resampling_freq`. `resampling_fun` is a function that receives a data frame with the time (`x`) and the values of a band (`y`) of the samples of a label and returns a model, for example `function(data) mgcv::gam(y ~ s(x), data = data)`. If `resampling_freq` is not given, the patterns have the dates of the observations of the samples. Version 1.0.0 inferred the frequency from the time series in this case, 16 days for the example data. With `resampling_freq = 16`, this version gives the temporal patterns and the land use map of version 1.0.0 for the example data. The package mgcv is now only suggested.

* The argument `label_colum` is now called `label_column`.

* Data cubes with a single band can be used to train a model and can be classified. Before, `twdtw_knn1()` stopped with "Unrecognized format in" followed by a date (vwmaus/twdtw#7).

* Arguments that `twdtw_knn()` does not know are an error. Before, they were ignored without a message, for example the arguments `formula` and `sampling_freq` of version 1.0.0.

* Start and end dates of the samples: The model uses only the observations of the data cube between the start date and the end date of each training sample. In version 1.0.0 the two columns were required, but had no effect. Samples without observations in this time interval are removed with a warning.

* The observations of the temporal patterns are in time order. They were out of order if the samples of a label did not have the same dates, for example samples from different years.

* Samples with the first observation on 31 December of a leap year can be used with a resampling function. Their dates became missing values.

* The band names of a model are the names of the attributes of the data cube. Version 1.0.0 replaced hyphens in the names with periods.

* A data cube without a dimension called `time` gives a clear error message.

* `print()` shows the root mean squared error of the models fitted with `resampling_fun`.

* The model keeps the root mean squared error of the models fitted with `resampling_fun`, and not the fitted models, which contain the time series of the samples. For the example data, a model with one GAM pattern for each label takes 22 KB in memory. With the new argument `keep_models = TRUE` the model also keeps the fitted models in `smooth_models` and takes 5.4 MB. The temporal patterns and the land use map are the same.

## Changes in `predict()`:

* Faster classification: The distances between all time series and all temporal patterns are calculated in a single call to the Fortran routines of the twdtw package. For the example data this is more than 50 times faster, and the land use map is the same.

* New argument `k`: `predict()` can classify each time series with its `k` nearest temporal patterns. The default is `k = 1`, the nearest pattern, as before. The new argument `vote` sets how the `k` nearest patterns vote for their labels: each pattern has one vote (`"majority"`, the default) or votes with the weight 1/distance (`"weighted"`). A vote can only change the prediction for a model that keeps all samples, i.e. a model without `resampling_fun`.

* New argument `type`: it sets what the map shows. `"class"` (default) gives the label with the most votes and the distance to its nearest pattern, `"prob"` gives the share of the votes of each label, with one attribute for each label, and `"neighbors"` gives the labels and the distances of the `k` nearest patterns without a vote.

* `predict.twdtw_knn1()` will be deprecated in a future release. It is only used for models created with earlier versions, classifies them with `k = 1`, and gives a message once per session.

* The land use map has a second attribute, `distance`, with the TWDTW distance of each pixel to the nearest temporal pattern. `predict()` for a model returns a data frame with the columns `prediction` and `distance`, and no longer a factor.

* Missing values: `predict(model, newdata = cube)` classifies a data cube without `stars::predict()`. An observation with a missing value is not used, and the pixel is classified with its remaining observations. With `predict(cube, model = model)`, stars replaces missing values with zeros and sets the result of all pixels that had a missing value to `NA`.

* The spatial dimensions of the data cube can have other names than `x` and `y`.

* Data cubes that do not fit in memory: `predict(model, newdata = cube)` reads and classifies a stars proxy object in blocks of rows and returns the complete map. The new argument `chunk_size` sets the number of rows of a block, and can also be used with a data cube in memory. `twdtw_knn()` reads only the rows of a proxy object that have training samples.

* The argument `version` of twdtw is no longer used.

* Arguments that `predict()` does not know are an error.

## Other changes:

* Requires twdtw version 1.1-0 or newer. It has the k-nearest neighbour model, and it fixes a memory error that crashed R in `predict()` when the time series have more observations than the temporal patterns (#63).

* Requires stars version 0.7-0 or newer. It is the first version that keeps band names such as `EVI-16` when it passes the time series to `predict()`; earlier versions replaced the hyphen with a period. The packages tidyr, methods, and utils are no longer imported. The package tibble is now suggested, because `stars::geom_stars()`, which plots the land use map in the vignette and in the examples, needs it.

* New vignette "2. Accuracy and processing time of TWDTW, DTW, random forest, and SVM", with a spatial cross-validation and the processing time for the example data. It uses the suggested packages dtw, randomForest and e1071.

* `shift_dates()` was removed. The dates of the samples are shifted by twdtw when it creates the temporal patterns.

* The package proxy is no longer imported.

* The examples and the vignette take the dates of the images from the file names. The full path of the files can have other digits, which were taken for a date.

# dtwSat v1.0.0

## Major Updates:

* Dependency Updates: This release removes obsolete dependencies, notably raster, rgdal, and sp.

* Reduced Dependencies: We have significantly minimized the number of package dependencies to streamline the installation and update process.

* Spatial Data Handling with sf and stars: Spatial data handling has been overhauled. We've introduced support for the sf and stars packages to enhance this capability.

* Improved Workflow Compatibility: The package now offers a workflow that aligns more seamlessly with other prevalent image classification workflows.

# dtwSat v0.2.8

* Adds faster implementation of TWDTW for logistic weight function

* Fixes small bugs 

* Adds vignettes 

# dtwSat v0.2.7

* Adds support to user defined TWDTW weight function

* Drop support to parallel processing

* Adds a minimalist function called twdtwReduceTime that is 3x faster than twdtwApply. This function can be used for high level parallel processing implemented by users

# dtwSat v0.2.6

* Fixes error in to - from : non-numeric argument to binary operator in "twdtwAssess"

* Fixes bug in .twdtw fundtion 

* Adds function for fast time series classification "twdtw_reduce_time" (~3x faster than twdtwApply)

# dtwSat v0.2.5

* Adds dtwSat paper published on Journal of Statistical Software 

* Fixing bugs 
  
  Fix error in plotAccuracy 
  
  Generalizes twdwAssess to cases with only one map 
  
  Fixes error in getTimeSeries due to time series with only one no observation 

# dtwSat v0.2.4

* New features 

  Include the function twdtwApplyParallel for TWDTW parallel processing using the package snow 
  
  Include writeRaster for twdtwRaster class 
  
  Improve tests and documentation 
  
  Improve memory usage of twdtwApply 
  
  Improve memory usage and speed of twdtwClassify 
  
  Auto recognition of the argument "doy" to avoid naming the argument "doy = doy" 
  
* Fixing bugs
  
  Fix bug in twdtwAssess for class twdtwMatches 
  
  Fix bug in twdtwRaster 
  
# dtwSat v0.2.3

* New features 

  Register TWDTW as a distance function into package proxy 
  
* Fixing bugs

  Fix typos in plot labels 

# dtwSat v0.2.2 Release Notes

* New features

  New accuracy metrics (twdtwAssess) for classified map, including User's and Producer's accuracy, and area uncertainty. 

  Include methods for accuracy visualization (plot and LaTeX tables) 

* Update data set names 

  Rename the data sets in ordes to avoid future overwriting of functions and data sets. "example\_ts" replaced with "MOD13Q1.ts". Tthe data sets are now called:
    
    MOD13Q1.MT.yearly.patterns	Data: patterns time series
    MOD13Q1.patterns.list	Data: patterns time series
    MOD13Q1.ts	Data: An example of satellite time series
    MOD13Q1.ts.labels	Data: Labels of the satellite time series in MOD13Q1.ts
    MOD13Q1.ts.list

* Fixing bugs

  Fix bug in twdtwApply wrong sign in 'by' argument
  
  Fix bug in time index for twdtwApply-twdtwRaster
  
# dtwSat v0.2.1 Release Notes

* Fix Solaris installation errors. 

# dtwSat v0.2.0 Release Notes

* Include Fortran optimization 
 
   This version includes functions written in Fortran. 

* Obsolete features 

  The S4 class 'twdtw' no longer exists. 

* New features
 
  New S4 classes: twdtwTimeSeries, twdtwMatches, and twdtwRaster.
 
  plot methods for twdtwRaster object: 'maps', 'area', 'changes', and 'distance'.
 
  plot methods for twdtwTimeSeries objects: ''patterns'' and ''timeseries''.
 
  plot methods for twdtwMatches objects: ''paths'', ''matches'', ''alignments'', ''classification'', ''cost'', ''patterns'', and ''timeseries''.
 
  createPattern function to create temporal patterns based on set of time series.
 
  getTimeSeries extract time series from raster objects.
 
  twdtwApply apply the TWDTW analysis for raster and time series objects.


# dtwSat v0.1.1 Release Notes

* New features
 
  'normalizeQuery' new normalization feature for TWDTW
 
  'template.list' new dataset. List of template time series  
 
  arguments 'from' and 'to' in 'classifyIntervals' updated to include 'character' or 'Dates' in in the format 'yyyy-mm-dd'
 
    Align query and template by name if names not null in 'twdtw' function

* deprecated features
 
    argument 'x' from function 'waveletSmoothing' is deprecated and is scheduled to be removed in the next version. Please use 'timeseries' instead.
 
    argument 'template' from functions 'twdtw' and 'mtwdtw' is deprecated and is scheduled to be removed in the next version. Please use 'timeseries' instead.
 
  argument 'normalized' is deprecated and is scheduled to be removed in the next version from all methods 
 
  'createTimeSequence' is deprecated. Use 'getModisTimeSequence' instead.
 
  Fix function name. 'classfyIntervals' is deprecated. Use 'classifyIntervals' instead.

* Fixing bugs
 
  Fix plot intervals in plotClassify
 
  replace range(x) for range(x, na.rm=TRUE) in all methods 
 
  Bug fixed in cost matrix indexing 

 
# dtwSat v0.1.0 Release Notes

* First version of dtwSat on CRAN

# dtwSat v0.0.1 Release Notes

* Earlier dtwSat development version
