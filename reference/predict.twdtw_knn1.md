# Models of class twdtw_knn1

Models created with version 1.0.0 of the package have the class
`twdtw_knn1`. These methods classify and print them as models of class
`twdtw_knn`, which is the class of the models created with this version.
See
[plot.twdtw_knn](http://r-spatial.github.io/dtwSat/reference/plot.twdtw_knn.md)
for the plot.

## Usage

``` r
# S3 method for class 'twdtw_knn1'
predict(object, newdata, ...)

# S3 method for class 'twdtw_knn1'
print(x, ...)
```

## Arguments

- object, x:

  A model of class `twdtw_knn1`.

- newdata:

  The time series to classify: a three-dimensional stars object (x, y,
  time) with bands as attributes, or another form of time series of
  [predict.twdtw_knn](https://rdrr.io/pkg/twdtw/man/predict.twdtw_knn.html).

- ...:

  Arguments of the TWDTW distance that replace the arguments of the
  model, see
  [predict.twdtw_knn](https://rdrr.io/pkg/twdtw/man/predict.twdtw_knn.html).
  `k` cannot be used.

## Value

[`predict()`](https://rdrr.io/r/stats/predict.html) returns the result
of
[predict.twdtw_knn](https://rdrr.io/pkg/twdtw/man/predict.twdtw_knn.html):
a stars object for a data cube, see
[twdtw_knn_predict.stars](http://r-spatial.github.io/dtwSat/reference/twdtw_knn_predict.stars.md).
[`print()`](https://rdrr.io/r/base/print.html) returns the model,
invisibly.

## Details

[`predict()`](https://rdrr.io/r/stats/predict.html) classifies with the
nearest temporal pattern (`k = 1`), as version 1.0.0, and gives a
message once per session, because this method will be deprecated in a
future release. Create the model again with
[twdtw_knn](http://r-spatial.github.io/dtwSat/reference/twdtw_knn.stars.md)
to get a model of class `twdtw_knn`, which can be classified with other
values of `k`.

The models of version 1.0.0 have a period in place of each hyphen of a
band name, so the hyphens in the band names of `newdata` are replaced in
the same way.

## See also

[twdtw_knn](http://r-spatial.github.io/dtwSat/reference/twdtw_knn.stars.md),
[twdtw_knn_predict.stars](http://r-spatial.github.io/dtwSat/reference/twdtw_knn_predict.stars.md)
