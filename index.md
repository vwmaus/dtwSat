# dtwSat

Provides a fast and robust approach to land use mapping using
multi-dimensional (multi-band) satellite image time series. By
leveraging the Time-Weighted Dynamic Time Warping (TWDTW) distance
metric in tandem with a k-nearest neighbour (KNN) classifier, this
package offers functions to produce land use maps based on distinct
seasonality patterns, commonly observed in the phenological cycles of
vegetation. The approach is described in Maus et al. (2016) and Maus et
al. (2019). A primary advantage of TWDTW is its capability to handle
irregularly sampled and noisy time series, while also requiring minimal
training sets. The package includes tools for training the KNN-TWDTW
model, visualizing temporal patterns, producing land use maps, and
visualizing the results. With one temporal pattern for each land use
class, the classification of a data cube has a processing time
comparable to that of other machine learning models available in ‘R’,
such as random forest and support vector machines.

## Getting Started

You can install the development version of dtwSat from GitHub:

``` r

devtools::install_github("r-spatial/dtwSat")
```

Version 1.0.0 was archived on CRAN in October 2023. Once version 1.0-1
is on CRAN, you can install it using the following command:

``` r

install.packages("dtwSat")
```

After installation, you can read the vignette for a quick start guide:

``` r

vignette("landuse-mapping", "dtwSat")
```

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
