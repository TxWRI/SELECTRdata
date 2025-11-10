# Thompsons Creek Elevation Data

Elevation raster of the Thompsons Creek watershed outside of College
Station, Texas. This data is obtained from the hydroreinforced digital
elevation model (DEM) rasters that are part of the United States
Geological Survey (USGS) and United States Environmental Protection
Agency (EPA) National Hydrography Dataset Plus (NHDPlus).

## See also

<https://nhdplus.com/NHDPlus/NHDPlusV2_home.php>

## Examples

``` r
# \donttest{
thompson <- system.file("extdata", "thompsoncreek.tif", package = "SELECTRdata")
terra::rast(thompson)
#> class       : SpatRaster 
#> size        : 695, 377, 1  (nrow, ncol, nlyr)
#> resolution  : 30, 30  (x, y)
#> extent      : -46575, -35265, 834735, 855585  (xmin, xmax, ymin, ymax)
#> coord. ref. : +proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs 
#> source      : thompsoncreek.tif 
#> name        : hydrodem 
#> min value   :   -76879 
#> max value   :   512599 
# }
```
