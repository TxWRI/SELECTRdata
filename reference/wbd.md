# Thompsons Creek Watershed Boundary

Polygon boundary of the Thompsons Creek watershed (Collecge Station,
Texas). This watershed boundary was created using the hydroreinformced
raster from the United States Geological Survey (USGS) and United States
Environmental Protection Agency (EPA) National Hydrography Dataset Plus
(NHDPlus) dataset.

## See also

[`thompson()`](https://txwri.github.io/SELECTRdata/reference/thompson.md)

## Examples

``` r
# \donttest{
gpkg <- system.file("extdata", "thompsoncreek.gpkg", package = "SELECTRdata")
terra::vect(gpkg, layer = "wbd")
#>  class       : SpatVector 
#>  geometry    : polygons 
#>  dimensions  : 1, 21  (geometries, attributes)
#>  extent      : -46572.15, -35267.4, 834724.6, 855575.7  (xmin, xmax, ymin, ymax)
#>  source      : thompsoncreek.gpkg (wbd)
#>  coord. ref. : +proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs 
#>  names       : OBJECTID    HUC_8     HUC_10       HUC_12     ACRES NCONTRB_A
#>  type        :    <num>    <chr>      <chr>        <chr>     <num>     <num>
#>  values      : 1.25e+05 12070101 1207010107 120701010702 3.428e+04         0
#>  HU_10_GNIS HU_12_GNIS      HU_10_NAME HU_10_MOD (and 11 more)
#>       <chr>      <chr>           <chr>     <chr>              
#>           0          0 Old River-Braz~     IT,AD              
# }
```
