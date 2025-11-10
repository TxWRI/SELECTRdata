# Download U.S. Urban Areas

Downloads a U.S. Census designated urban areas intersecting the spatial
extent of the template SpatRaster.

## Usage

``` r
download_urban_areas(
  template,
  page_size = NULL,
  output = tempfile(fileext = ".gpkg")
)
```

## Arguments

- template:

  A SpatRaster or SpatVector object.

- page_size:

  a numeric value passed to `arcgislayers::arcselect()`. Defaults to
  NULL. Useful when the requests returns a 500 error code.

- output:

  A character file path specifying where the `SpatVector` output object
  should be written. Defaults to a temporary file.

## Value

A terra SpatVector object. If API resources are not available an
invisible `NULL` is returned.

## Examples

``` r
# example code
# \donttest{
## This example requires an internet connection to run
dem <- system.file("extdata", "thompsoncreek.tif", package = "SELECTRdata")
dem <- terra::rast(dem)

ua <- download_urban_areas(template = dem)
ua
#>  class       : SpatVector 
#>  geometry    : polygons 
#>  dimensions  : 1, 9  (geometries, attributes)
#>  extent      : -42298.37, -19741.85, 826766.2, 850727.8  (xmin, xmax, ymin, ymax)
#>  source      : file217124d6038f.gpkg
#>  coord. ref. : +proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs 
#>  names       : OBJECTID UA_CODE            NAME POPULATION POP_DENSITY  HOUSING
#>  type        :    <num>   <chr>           <chr>      <num>       <num>    <num>
#>  values      :      494   18748 College Statio~  2.061e+05        2525 8.65e+04
#>  AREA_SQMI Shape__Area Shape__Length
#>      <num>       <num>         <num>
#>       81.7     0.01991         1.737
# }
```
