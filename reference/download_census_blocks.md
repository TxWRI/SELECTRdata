# Download U.S. Census block boundaries

Downloads U.S. Census block boundaries and associated information for
blocks within the spatial extent of the template SpatRaster.

## Usage

``` r
download_census_blocks(
  template,
  year = "2020",
  page_size = NULL,
  output = tempfile(fileext = ".gpkg")
)
```

## Arguments

- template:

  A SpatRaster or SpatVector object.

- year:

  A character value. Any of the following values should work:
  `c("2000","2010","2020")`.

- page_size:

  a numeric value passed to `arcgislayers::arcselect()`. Defaults to
  NULL. Useful when the requests returns a 500 error code.

- output:

  A character file path specifying where the raster file should be
  stored. Defaults to a temporary file.

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

blocks <- download_census_blocks(template = dem)
blocks
#>  class       : SpatVector 
#>  geometry    : polygons 
#>  dimensions  : 1034, 22  (geometries, attributes)
#>  extent      : -52023.23, -33083.29, 831065, 862761.1  (xmin, xmax, ymin, ymax)
#>  source      : file20bc4760f2b3.gpkg
#>  coord. ref. : +proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs 
#>  names       :  OBJECTID           GEOID SUFFIX       NAME     ALAND AWATER
#>  type        :     <num>           <chr>  <chr>      <chr>     <num>  <num>
#>  values      : 6.782e+06 480410001041000     NA BLOCK 1000  4.39e+06      0
#>                6.782e+06 480410001041001     NA BLOCK 1001 1.491e+05      0
#>                6.782e+06 480410001041002     NA BLOCK 1002 2.104e+06      0
#>     INTPTLAT    INTPTLON State        County (and 12 more)
#>        <chr>       <chr> <chr>         <chr>              
#>  +30.7378816 -96.3737787 Texas Brazos County              
#>  +30.7394108 -96.3824119 Texas Brazos County              
#>  +30.7258887 -96.3768835 Texas Brazos County              
# }
```
