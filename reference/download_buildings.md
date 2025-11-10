# Download building footprints

Downloads building footprints and attribute data from FEMA's USA
Structures dataset
(https://fema.maps.arcgis.com/home/item.html?id=0ec8512ad21e4bb987d7e848d14e7e24#overview).

## Usage

``` r
download_buildings(
  template,
  return = "SpatVector",
  output = tempfile(fileext = ".gpkg")
)
```

## Arguments

- template:

  A SpatRaster or SpatVector object. The extent of the returned object
  will match `template`.

- return:

  A character object, either `SpatVector` or `sf`. Defaults to
  `SpatVector`.

- output:

  A character file path specifying where the `SpatVector` file should be
  written. Defaults to a temporary file.

## Value

A `SpatVector` or `sf` object with extents matching the `SpatRaster`
object provided in the `template` argument. If API resources are not
available an invisible `NULL` is returned.

## Examples

``` r
# \donttest{
## This example requires an internet connection to run
dem <- system.file("extdata", "thompsoncreek.tif", package = "SELECTRdata")
dem <- terra::rast(dem)

buildings <- download_buildings(template = dem)
#> Registered S3 method overwritten by 'jsonify':
#>   method     from    
#>   print.json jsonlite
#> [working] (0 + 0) -> 6 -> 2 | ■■■■■■■■■                         25%
#> [working] (0 + 0) -> 0 -> 8 | ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100%
# }
```
