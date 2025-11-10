# Download TIGER county spatial data.

Shortcut function that downloads and extracts TIGER U.S. County
boundaries and returns them as a terra SpatVector object.

## Usage

``` r
download_counties(template, output = tempfile(fileext = ".gpkg"))
```

## Arguments

- template:

  A SpatRaster or SpatVector object. The extent of the returned object
  will match `template`.

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

counties <- download_counties(template = dem)
# }
```
