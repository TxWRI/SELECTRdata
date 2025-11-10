# Download and write annual NLCD raster data to file.

Downloads and writes an NLCD SpatRaster to file with extents defined by
`template`. This function downloads the annualized NLCD data products.
See <https://www.mrlc.gov/data/project/annual-nlcd> for more
information.

## Usage

``` r
download_nlcd(
  template,
  year = "2021",
  dataset = "LndCov",
  landmass = "CU",
  output = tempfile(fileext = ".tiff"),
  overwrite = FALSE,
  verbose = FALSE,
  ...
)
```

## Arguments

- template:

  A SpatRaster or SpatVector object. The extent of the returned object
  will match `template`.

- year:

  character, expects a value between `1986:2024`.

- dataset:

  Character. Expects
  `c("LndCov","LndChg","LndCnf","FctImp","ImpDsc","SpcChg")`. Only
  `"LndCov"` is supported at this time.

- landmass:

  Depreciated. Character, one of: `c("CU", "AK", "HI")`.

- output:

  A character file path specifying where the raster file should be
  stored. Defaults to a temporary file.

- overwrite:

  logical. If `TRUE`, filename is overwritten

- verbose:

  Logical, if `TRUE` informative messages will be printed.

- ...:

  additional arguments for for writing files, see
  [`terra::writeRaster()`](https://rspatial.github.io/terra/reference/writeRaster.html)

## Value

A SpatRaster object with file written to `output`

## Examples

``` r
# \donttest{
## This example requires an internet connection to run
dem <- system.file("extdata", "thompsoncreek.tif", package = "SELECTRdata")
dem <- terra::rast(dem)
download_nlcd(template = dem, year = "2024")
#> class       : SpatRaster 
#> size        : 695, 377, 1  (nrow, ncol, nlyr)
#> resolution  : 30, 30  (x, y)
#> extent      : -46575, -35265, 834735, 855585  (xmin, xmax, ymin, ymax)
#> coord. ref. : NAD83 / Conus Albers (EPSG:5070) 
#> source      : file21714c164e39.tiff 
#> color table : 1 
#> categories  : Label 
#> name        :                        Label 
#> min value   :                   Open Water 
#> max value   : Emergent Herbaceous Wetlands 
# }
```
