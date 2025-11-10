# Set GDAL configuration options

A convenience functions for simultaneously setting the GDAL runtime
configuration options in both terra and gdalraster.

## Usage

``` r
set_gdal_config(option, value)
```

## Arguments

- option:

  Character name of a configuration option.

- value:

  Character value to set for the option. `value = ""` (empty string)
  will unset previously set values.

## Value

No return value, called for side effects.

## See also

[`terra::setGDALconfig()`](https://rspatial.github.io/terra/reference/gdal.html)
and
[`gdalraster::set_config_option()`](https://usdaforestservice.github.io/gdalraster/reference/set_config_option.html)

## Examples

``` r
set_gdal_config("GDAL_CACHEMAX", "10%")
## unset
set_gdal_config("GDAL_CACHEMAX", "")
```
