# Check gdalraster AWS signin option

**\[deprecated\]** No longer used internally by SELECTRdata. Checks the
config options for gdalraster AWS_NO_SIGN_REQUEST != "YES

## Usage

``` r
check_gdalraster_gdal_config(call = rlang::caller_env())
```

## Arguments

- call:

  environments that define where check_gdalraster_config was called.

## Value

called for side effect.
