# Download county level total livestock

Provides a wrapper to
[`rnassqs::nassqs()`](https://docs.ropensci.org/rnassqs/reference/nassqs.html)
with simplified arguments for making queries that return county level
livestock estimates. This function requires an API key from USDA NASS
Quickstats. See details for more information.

## Usage

``` r
download_nass_livestock(
  state_alpha = NULL,
  county_name = NULL,
  year = "2022",
  ...
)
```

## Arguments

- state_alpha:

  Two letter character abbreviation for state.

- county_name:

  Character, county name.

- year:

  A character to filter the NASS Agriculture Census year. Expects one
  of: `c('2022', '2017', '2012', '2007', '2002', '1997')`.

- ...:

  Additional arguments passed to
  [`rnassqs::nassqs()`](https://docs.ropensci.org/rnassqs/reference/nassqs.html)

## Value

A dataframe.

## Details

This function requires an API key to make requests on the USDA NASS
QuickStats service.

1.  Obtain an API key from <https://quickstats.nass.usda.gov/api/>.

2.  Set the `NASSQS_TOKEN` variable in your global environment. This can
    be done with `Sys.setenv('NASSQS_TOKEN' = <your api key>)`, or
    `usethis::edit_r_environment()`.

3.  Restart your session.

## Examples

``` r
download_nass_livestock(state_alpha = "TX", county_name = "Brazos", year = "2022")
#> Downloading: 1 kB     Downloading: 1 kB     Downloading: 1 kB     Downloading: 1 kB     Downloading: 1 kB     Downloading: 1 kB     
```
