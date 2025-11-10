# Wrapper for rnassqs

Shortcut wrapper with arguments for commonly used parameters in rnassqs.
Intended for internal use.

## Usage

``` r
download_nass_census(
  group_desc = "LIVESTOCK",
  commodity_desc = "CATTLE",
  statisticcat_desc = "INVENTORY",
  short_desc = "CATTLE, INCL CALVES - INVENTORY",
  domain_desc = "TOTAL",
  state_alpha = "TX",
  county_name = "BRAZOS",
  year = "2022",
  ...
)
```

## Arguments

- group_desc:

  character. See `rnassqs::parameter_values("group_desc")`.

- commodity_desc:

  character. See `rnassqs::parameter_values("commodity_desc")`.

- statisticcat_desc:

  character. See `rnassqs::parameter_values("statisticcat_desc")`.

- short_desc:

  character. See `rnassqs::parameter_values("short_desc")`.

- domain_desc:

  character. See `rnassqs::parameter_values("domain_desc")`.

- state_alpha:

  character. See `rnassqs::parameter_values("state_alpha")`.

- county_name:

  character. See `rnassqs::parameter_values("county_name")`.

- year:

  character. See `rnassqs::parameter_values("year")`. Expects one of:
  `c('2022', '2017', '2012', '2007', '2002', '1997')`.

- ...:

  additional arguments carried to
  [`rnassqs::nassqs()`](https://docs.ropensci.org/rnassqs/reference/nassqs.html).

## Value

dataframe
