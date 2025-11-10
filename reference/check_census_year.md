# Check year argument

Check year argument

## Usage

``` r
check_census_year(x, arg = rlang::caller_arg(x), call = rlang::caller_env())
```

## Arguments

- x:

  character

- arg:

  defaults to
  [`rlang::caller_arg()`](https://rlang.r-lib.org/reference/caller_arg.html)

- call:

  defaults to
  [`rlang::caller_env()`](https://rlang.r-lib.org/reference/stack.html)

## Value

error or nothing
