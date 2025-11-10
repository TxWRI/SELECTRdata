# Check internet connectivity

Checks dns via curl and return cli message or invisible null.

## Usage

``` r
check_connectivity(host, call = rlang::caller_env())
```

## Arguments

- host:

  url

- call:

  defaults to
  [`rlang::caller_env()`](https://rlang.r-lib.org/reference/stack.html)
  and passed to
  [`cli::cli_inform`](https://cli.r-lib.org/reference/cli_abort.html)

## Value

`TRUE` or message with invisible `NULL`
