# Retrieve status code from ArcGIS servers

ArcGIS servers return status codes within the response body itself (as
json). `catch_arcgislayer_error` captures and returns status code
message if there is an error code. Otherwise it will return an invisible
`NULL`. This is primarily an internal function to help our data
retrieval functions fail gracefully in case of eorrs returns by the
server.

## Usage

``` r
catch_arcgislayer_error(furl)
```

## Arguments

- furl:

  the base url.

## Value

character message or invisible `NULL`
