
#' Download county level total livestock
#'
#' Provides a wrapper to `rnassqs::nassqs()` with simplified arguments
#' for making queries that return county level livestock estimates. This
#' function requires an API key from USDA NASS Quickstats. See details for
#' more information.
#'
#' @param state_alpha Two letter character abbreviation for state.
#' @param county_name Character, county name.
#' @param year A character to filter the NASS Agriculture Census year. Expects one of: `c('2022', '2017', '2012', '2007', '2002', '1997')`.
#' @param ... Additional arguments passed to `rnassqs::nassqs()`
#'
#' @details
#' This function requires an API key to make requests on the USDA NASS QuickStats service.
#' 1. Obtain an API key from [https://quickstats.nass.usda.gov/api/](https://quickstats.nass.usda.gov/api/).
#' 2. Set the `NASSQS_TOKEN` variable in your global environment. This can be done with `Sys.setenv('NASSQS_TOKEN' = <your api key>)`, or `usethis::edit_r_environment()`.
#' 3. Restart your session.
#'
#' @return A dataframe.
#' @export
#' @examplesIf has_nass_token()
#' download_nass_livestock(state_alpha = "TX", county_name = "Brazos", year = "2022")
download_nass_livestock <- function(state_alpha = NULL,
                                    county_name = NULL,
                                    year = "2022",
                                    ...) {
  download_nass_census(state_alpha = state_alpha,
                       county_name = county_name,
                       year = year,
                       ...)
}




#' Wrapper for rnassqs
#'
#' Shortcut wrapper with arguments for commonly used parameters in rnassqs. Intended for internal use.
#' @param group_desc character. See `rnassqs::parameter_values("group_desc")`.
#' @param commodity_desc character. See `rnassqs::parameter_values("commodity_desc")`.
#' @param statisticcat_desc character. See `rnassqs::parameter_values("statisticcat_desc")`.
#' @param short_desc character. See `rnassqs::parameter_values("short_desc")`.
#' @param domain_desc character. See `rnassqs::parameter_values("domain_desc")`.
#' @param state_alpha character. See `rnassqs::parameter_values("state_alpha")`.
#' @param county_name character. See `rnassqs::parameter_values("county_name")`.
#' @param year character. See `rnassqs::parameter_values("year")`. Expects one of: `c('2022', '2017', '2012', '2007', '2002', '1997')`.
#' @param ... additional arguments carried to `rnassqs::nassqs()`.
#'
#' @return dataframe
#' @export
#' @importFrom rnassqs nassqs
#' @keywords internal
download_nass_census <- function(group_desc = "LIVESTOCK",
                                 commodity_desc = "CATTLE",
                                 statisticcat_desc = "INVENTORY",
                                 short_desc = "CATTLE, INCL CALVES - INVENTORY",
                                 domain_desc = "TOTAL",
                                 state_alpha = "TX",
                                 county_name = "BRAZOS",
                                 year = "2022",
                                 ...) {

  ##check that api token is defined
  key <- Sys.getenv("NASSQS_TOKEN")
  if(identical(key, "")) {
    rlang::abort("Please use `Sys.setenv('NASSQS_TOKEN' = <your api key>)` to set your api key. You can obtain an API key from https://quickstats.nass.usda.gov/api/.")
  }

  ## all args should be strings
  if(!is.null(group_desc)) {
    check_string(group_desc)
  }
  if(!is.null(commodity_desc)) {
    check_string(commodity_desc)
  }
  if(!is.null(statisticcat_desc)) {
    check_string(statisticcat_desc)
  }
  if(!is.null(short_desc)) {
    check_string(short_desc)
  }
  if(!is.null(domain_desc)) {
    check_string(domain_desc)
  }
  if(!is.null(state_alpha)) {
    check_string(state_alpha)
  }
  if(!is.null(county_name)) {
    check_string(county_name)
  }
  if(!is.null(year)) {
    check_string(year)
  }

  ## Relying on census data, so year must be one of 2022, 2017, 2012, 2007, 2002, 1997
  check_census_year(year, arg = "year")

  params <- list(source_desc = "CENSUS",
                 sector_desc = "ANIMALS & PRODUCTS",
                 group_desc = group_desc,
                 commodity_desc = commodity_desc,
                 statisticcat_desc = statisticcat_desc,
                 short_desc = short_desc,
                 domain_desc = domain_desc,
                 state_alpha = state_alpha,
                 county_name = county_name,
                 year = year)

  livestock <- rnassqs::nassqs(params, ...)
}


check_census_year <- function(x,
                              arg = rlang::caller_arg(x),
                              call = rlang::caller_env()) {
  if (!x %in% c("2022", "2017", "2012", "2007", "2002", "1997")) {
    cli::cli_abort("{.arg {arg}} must be one of c('2022', '2017', '2012', '2007', '2002', '1997')",
                   call = call)
  }
}
