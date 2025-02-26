#' Check internet connectivity
#'
#' Checks dns via curl and return cli message or invisible null.
#' @param host url
#' @param call defaults to [`rlang::caller_env()`] and passed to [`cli::cli_inform`]
#'
#' @return `TRUE` or message with invisible `NULL`
#' @keywords internal
#'
#' @importFrom cli cli_inform
check_connectivity <- function(host,
                               call = rlang::caller_env()) {
  ## check connectivity
  if (!has_internet_2(host)) {
    cli::cli_inform(c("i" = paste0("No connection to ", host, " available!")),
                    call = call)

    return(invisible(NULL))
  } else {TRUE}
}

check_gdalraster_gdal_config <- function(call = rlang::caller_env()) {
  if(gdalraster::get_config_option("AWS_NO_SIGN_REQUEST") != "YES") {
    cli::cli_abort(c("Please set the GDAL configuration in {.pkg gdalraster} with: {.code gdalraster::set_config_options('AWS_NO_SIGN_REQUEST', 'YES')}"),
                   call = call)
  }
}

check_spat_ras <- function(x,
                           arg = rlang::caller_arg(x),
                           call = rlang::caller_env()) {
  if(!inherits(x, c("SpatRaster", "SpatRasterCollection"))) {
    cli::cli_abort("The object supplied to {.arg {arg}} must be a {.cls SpatRaster} or {.cls SpatRasterCollection} object. Try loading the raster with {.code terra::rast()}.",
                   call = call)
  }
}

check_string <- function(x,
                         arg = rlang::caller_arg(x),
                         call = rlang::caller_env()) {
  if (!rlang::is_string(x)) {
    cli::cli_abort(c("The object provided for {.arg {arg}} must be a {.cls {class('string')}}.",
                     "x" = "You've supplied a {.cls {class(x)}}"),
                   call = call)
  }
}

check_string_contains <- function(x,
                                  y,
                                  arg = rlang::caller_arg(x),
                                  call = rlang::caller_env()) {
  if(!x %in% y) {
    cli::cli_abort(c("The object provided to {.arg {arg}} must be a {.cls character} string with a value that is one of {.or {.val {y}}}.",
                     "x" = "You've supplied a {.cls {class(x)}} with a value {.val {x}}"),
                   call = call)
  }

}

check_terra_gdal_config <- function(call = rlang::caller_env()) {
  if(terra::getGDALconfig("AWS_NO_SIGN_REQUEST") != "YES") {
    cli::cli_abort(c("Please set the GDAL configuration in {.pkg terra} with: {.code terra::setGDALconfig('AWS_NO_SIGN_REQUEST=YES')}"),
                   call = call)
  }
}



#' nslookup wrapper
#'
#' Wrapper for [`curl::nslookup`] that returns logical without error.
#' @param host hostname
#'
#' @return logical value
#' @keywords internal
#'
#' @importFrom curl nslookup
has_internet_2 <- function(host) {
  !is.null(curl::nslookup(host, error = FALSE))
}


#' Is there a NASS token?
#'
#' Checks if a NASS token has been set in the user environment.
#' @return logical
#' @export
#'
#' @examples
#' has_nass_token()
has_nass_token <- function() {
  key <- Sys.getenv("NASSQS_TOKEN")
  if(identical(key, "")) {
    return(FALSE)
    } else {TRUE}
}


#' Retrieve status code from ArcGIS servers
#'
#' ArcGIS servers return status codes within the response body itself (as json).
#' `catch_arcgislayer_error` captures and returns status code message if there
#' is an error code. Otherwise it will return an invisible `NULL`. This is
#' primarily an internal function to help our data retrieval functions
#' fail gracefully in case of eorrs returns by the server.
#'
#' @param furl the base url.
#'
#' @return character message or invisible `NULL`
#' @importFrom arcgisutils arc_base_req
#' @importFrom httr2 req_url_query req_perform resp_body_json
#' @keywords internal

catch_arcgislayer_error <- function(furl) {

  resp_string <- arcgisutils::arc_base_req(furl)
  resp_string <-  httr2::req_url_query(resp_string, f = "json")
  resp_string <-  httr2::req_perform(resp_string)
  resp_string  <- httr2::resp_body_json(resp_string, check_type = FALSE)
  resp_string <- capture_error_message(resp_string)

  if(is.null(resp_string)) {
    invisible(NULL)
  } else {resp_string}

}

## adapted from arcgisutils https://github.com/R-ArcGIS/arcgisutils/blob/main/R/utils-requests.R
capture_error_message <- function(resp_string) {
  e <- resp_string[["error"]]
  if (!is.null(e)) {
    err_msg <- strwrap(
      paste0("  Error", e$messageCode, ": ", e$message),
      prefix = "    ",
      initial = ""
    )

    full_msg <- c(
      "Status code: ",
      e[["code"]],
      "\n",
      paste0(err_msg, collapse = "\n")
    )

    c(paste0(full_msg, collapse = ""), "i" = e$details)
  } else {
    invisible(NULL)
  }
}



# request urls --------------------------------------------------------------

## builds the ECHO API request

#' Construct URL used in the httr call
#'
#' @param resource Character vector, specifies API path to ECHO's webservices
#' @param ... Character vector, specifies the parameters sent in the GET request
#' @importFrom httr2 request req_url_path_append req_url_query
#' @importFrom rlang !!!
#' @keywords internal
#' @noRd
requestECHO <- function(resource, ...) {

  params <- list(
    ...
  )

  request <- httr2::request("https://echodata.epa.gov/echo")
  request <-  httr2::req_url_path_append(request, resource)
  request <-  httr2::req_url_query(request, !!!params)


}
