#' Download TIGER county spatial data.
#'
#' Shortcut function that downloads and extracts TIGER U.S. County boundaries
#' and returns them as a terra SpatVector object.
#'
#' @param template A SpatRaster object.
#' @param output A character file path specifying where the raster file should be stored. Defaults to a temporary file.
#'
#' @return A terra SpatVector object. If API resources are not available an invisible `NULL` is returned.
#' @export
#' @examples
#' # example code
#' \donttest{
#' ## This example requires an internet connection to run
#' dem <- system.file("extdata", "thompsoncreek.tif", package = "SELECTRdata")
#' dem <- terra::rast(dem)
#'
#' counties <- download_counties(template = dem)
#' }
#'
download_counties <- function(template,
                              output = tempfile(fileext = ".gpkg")) {

  ## are we online?
  ## check connectivity
  if (!isTRUE(check_connectivity("tigerweb.geo.census.gov"))) {
    return(invisible(NULL))
  }

  ## check template if a spatraster
  check_spat_ras(template)

  furl <- "https://tigerweb.geo.census.gov/arcgis/rest/services/TIGERweb/State_County/MapServer"

  ## check for service errors first
  msg <- catch_arcgislayer_error(furl)
  if(!is.null(msg)) {
    cli::cli_alert_info(msg[[1]])
    return(invisible(NULL))
  } else {
    state_county <- arcgislayers::arc_open(furl)
  }


  county_layer <- arcgislayers::get_layer(state_county, id = 7)

  ## create a bbox object from DEM
  bounds <- sf::st_bbox(template)

  ## retrieve the cropped featuer layer
  county_sf <- arcgislayers::arc_select(county_layer,
                                        filter_geom = bounds)

  county_vect <- terra::vect(county_sf)

  terra::writeVector(county_vect,
                     filename = output)

  return(vect(output))
}


