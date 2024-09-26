#' Download U.S. Census block boundaries
#'
#' Downloads U.S. Census block boundaries and associated information for
#' blocks within the spatial extent of the template SpatRaster.
#'
#' @param template A SpatRaster object.
#' @param year A character value. Any of the following values should work: `c("2000","2010","2020")`.
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
#' blocks <- download_census_blocks(template = dem)
#' }
#'
download_census_blocks <- function(template,
                                   year = "2020",
                                   output = tempfile(fileext = ".gpkg")) {
  ## are we online?
  ## check connectivity
  if (!isTRUE(check_connectivity("tigerweb.geo.census.gov"))) {
    return(invisible(NULL))
  }
  ## check template if a spatraster
  check_spat_ras(template)

  ## check years
  year <- rlang::arg_match(year,
                           values = c('2000', '2010', '2020'))


  furl <- "https://tigerweb.geo.census.gov/arcgis/rest/services/Census2020/Tracts_Blocks/MapServer"
  ## check for service errors first
  msg <- catch_arcgislayer_error(furl)
  if(!is.null(msg)) {
    cli::cli_alert_info(msg[[1]])
    return(invisible(NULL))
  } else {
    tracts_blocks <- arcgislayers::arc_open(furl)
  }

  if(year == '2020') {id <- 2}
  if(year == '2010') {id <- 6}
  if(year == '2000') {id <- 10}

  blocks_layer <- arcgislayers::get_layer(tracts_blocks, id = id)

  ## create a bbox object from DEM
  bounds <- sf::st_bbox(template)

  ## retrieve the cropped featuer layer
  blocks_sf <- arcgislayers::arc_select(blocks_layer,
                                        filter_geom = bounds)

  blocks_vect <- terra::vect(blocks_sf)

  terra::writeVector(blocks_vect,
                     filename = output)

  return(vect(output))

}
