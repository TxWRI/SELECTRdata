#' Download U.S. Census block boundaries
#'
#' Downloads U.S. Census block boundaries and associated information for
#' blocks within the spatial extent of the template SpatRaster.
#'
#' @param template A SpatRaster object.
#' @param year A character value. Any of the following values should work: `c("2000","2010","2020")`.
#' @param page_size a numeric value passed to `arcgislayers::arcselect()`. Defaults to NULL. Useful when the requests returns a 500 error code.
#' @param output A character file path specifying where the raster file should be stored. Defaults to a temporary file.
#'
#' @return A terra SpatVector object. If API resources are not available an invisible `NULL` is returned.
#' @importFrom arcgislayers arc_open arc_select get_layer
#' @importFrom cli cli_alert_info
#' @importFrom rlang arg_match
#' @importFrom sf st_bbox
#' @importFrom terra project vect writeVector
#' @export
#' @examples
#' # example code
#' \donttest{
#' ## This example requires an internet connection to run
#' dem <- system.file("extdata", "thompsoncreek.tif", package = "SELECTRdata")
#' dem <- terra::rast(dem)
#'
#' blocks <- download_census_blocks(template = dem)
#' blocks
#' }
#'
download_census_blocks <- function(template,
                                   year = "2020",
                                   page_size = NULL,
                                   output = tempfile(fileext = ".gpkg")) {
  ## are we online?
  ## check connectivity
  if (!isTRUE(check_connectivity("services.arcgis.com"))) {
    return(invisible(NULL))
  }
  ## check template if a spatraster
  check_spat_ras(template)

  ## check years
  year <- rlang::arg_match(year,
                           values = c('2000', '2010', '2020'))

  if(year == "2020") {
    furl <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services/USA_Census_2020_DHC_Blocks/FeatureServer"
    id <- 1
  }
  if(year == "2010") {
    message("2010 Census endpoint not yet implemented")
    return(invisible(NULL))
  }
  if(year == "2000") {
    message("2000 Census endpoint not yet implemented")
    return(invisible(NULL))
  }


  ## check for service errors first
  msg <- catch_arcgislayer_error(furl)
  if(!is.null(msg)) {
    cli::cli_alert_info(msg[[1]])
    return(invisible(NULL))
  } else {
    tracts_blocks <- arcgislayers::arc_open(furl)
  }


  ## this needs to be wrapped a try
  blocks_layer <- arcgislayers::get_layer(tracts_blocks, id = id)

  ## create a bbox object from DEM
  bounds <- sf::st_bbox(template)

  ## retrieve the cropped featuer layer
  blocks_sf <- arcgislayers::arc_select(x = blocks_layer,
                                        filter_geom = bounds,
                                        page_size = page_size)

  blocks_vect <- terra::vect(blocks_sf)

  blocks_vect <- terra::project(blocks_vect, template)

  terra::writeVector(blocks_vect,
                     filename = output)

  return(terra::vect(output))

}
