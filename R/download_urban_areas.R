#' Download U.S. Urban Areas
#'
#' Downloads a U.S. Census designated urban areas intersecting the spatial extent of the template SpatRaster.
#'
#' @param template A SpatRaster object.
#' @param page_size a numeric value passed to `arcgislayers::arcselect()`. Defaults to NULL. Useful when the requests returns a 500 error code.
#' @param output A character file path specifying where the raster file should be stored. Defaults to a temporary file.
#'
#' @return A terra SpatVector object. If API resources are not available an invisible `NULL` is returned.
#' @importFrom arcgislayers arc_open arc_select get_layer
#' @importFrom cli cli_alert_info
#' @importFrom sf st_bbox
#' @importFrom terra vect writeVector
#' @export
#'
#' @examples
#' # example code
#' \donttest{
#' ## This example requires an internet connection to run
#' dem <- system.file("extdata", "thompsoncreek.tif", package = "SELECTRdata")
#' dem <- terra::rast(dem)
#'
#' ua <- download_urban_areas(template = dem)
#' ua
#' }
#'
download_urban_areas <- function(template,
                                 page_size = NULL,
                                 output = tempfile(fileext = ".gpkg")) {

  ## are we online?
  ## check connectivity
  if (!isTRUE(check_connectivity("services.arcgis.com"))) {
    return(invisible(NULL))
  }
  ## check template if a spatraster
  check_spat_ras(template)

  furl <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services/USA_Census_Urban_Areas/FeatureServer"

  ## check for service errors first
  msg <- catch_arcgislayer_error(furl)
  if(!is.null(msg)) {
    cli::cli_alert_info(msg[[1]])
    return(invisible(NULL))
  } else {
    feature_server <- arcgislayers::arc_open(furl)
  }
  id <- 0

  ## this needs to be wrapped a try
  ## we also use this mutliple times, could be wrapped up in a function (download_census, download_county etc.)
  layer <- arcgislayers::get_layer(feature_server, id = id)

  ## create a bbox object from DEM
  bounds <- sf::st_bbox(template)

  ## retrieve the cropped featuer layer
  queried_layer <- arcgislayers::arc_select(x = layer,
                                        filter_geom = bounds,
                                        page_size = page_size)

  queried_layer_vect <- terra::vect(queried_layer)

  terra::writeVector(queried_layer_vect,
                     filename = output)

  return(terra::vect(output))
}
