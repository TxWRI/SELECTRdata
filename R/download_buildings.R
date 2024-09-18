#' Download building footprints
#'
#' Downloads building footprints and attribute data from FEMA's
#' USA Structures dataset (https://fema.maps.arcgis.com/home/item.html?id=0ec8512ad21e4bb987d7e848d14e7e24#overview).
#'
#' @param template A SpatRaster object. The extent of the returned object will match `template`.
#' @param return A character object, either `SpatVector` or `sf`. Defaults to `SpatVector`.
#'
#' @return A `SpatVector` or `sf` object with extents matching the `SpatRaster` object provided in the `template` argument.
#' @export
#' @importFrom arcgislayers arc_open arc_select get_layer
#' @importFrom sf st_bbox
#' @importFrom terra vect
download_buildings <- function(template,
                               return = "SpatVector") {

  ## are we online?
  ## check connectivity
  if (!isTRUE(check_connectivity("services2.arcgis.com"))) {
    return(invisible(NULL))
  }

  ## check that DEM is SpatRaster
  check_spat_ras(template)

  ## check that return is one of "SpatVector" or "sf"
  check_string(return)
  check_string_contains(return, c("SpatVector", "sf"))

  ## feature server url
  furl <- "https://services2.arcgis.com/FiaPA4ga0iQKduv3/arcgis/rest/services/USA_Structures_View/FeatureServer"
  buildings <- arcgislayers::arc_open(furl)

  ## feature layer object
  buildings_layer <- arcgislayers::get_layer(buildings, id = 0)

  ## create a bbox object from DEM
  bounds <- sf::st_bbox(template)

  ## retrieve the cropped featuer layer
  buildings_sf <- arcgislayers::arc_select(buildings_layer,
                                           filter_geom = bounds)

  if(return == "SpatVector") {
    output <- terra::vect(buildings_sf)
  } else {
    output <- buildings_sf
  }

  return(output)
}
