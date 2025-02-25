#' Thompsons Creek Elevation Data
#'
#' Elevation raster of the Thompsons Creek watershed outside of
#' College Station, Texas. This data is obtained from the hydroreinforced
#' digital elevation model (DEM) rasters that are part of the United States
#' Geological Survey (USGS) and United States Environmental Protection Agency
#' (EPA) National Hydrography Dataset Plus (NHDPlus).
#' @name thompson
#' @docType data
#' @seealso \url{https://nhdplus.com/NHDPlus/NHDPlusV2_home.php}
#' @examples
#' \donttest{
#' thompson <- system.file("extdata", "thompsoncreek.tif", package = "SELECTRdata")
#' terra::rast(thompson)
#' }
NULL

#' Thompsons Creek Watershed Boundary
#'
#' Polygon boundary of the Thompsons Creek watershed (Collecge Station, Texas).
#' This watershed boundary was created using the hydroreinformced raster from
#' the United States Geological Survey (USGS) and United States Environmental
#' Protection Agency (EPA) National Hydrography Dataset Plus (NHDPlus) dataset.
#' @name wbd
#' @docType data
#' @seealso [thompson()]
#' @examples
#' \donttest{
#' gpkg <- system.file("extdata", "thompsoncreek.gpkg", package = "SELECTRdata")
#' terra::vect(gpkg, layer = "wbd")
#' }
NULL
