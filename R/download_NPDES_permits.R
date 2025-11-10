#' Download NPDES permits
#'
#' Downloads NPDES permits from EPA ECHO API services within the bounds of the
#' SpatRast template.
#'
#' @param template A SpatRaster or SpatVector object. The extent of the returned object will match `template`.
#' @param permit_component A character vector with one or more of the following: `PRE` (pretreatment), `CAF` (CAFO), `CSO` (CSO), `POT` (Publicly Owned Treatment Works, the default), `BIO` (Biosolids), `SWS` (Stormwater Small MS4), `SWM` (Stormwater Medium/Large MS4), `SWI` (Stormwater Industrial), `SWC` (Stormwater Construction).
#' @param permit_status A character vector with one or more of the following: `EFF` (effective, the default), `EXP` (expired), `PND` (pending), `TRM` (terminated), `RET` (retired), `NON` (not needed), `ADC` (administratively continued).
#' @param output A character file path specifying where the raster file should be stored. Defaults to a temporary file.
#'
#' @return SpatVector object
#' @export
#'
#' @examples
#' dem <- system.file("extdata", "thompsoncreek.tif", package = "SELECTRdata")
#' dem <- terra::rast(dem)
#' download_NPDES_permits(dem)
#'
download_NPDES_permits <- function(template,
                                   permit_component = "POT",
                                   permit_status = "EFF",
                                   output = tempfile(fileext = ".gpkg")) {

  ## check that DEM is SpatRaster or is SpatVector
  check_terra_spat_obj(template)

  ## get bbox of the template. need lat, lons in decimal degrees
  ## create a bbox object from DEM
  ## note, we transform first bbox to sfc
  ## because the gdal method for transforming a bbox is not available
  ## on some gdal builds resulting in build errors (I believe on GDAL 3.10.1)
  bounds <- sf::st_bbox(template)
  bounds <- sf::st_as_sfc(bounds)
  bounds <- sf::st_transform(bounds, 4326)
  bounds <- sf::st_bbox(bounds)

  p_c1lat <- bounds[["ymin"]]
  p_c1lon <- bounds[["xmin"]]
  p_c2lat <- bounds[["ymax"]]
  p_c2lon <- bounds[["xmax"]]

  permit_component <- rlang::arg_match(permit_component,
                                       values = c("PRE",
                                                  "CAF",
                                                  "CSO",
                                                  "POT",
                                                  "BIO",
                                                  "SWS",
                                                  "SWM",
                                                  "SWI",
                                                  "SWC"),
                                       multiple = TRUE)

  permit_status <- rlang::arg_match(permit_status,
                                    values = c("EFF",
                                               "EXP",
                                               "PND",
                                               "TRM",
                                               "RET",
                                               "NON",
                                               "ADC"),
                                    multiple = TRUE)


  ## make a request and obtain the QID for the search
  req <- requestECHO(resource = "cwa_rest_services.get_facilities",
                     p_c1lat = p_c1lat,
                     p_c1lon = p_c1lon,
                     p_c2lat = p_c2lat,
                     p_c2lon = p_c2lon,
                     p_pcomp = permit_component,
                     p_pstat = permit_status
                     )

  body <- httr2::req_perform(req)
  body <- httr2::resp_body_json(body)

  rows <- as.integer(body$Results$QueryRows)
  QID <- body$Results$QueryID

  ## if $Results$QueryRows == 0 should reurn a message and invisible NULL
  if(rows == 0) {
    cli::cli_inform(c("i" = paste0("Query returned ", rows, " results!")))
    return(invisible(NULL))
  } else {
    cli::cli_inform(c("i" = paste0("Query returned ", rows, " results!")))
  }

  ## use qid returned by first request to download a geojson
  req <- requestECHO(resource = "cwa_rest_services.get_geojson",
                     qid = QID)
  body <- httr2::req_perform(req)
  body <- httr2::resp_body_string(body, encoding = "UTF-8")

  points <- terra::vect(body)
  points <- terra::project(points, template)

  terra::writeVector(points,
                     filename = output)

  return(terra::vect(output))
}
