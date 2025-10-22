#' Download digital elevation model
#'
#' Downloads a 1/3 arc-second high resolution seamless USGS DEM raster. Standard DEMs represent the topographic surface of the earth and contain flattened water surfaces.
#'
#' @param x Either a SpatVector, SpatRaster, or SpatExtent. Or object that a SpatExtent can be retrieved from.
#' @param srs character in `<auth>:<code>` format of the spatial reference system used in `x` if it is a SpatExtent object. Defaults NULL, can be left NULL if x is a SpatVector or SpatRaster.
#' @param output A character file path specifying where the raster file should be stored. Defaults to a temporary file.
#'
#' @return A SpatRaster object.
#' @export
#'
#' @seealso [terra::SpatExtent()]
#' @examples
#' library(terra)
#' location_of_interest <- system.file("extdata", "thompsoncreek.tif", package = "SELECTRdata")
#' location_of_interest <- terra::rast(location_of_interest)
#' extent <- ext(location_of_interest)
#' extent <- vect(extent, crs = crs(location_of_interest))
#' extent <- project(extent, "EPSG:6579")
#' auth <- crs(extent, describe = TRUE)
#' auth <- paste0(auth$authority, ":", auth$code)
#' extent <- ext(extent)
#' example_dem <- download_dem(x = extent, srs = auth)
#' plot(example_dem)

download_dem <- function(x,
                         srs = NULL,
                         output = tempfile(fileext = ".tiff")) {

  ## see https://github.com/ropensci/terrainr and https://apps.nationalmap.gov/services/

  ## are we online?
  ## check connectivity
  if (!isTRUE(check_connectivity("elevation.nationalmap.gov"))) {
    return(invisible(NULL))
    }

  ## if extent, we need the crs
  ## if SpatVector or SpatRaster we can use crs(x, describe = TRUE)
  if (inherits(x, c("SpatExtent"))) {
    if (is.null(srs)) {
      cli::cli_abort("A valid srs must be supplied to {.arg srs} when {.arg x} is a {.cls SpatExtent} object. Use the <auth>:<code> notation, for example 'epsg:3857'.")
      }
    }
  if (inherits(x, c("SpatRaster", "SpatVector"))) {
    if (is.null(srs)) {
      srs <- crs(x, describe = TRUE)
      srs <- paste0(srs$authority, ":", srs$code)
      ## convert to extent
      x <- terra::ext(x)
      } else {
      ## check that srs is in the correct format <auth>:<code>
      if(!grepl("^epsg:\\d+$", srs, ignore.case = TRUE)) {
        cli::cli_abort("A valid srs must be supplied to {.arg srs} when {.arg x} is a {.cls SpatExtent} object. Use the <auth>:<code> notation, for example 'epsg:3857' or NULL when {.arg x} is a {.cls SpatRaster} or {.cls SpatVector} object.")
        }
      }
  }

  ## call api
  x <- request_dem_download(extent = x,
                            crs = srs,
                            download_path = output)
  return(x)


  }



request_dem_download <- function(resource = "https://elevation.nationalmap.gov/arcgis/rest/services/3DEPElevation/ImageServer/exportImage",
                                 extent,
                                 crs, #input crs in <auth>:<code> format
                                 download_path,
                                 ...) {
  check_is_extent(extent)

  ## return bbox as string: BBOX=xmin,ymin,xmax,ymax from SpatExtent
  bbox = paste0(as.vector(extent)["xmin"],
                ",",
                as.vector(extent)["ymin"],
                ",",
                as.vector(extent)["xmax"],
                ",",
                as.vector(extent)["ymax"])

  bboxSR <- sub("^EPSG:", "", crs, ignore.case = TRUE)


  req <- httr2::request(resource)

  query_list <- list(
    bbox      = bbox,
    format    = "tiff",
    f         = "image",               # response format
    bboxSR = bboxSR, ## use crs arg
    imageSR = bboxSR, ## not sure if we have to return 3857 or can return the same as the input crs
    pixelType = "F32",
    noDataInterpretation = "esriNoDataMatchAny",
    interpolation = "+RSP_BilinearInterpolation"
  )


  req <- httr2::req_url_query(req, !!!query_list)

  download_path <- tempfile(fileext = ".tif")


  x_resp <- httr2::req_perform(req, path = download_path)

  ## to do, check http status and return msg and invisible null as needed

  ## check the output type, if xml then need to return message and invisible null
  ## else move on to loading raster as terra rast object

  if(httr2::resp_content_type(x_resp) != "image/tiff") {
    msg <- httr2::resp_body_xml(x_resp, check_type = FALSE)
    msg <- xml2::as_list(msg)
    cli::cli_alert(msg)
    return(invisible(NULL))
  }

  ## make into a terra raster
  dem <- terra::rast(download_path)
  return(dem)

}
