
## return coverage identifier string
request_mrlc_cov_id <- function(dataset,
                                resource,
                                ...) {

  query_list <- list(
    ...,
    SERVICE = "WCS",
    VERSION = "1.0.0",
    REQUEST = "GetCapabilities"
  )

  ## check status code and return message if status != 200

  req <- httr2::request(resource)
  req <- httr2::req_url_query(req, !!!query_list)
  req <- httr2::req_error(req, is_error = ~ FALSE)
  req <- httr2::req_perform(req)

  if(httr2::resp_is_error(req)) {
    cli::cli_alert_danger(httr2::resp_status_desc(req))
    return(invisible(NULL))
  }


  req <- httr2::resp_body_xml(req)
  req <- xml2::as_list(req)

  return(req$WCS_Capabilities$ContentMetadata$CoverageOfferingBrief$name[[1]])

}


## returns nlcd CRS native output format as string in EPSG:XXXX format
request_mrlc_crs <- function(resource,
                             coverage,
                             ...) {

  query_list <- list(
    ...,
    SERVICE = "WCS",
    VERSION = "1.0.0",
    REQUEST = "DescribeCoverage",
    COVERAGE = coverage
  )
  ## check status code and return message if status != 200
  req <- httr2::request(resource)
  req <- httr2::req_url_query(req, !!!query_list)
  req <- httr2::req_perform(req)
  req <- httr2::resp_body_xml(req, check_type = FALSE)
  req <- xml2::as_list(req)

  nlcd_crs <- req$CoverageDescription$CoverageOffering$supportedCRSs$requestResponseCRSs[[1]]

  return(nlcd_crs)


}



request_mrlc_download <- function(resource,
                                  extent,
                                  coverage,
                                  time,
                                  nlcd_epsg,
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


  query_list <- list(
    ...,
    SERVICE = "WCS",
    VERSION = "1.0.0",
    COVERAGE = coverage,
    BBOX = bbox,
    TIME = time,
    RESX = 30,
    RESY = 30,
    CRS = paste0("EPSG:",nlcd_epsg),
    FORMAT = "image/geotiff",
    REQUEST = "GetCoverage")

  req <- httr2::request(resource)
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
  nlcd <- terra::rast(download_path)

  nlcd <- terra::as.factor(nlcd)
  level_data <- data.frame(ID = c(11L, 12L, 21L, 22L, 23L, 24L, 31L, 41L, 42L, 43L, 52L, 71L, 81L, 82L, 90L, 95L),
                           Label = c("Open Water", "Perennial Ice/Snow", "Developed, Open Space", "Developed, Low Intensity", "Developed, Medium Intensity", "Developed, High Intensity", "Barren Land (Rock/Sand/Clay", "Deciduous Forest", "Evergreen Forest", "Mixed Forest", "Shrub/Scrub", "Grassland/Herbaceous", "Pasture/Hay", "Cultivated Crops", "Woody Wetlands", "Emergent Herbaceous Wetlands"))
  levels(nlcd) <- level_data

  nlcd

}
