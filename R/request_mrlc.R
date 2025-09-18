# Returns a httr2 request

request_mrlc <- function(dataset = "LndCov", ## chr one of LndCov, etc.
                          year = "2024", ## string 2024, etc
                          extent = terra::ext(), ## should be class SpatExtent
                          template_srs = NULL, ## character string OGC WKT for a spatial reference system
                          ...) {

  ## notes, I'd prefer to use WCS 2.0.1 which the geoserver seems to expose,
  ## but I'm having issues creating successful queries in that format
  ## so this defaults to WCS 1.0.0. The downside is the files don't include raster attributes.


  ## choose resources based on dataset
  resource <- switch(
    EXPR = dataset,
    LndCov = "https://dmsdata.cr.usgs.gov/geoserver/mrlc_Land-Cover-Native_conus_year_data/wcs",
    LndChg = "https://dmsdata.cr.usgs.gov/geoserver/mrlc_Land-Cover-Change-Native_conus_year_data/wcs",
    LndCnf = "https://dmsdata.cr.usgs.gov/geoserver/mrlc_Land-Cover-Confidence-Native_conus_year_data/wcs",
    FctImp = "https://dmsdata.cr.usgs.gov/geoserver/mrlc_Factional-Impervious-Surface-Native_conus_year_data/wcs",
    ImpDsc = "https://dmsdata.cr.usgs.gov/geoserver/mrlc_Impervious-Descriptor-Native_conus_year_data/wcs",
    SpcChg = "https://dmsdata.cr.usgs.gov/geoserver/mrlc_Spectral-Change-Day-of-Year-Native_conus_year_data/wcs"
  )

  ## returns the coverage id in xml
  coverage <- request_mrlc_cov_id(dataset, resource)

  ## how do we select the right time based on input string year?
  time <- strptime(paste0(year, "-01-01"),
                   format = "%Y-%m-%d") |>
    format("%Y-%m-%dT%TZ")

  ## returns the NLCD native SRS in string format EPSG:XXXX
  nlcd_epsg <-request_mrlc_crs(resource,
                               coverage)
  nlcd_epsg <- grep("[0-9]",
                    nlcd_epsg,
                    value = TRUE)
  nlcd_epsg <- regmatches(nlcd_epsg,
                          gregexec("[0-9]",
                                   nlcd_epsg))[[1]] |>
    paste0(collapse = "") |>
    as.integer()


  ## evaluate if the template srs and the nlcd srs are the same
  if(!gdalraster::srs_is_same(template_srs,
                              gdalraster::epsg_to_wkt(nlcd_epsg))) {
    ## project extent to the nlcd_epsg
    cli::cli_alert("CRS of the template does not match the NLCD. Projecting the extent of the template to {.code {nlcd_epsg}}.",
                   wrap = TRUE)
    extent <- project(extent,
                      from = template_srs,
                      to = nlcd_epsg)

  }


  ## check the extent is class SpatExtent
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

req <- httr2::request(resource) |>
  httr2::req_url_query(!!!query_list)

return(req)

}

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

  req <- httr2::request(resource) |>
    httr2::req_url_query(!!!query_list) |>
    httr2::req_perform() |>
    httr2::resp_body_xml() |>
    xml2::as_list()

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

  req <- httr2::request(resource) |>
    httr2::req_url_query(!!!query_list) |>
    httr2::req_perform() |>
    httr2::resp_body_xml(check_type = FALSE) |>
    xml2::as_list()

  nlcd_crs <- req$CoverageDescription$CoverageOffering$supportedCRSs$requestResponseCRSs[[1]]

  return(nlcd_crs)


}

