## download_nlcd


#' Download and write annual NLCD raster data to file.
#'
#' Downloads and writes an NLCD SpatRaster to file with extents defined by `template`. This function downloads the annualized NLCD data products. See [https://www.mrlc.gov/data/project/annual-nlcd](https://www.mrlc.gov/data/project/annual-nlcd) for more information.
#'
#' @param template A SpatRaster or SpatVector object. The extent of the returned object will match `template`.
#' @param year character, expects a value between `1986:2024`.
#' @param dataset Character. Expects `c("LndCov","LndChg","LndCnf","FctImp","ImpDsc","SpcChg")`. Only `"LndCov"` is supported at this time.
#' @param landmass  Depreciated. Character, one of: `c("CU", "AK", "HI")`.
#' @param output A character file path specifying where the raster file should be stored. Defaults to a temporary file.
#' @param overwrite logical. If `TRUE`, filename is overwritten
#' @param verbose Logical, if `TRUE` informative messages will be printed.
#' @param ... additional arguments for for writing files, see `terra::writeRaster()`
#'
#' @return A SpatRaster object with file written to `output`
#' @export
#' @examples
#' \donttest{
#' ## This example requires an internet connection to run
#' dem <- system.file("extdata", "thompsoncreek.tif", package = "SELECTRdata")
#' dem <- terra::rast(dem)
#' download_nlcd(template = dem, year = "2024")
#' }
#'

download_nlcd       <- function(template,
                                year = "2021",
                                dataset = "LndCov",
                                landmass = "CU", ## we should depreciate this since only CU annualized products are available.
                                output = tempfile(fileext = ".tiff"),
                                overwrite = FALSE,
                                verbose = FALSE,
                                ...) {
  ## are we online?
  ## check connectivity
  if (!isTRUE(check_connectivity("dmsdata.cr.usgs.gov"))) {
    return(invisible(NULL))
  }

  ## check that DEM is SpatRaster or is SpatVector
  check_terra_spat_obj(template)

  ## need to check year, dataset, and landmass are valid values
  ## check landmass

  year <- rlang::arg_match(year,
                           values = as.character(c(1986:2024)))

  dataset <- rlang::arg_match(dataset,
                              values = c(
                                "LndCov",
                                "LndChg",
                                "LndCnf",
                                "FctImp",
                                "ImpDsc",
                                "SpcChg"
                              ))

  landmass <- rlang::arg_match(landmass,
                               values = c(
                                 "CU",
                                 "AK",
                                 "HI")
  )

  ## return error if landmass != CU
  if(landmass != "CU") {
    cli::cli_abort(c(
      "{.var landmass} currently only accepts 'CU' until annaulized NLCD products are available for other regions."
    ),
    call = rlang::caller_env())
  }

  ## grab the extent of the template
  template_srs <- terra::crs(template)
  template_ext <- terra::ext(template)

  ##  each request should be called in this function so we can return null if any single one fails...
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
  if(is.null(coverage)) {
    return(invisible(NULL))
    }


  ## how do we select the right time based on input string year?
  time <- strptime(paste0(year, "-01-01"),
                   format = "%Y-%m-%d")
  time <- format(time, "%Y-%m-%dT%TZ")

  ## returns the NLCD native SRS in string format EPSG:XXXX
  nlcd_epsg <-request_mrlc_crs(resource,
                               coverage)
  nlcd_epsg <- grep("[0-9]",
                    nlcd_epsg,
                    value = TRUE)
  nlcd_epsg <- regmatches(nlcd_epsg,
                          gregexec("[0-9]",
                                   nlcd_epsg))[[1]]
  nlcd_epsg <- paste0(nlcd_epsg, collapse = "")
  nlcd_epsg <- as.integer(nlcd_epsg)

  ## evaluate if the template srs and the nlcd srs are the same
  if(!gdalraster::srs_is_same(template_srs,
                              gdalraster::epsg_to_wkt(nlcd_epsg))) {
    ## project extent to the nlcd_epsg
    cli::cli_alert("CRS of the template does not match the NLCD. Projecting the extent of the template to {.code {nlcd_epsg}}.",
                   wrap = TRUE)
    template_ext <- terra::project(template_ext,
                                   from = template_srs,
                                   to = paste0("epsg:",nlcd_epsg))

  }

  ## return terra object or invisible no
  x <- request_mrlc_download(resource = resource,
                             extent = template_ext,
                             coverage = coverage,
                             time = time,
                             nlcd_epsg = nlcd_epsg)

  x <- terra::writeRaster(x, output, ...)

  x

}
