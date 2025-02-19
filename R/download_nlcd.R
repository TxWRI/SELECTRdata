## download_nlcd


#' Download and write annual NLCD raster data to file.
#'
#' Downloads and writes an NLCD SpatRaster to file with extents defined by `template`. This function downloads the annualized NLCD data products. See [https://www.mrlc.gov/data/project/annual-nlcd](https://www.mrlc.gov/data/project/annual-nlcd) for more information.
#'
#' @param template A SpatRaster object defining the spatial extent of the returned NLCD raster.
#' @param year character, expects a value between `1986:2023`.
#' @param dataset Character. Expects `c("LndCov","LndChg","LndCnf","FctImp","ImpDsc","SpcChg")`.
#' @param landmass  Character, one of: `c("CU", "AK", "HI")`.
#' @param output A character file path specifying where the raster file should be stored. Defaults to a temporary file.
#' @param overwrite logical. If `TRUE`, filename is overwritten
#' @param verbose Logical, if `TRUE` informative messages will be printed.
#' @param ... additional arguments for for writing files, see `terra::writeRaster()`
#'
#' @return A SpatRaster object with file written to `output`
#' @importFrom rlang arg_match
#' @importFrom gdalraster vsi_read_dir
#' @importFrom terra crop crs ext project rast
#' @export

download_nlcd       <- function(template,
                                year = "2021",
                                dataset = "LndCov",
                                landmass = "CU",
                                output = tempfile(fileext = ".tiff"),
                                overwrite = FALSE,
                                verbose = FALSE,
                                ...) {
  ## are we online?
  ## check connectivity
  if (!isTRUE(check_connectivity("s3-us-west-2.amazonaws.com"))) {
    return(invisible(NULL))
  }

  ## check config options

  ## We should make a helper function that sets this for the user
  check_terra_gdal_config()
  check_gdalraster_gdal_config()
  #set_config_option("AWS_NO_SIGN_REQUEST", "YES")
  #setGDALconfig(c("AWS_NO_SIGN_REQUEST=YES"))

  ## check template if a spatraster
  check_spat_ras(template)

  ## need to check year, dataset, and landmass are valid values
  ## check landmass

  year <- rlang::arg_match(year,
                           values = as.character(c(1985:2023)))

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

  ## generate s3 path
  s3_path <- gen_s3_path(landmass, year, dataset)

  ## need to check path is valid somehow
  #files <- gdalraster::vsi_read_dir(s3_path)
  #nlcd_file <- paste0(s3_path, "/", files[grep(".img", files)])

  ## grab the extent of the template
  template_crs <- terra::crs(template)
  template <- terra::ext(template)

  ## check the crs of template and nlcd match
  nlcd_ds <- terra::rast(s3_path)
  nlcd_crs <- terra::crs(nlcd_ds)

  if(nlcd_crs != template_crs) {

    if(verbose) {
      cli::cli_alert_info("Projecting {.arg template} to match CRS of NLCD before cropping. CRS: {.emp {nlcd_crs}}")
    }
    template <- terra::project(template,
                               from = template_crs,
                               to = nlcd_ds)
  }

  ## return windowed nlcd
  nlcd_crop <- terra::crop(x = nlcd_ds,
                           y = template,
                           filename = output,
                           overwrite = overwrite,
                           verbose = verbose,
                           ...)

  return(nlcd_crop)


}

gen_s3_path <- function(landmass, year, dataset) {

  ## return error if landmass != CU
  if(landmass != "CU") {
    cli_abort(c(
      "{.var landmass} currently only accepts 'CU' until annaulized NLCD products are available for other regions."
    ),
    call = rlang::caller_env())
  }

  #nlcd_annual_bucket <- "https://s3-us-west-2.amazonaws.com/mrlc"
  collection <- 1
  version <- 0

  base_url <- paste0("/vsis3/mrlc/")
  path_url <- paste0("Annual_NLCD_", dataset, "_", year, "_", landmass, "_C", collection, "V", version, ".tif")

  return(paste0(base_url, path_url))
}


