## download_nlcd


#' Download and write annual NLCD raster data to file.
#'
#' Downloads and writes an NLCD SpatRaster to file with extents defined by `template`. This function downloads the annualized NLCD data products. See [https://www.mrlc.gov/data/project/annual-nlcd](https://www.mrlc.gov/data/project/annual-nlcd) for more information.
#'
#' @param template A SpatRaster object defining the spatial extent of the returned NLCD raster.
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

  ## check template if a spatraster
  check_spat_ras(template)

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
    cli_abort(c(
      "{.var landmass} currently only accepts 'CU' until annaulized NLCD products are available for other regions."
    ),
    call = rlang::caller_env())
  }

  ## grab the extent of the template
  template_crs <- terra::crs(template)
  template_ext <- terra::ext(template)


  ## returns the httr2 request
  ## TODO: wrap in try
  x <- request_mrlc(dataset = dataset,
                    year = year,
                    extent = template_ext,
                    template_srs = template_crs)

  download_path <- tempfile(fileext = ".tif")
  x_resp <- x |>
    httr2::req_perform(path = download_path)

  ## TODO: check the output type

  ## make into a terra raster
  nlcd <- terra::rast(download_path)

  nlcd <- terra::as.factor(nlcd)
  level_data <- data.frame(ID = c(11L, 12L, 21L, 22L, 23L, 24L, 31L, 41L, 42L, 43L, 52L, 71L, 81L, 82L, 90L, 95L),
                           Label = c("Open Water", "Perennial Ice/Snow", "Developed, Open Space", "Developed, Low Intensity", "Developed, Medium Intensity", "Developed, High Intensity", "Barren Land (Rock/Sand/Clay", "Deciduous Forest", "Evergreen Forest", "Mixed Forest", "Shrub/Scrub", "Grassland/Herbaceous", "Pasture/Hay", "Cultivated Crops", "Woody Wetlands", "Emergent Herbaceous Wetlands"))
  levels(nlcd) <- level_data

  outpath <- terra::writeRaster(nlcd, output, ...)

  outpath

}

# gen_s3_path <- function(landmass, year, dataset) {
#
#   ## return error if landmass != CU
#   if(landmass != "CU") {
#     cli_abort(c(
#       "{.var landmass} currently only accepts 'CU' until annaulized NLCD products are available for other regions."
#     ),
#     call = rlang::caller_env())
#   }
#
#   #nlcd_annual_bucket <- "https://s3-us-west-2.amazonaws.com/mrlc"
#   collection <- 1
#   version <- 0
#
#   base_url <- paste0("/vsis3/mrlc/")
#   path_url <- paste0("Annual_NLCD_", dataset, "_", year, "_", landmass, "_C", collection, "V", version, ".tif")
#
#   return(paste0(base_url, path_url))
# }


