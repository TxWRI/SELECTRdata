## download_nlcd


#' Downlaod and write NLCD raster data to file.
#'
#' Downloads and writes an NLCD SpatRaster to file with extents defined by `template`.
#'
#' @param template A SpatRaster object defining the spatial extent of the returned NLCD raster.
#' @param year character, expects one of: `c("2001", "2004", "2006", "2008", "2011", "2013", "2016", "2019", "2021")`. Note that not all years are available for all landmass areas. See https://www.mrlc.gov/data-services-page for data and area availability.
#' @param dataset Character, currently only supports `land_cover`. Future support for impervious and tree canopy cover is possible.
#' @param landmass  Character, one of: `c("l48", "ak", "hi", "pr")`.
#' @param output A character file path specifying where the raster file should be stored. Defaults to a temporary file.
#' @param overwrite logical. If `TRUE`, filename is overwritten
#' @param verbose Logical, if `TRUE` informative messages will be printed.
#' @param ... additional arguments for for writing files, see `terra::writeRaster()`
#'
#' @return A SpatRaster object with file written to `output`
#' @export

download_nlcd       <- function(template,
                                year = "2021",
                                dataset = "land_cover",
                                landmass = "l48",
                                output = tempfile(fileext = ".tiff"),
                                overwrite = FALSE,
                                verbose = FALSE,
                                ...) {
  ## are we online?
  ## check connectivity
  if (!isTRUE(check_connectivity("mrlc.gov"))) {
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
                           values = c("2001", "2004", "2006", "2008", "2011", "2013", "2016", "2019", "2021"))

  dataset <- rlang::arg_match(dataset,
                              values = c("land_cover"))

  landmass <- rlang::arg_match(landmass,
                               values = c(
                                 "l48",
                                 "ak",
                                 "hi",
                                 "pr")
  )

  ## generate s3 path
  if(landmass == "l48") {
    if(year == 2021) {
      version <- "20230630"
    } else {version <- "20210604"}
  }
  if(landmass == "ak") {
    if(year %in% c("2016", "2011", "2001")) {
      version <- "20200724"
    } else {
      ## return error that data is not available
      msg <- paste0("NLCD data for ", year, " and ", landmass, " is not available. See https://www.mrlc.gov/data?f%5B0%5D=category%3ALand%20Cover for available datasets.")
      rlang::abort(message = msg)
    }
  }

  base_url <- paste0("/vsizip/vsis3/mrlc/")
  path_url <- paste0("nlcd_", year, "_", dataset, "_", landmass, "_", version, ".zip")


  ## need to check path is valid somehow
  files <- gdalraster::vsi_read_dir(paste0(base_url, path_url))
  nlcd_file <- paste0(base_url, path_url, "/", files[grep(".img", files)])

  ## grab the extent of the template
  template_crs <- terra::crs(template)
  template <- terra::ext(template)

  ## check the crs of template and nlcd match
  nlcd_ds <- terra::rast(nlcd_file)
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



