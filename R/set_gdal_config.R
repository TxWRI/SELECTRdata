## set GDAL config options that allow downloading NLCD data from MRLC's AWS S3 bucket

#' Set GDAL configuration options
#'
#' A convience functions for simultaneously setting the GDAL runtime
#' configuration options in both terra and gdalraster.
#' @param option Character name of a configuration option.
#' @param value Character value to set for the option. `value = ""` (empty string) will unset previously set values.
#'
#' @return No return value, called for side effects.
#' @export
#' @seealso [terra::setGDALconfig()] and [gdalraster::set_config_option()]
#' @examples
#' set_gdal_config("GDAL_CACHEMAX", "10%")
#' ## unset
#' set_gdal_config("GDAL_CACHEMAX", "")
set_gdal_config <- function(option, value) {

  ## sets gdal configuration options in gdalraster
  gdalraster::set_config_option(key = option,
                                value = value)

  ## sets gdal configuration options in terra
  terra::setGDALconfig(option = option,
                       value = value)

  return(invisible(NULL))

}
