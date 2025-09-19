# test_that("set_gdal_config works", {
#   param <- "GDAL_CACHEMAX"
#   value <- "10%"
#   set_gdal_config(param, value)
#
#   gdalraster_val <- gdalraster::get_config_option(param)
#   terra_val <- terra::getGDALconfig(param)
#
#   testthat::expect_type(gdalraster_val, "character")
#   testthat::expect_type(terra_val, "character")
#   testthat::expect_match(gdalraster::get_config_option(param),
#                              value)
#   testthat::expect_match(terra::getGDALconfig(param),
#                              value)
#   ## reset
#   set_gdal_config(param, "")
#   testthat::expect_no_match(gdalraster::get_config_option(param),
#                          value)
#   testthat::expect_no_match(terra::getGDALconfig(param),
#                          value)
# })
#
#
# test_that("gdal checks work", {
#   testthat::expect_error(check_terra_gdal_config())
#   testthat::expect_error(check_gdalraster_gdal_config())
# })
