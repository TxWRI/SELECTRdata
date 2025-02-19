test_that("nlcd paths are correct", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  set_gdal_config('AWS_NO_SIGN_REQUEST', 'YES')
  landmasses <- c("CU")
  years <- as.character(c(1986:2023))
  datasets <- c("LndCov","LndChg","LndCnf","FctImp","ImpDsc","SpcChg")

  ## these should return valid URLs
  for(i in 1:length(years)) {
    year <- years[[i]]
    for(i in 1:length(landmasses)) {
      landmass <- landmasses[[i]]
      for(i in 1:length(datasets)) {
        dataset <- datasets[[i]]
        s3_path <- SELECTRdata:::gen_s3_path(landmass, year, dataset)
        file <- gdalraster::vsi_stat(s3_path, info = "exists")
        testthat::expect_true(file)
      }
    }
  }

  ## these should error out due to HI and AK not having annualized product available
  landmasses <- c("HI", "AK")
  for(i in 1:length(years)) {
    year <- years[[i]]
    for(i in 1:length(landmasses)) {
      landmass <- landmasses[[i]]
      for(i in 1:length(datasets)) {
        dataset <- datasets[[i]]
        testthat::expect_error(SELECTRdata:::gen_s3_path(landmass, year, dataset))
      }
    }
  }
  set_gdal_config('AWS_NO_SIGN_REQUEST', '')
})


#
# test_that("nlcd returns same raster as manual downloads", {
#   ## this is a pretty heavy test
#   ## might want to better condition when it runs
#   testthat::skip_on_cran()
#   testthat::skip_if_offline()
#   set_gdal_config('AWS_NO_SIGN_REQUEST', 'YES')
#   dem <- system.file("extdata", "thompsoncreek.tif", package = "SELECTRdata")
#   dem <- terra::rast(dem)
#   landmass <- "l48"
#   year <- "2021"
#   dataset <- "land_cover"
#   # file download by function
#   nlcd <- SELECTRdata::download_nlcd(template = dem)
#
#   # manual file download
#   s3_path <- SELECTRdata:::gen_s3_path(landmass, year, dataset)
#   files <- gdalraster::vsi_read_dir(s3_path)
#   nlcd_file <- paste0(s3_path, "/", files[grep(".img", files)])
#
#   nlcd_ds <- terra::rast(nlcd_file)
#   nlcd_ds <- terra::crop(x = nlcd_ds,
#                          y = dem)
#
#   testthat::expect_identical(terra::global(nlcd, fun = "sum"),
#                              terra::global(nlcd_ds, fun = "sum"))
#
#
#   set_gdal_config('AWS_NO_SIGN_REQUEST', '')
# })
