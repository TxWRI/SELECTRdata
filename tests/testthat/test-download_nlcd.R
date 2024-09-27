test_that("l48 nlcd paths are correct", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  set_gdal_config('AWS_NO_SIGN_REQUEST', 'YES')
  landmass <- "l48"
  years <- c("2001", "2004", "2006", "2008", "2011", "2013", "2016", "2019", "2021")
  dataset <- "land_cover"
  for(i in 1:length(years)) {
    s3_path <- SELECTRdata:::gen_s3_path(landmass, years[[i]], dataset)
    files <- gdalraster::vsi_read_dir(s3_path)
    testthat::expect_type(files, "character")
    testthat::expect_true(if(length(files) > 1) {TRUE})

  }
  set_gdal_config('AWS_NO_SIGN_REQUEST', '')
})


test_that("ak nlcd paths are correct", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  set_gdal_config('AWS_NO_SIGN_REQUEST', 'YES')
  landmass <- "ak"
  years <- c("2016", "2011", "2001")
  dataset <- "land_cover"
  for(i in 1:length(years)) {
    s3_path <- SELECTRdata:::gen_s3_path(landmass, years[[i]], dataset)
    files <- gdalraster::vsi_read_dir(s3_path)
    testthat::expect_type(files, "character")
    testthat::expect_true(if(length(files) > 1) {TRUE})

  }
  set_gdal_config('AWS_NO_SIGN_REQUEST', '')
})
