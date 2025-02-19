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


test_that("nlcd returns same raster as manual downloads", {
  ## this is a pretty heavy test
  ## might want to better condition when it runs
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  set_gdal_config('AWS_NO_SIGN_REQUEST', 'YES')
  dem <- system.file("extdata", "thompsoncreek.tif", package = "SELECTRdata")
  dem <- terra::rast(dem)
  landmass <- "l48"
  year <- "2021"
  dataset <- "land_cover"
  # file download by function
  nlcd <- SELECTRdata::download_nlcd(template = dem)

  # manual file download
  s3_path <- SELECTRdata:::gen_s3_path(landmass, year, dataset)
  files <- gdalraster::vsi_read_dir(s3_path)
  nlcd_file <- paste0(s3_path, "/", files[grep(".img", files)])

  nlcd_ds <- terra::rast(nlcd_file)
  nlcd_ds <- terra::crop(x = nlcd_ds,
                         y = dem)

  testthat::expect_identical(terra::global(nlcd, fun = "sum"),
                             terra::global(nlcd_ds, fun = "sum"))


  set_gdal_config('AWS_NO_SIGN_REQUEST', '')
})
