test_that("download_counties returns errors", {

  testthat::expect_error(
    download_counties(terra::vect(system.file("ex/lux.shp", package="terra")))
  )

})


test_that("download_counties returns expected objects", {
  testthat::skip_on_cran()
  dem <- system.file("extdata", "thompsoncreek.tif", package = "SELECTRdata")
  dem <- terra::rast(dem)

  counties <- download_counties(template = dem)
  testthat::expect_s4_class(counties, "SpatVector")

  ## check that CRS's are matching
  testthat::expect_equal(terra::crs(counties, proj = TRUE),
                         terra::crs(dem, proj = TRUE))

})
