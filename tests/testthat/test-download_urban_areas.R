test_that("download_urban_areas returns errors", {
  testthat::expect_error(
    download_urban_areas(terra::vect(system.file("ex/lux.shp", package="terra")))
  )
})


test_that("download_urban_areas returns expected objects",{
  testthat::skip_on_cran()
  dem <- system.file("extdata", "thompsoncreek.tif", package = "SELECTRdata")
  dem <- terra::rast(dem)
  ua <- download_urban_areas(template = dem)
  testthat::expect_s4_class(ua, "SpatVector")

  ## check that CRS's are matching
  testthat::expect_equal(terra::crs(ua, proj = TRUE),
                         terra::crs(dem, proj = TRUE))
})


### to do:
### check that the output intersects the template
