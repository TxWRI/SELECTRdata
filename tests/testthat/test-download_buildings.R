test_that("download_urban_buildings returns errors", {
  testthat::expect_error(
    download_buildings(terra::vect(system.file("ex/lux.shp", package="terra")))
  )
})

test_that("download_buildings returns expected objects",{
  testthat::skip_on_cran()
  dem <- system.file("extdata", "thompsoncreek.tif", package = "SELECTRdata")
  dem <- terra::rast(dem)
  buildings <- download_buildings(template = dem)
  testthat::expect_s4_class(buildings, "SpatVector")

  ## check that CRS's are matching
  testthat::expect_equal(terra::crs(buildings, proj = TRUE),
                         terra::crs(dem, proj = TRUE))

  ## check sf works
  buildings <- download_buildings(template = dem, return = "sf")
  testthat::expect_s3_class(buildings, "sf")
  testthat::expect_equal(terra::crs(buildings, proj = TRUE),
                         terra::crs(dem, proj = TRUE))
})
