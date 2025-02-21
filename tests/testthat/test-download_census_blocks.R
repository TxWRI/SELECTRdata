test_that("download_census_blocks returns errors", {
  testthat::expect_error(
    download_census_blocks(terra::vect(system.file("ex/lux.shp", package="terra")))
  )

  dem <- system.file("extdata", "thompsoncreek.tif", package = "SELECTRdata")
  dem <- terra::rast(dem)
  ## needs to be character
  testthat::expect_error(
    download_census_blocks(dem,
                           year = 2020)
  )
  ## needs to be 2020
  testthat::expect_invisible(
    download_census_blocks(dem,
                           year = "2010")
  )
})


test_that("download_census_blocks returns expected objects",{
  testthat::skip_on_cran()
  dem <- system.file("extdata", "thompsoncreek.tif", package = "SELECTRdata")
  dem <- terra::rast(dem)
  blocks <- download_census_blocks(template = dem)

  ## should be SpatVector
  testthat::expect_s4_class(blocks, "SpatVector")

  ## check that CRS's are matching
  testthat::expect_equal(terra::crs(blocks, proj = TRUE),
                         terra::crs(dem, proj = TRUE))

})
