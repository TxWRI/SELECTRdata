test_that("ECHO argument checks work", {
  testthat::skip_on_cran()

  dem <- system.file("extdata", "thompsoncreek.tif", package = "SELECTRdata")
  dem <- terra::rast(dem)

  ## check permit component argument
  testthat::expect_error(download_NPDES_permits(dem,
                                                permit_component = "xxx",
                                                permit_status = "EFF",
                                                output = tempfile(fileext = ".gpkg"))
                         )

  ## check permit status argument
  testthat::expect_error(download_NPDES_permits(dem,
                                                permit_component = "POT",
                                                permit_status = "xxx",
                                                output = tempfile(fileext = ".gpkg"))
  )

})


test_that("download_NPDES_permits returns expected objects", {
  testthat::skip_on_cran()

  dem <- system.file("extdata", "thompsoncreek.tif", package = "SELECTRdata")
  dem <- terra::rast(dem)

  permits <- download_NPDES_permits(dem,
                                    permit_component = "POT",
                                    permit_status = "EFF",
                                    output = tempfile(fileext = ".gpkg"))
  testthat::expect_s4_class(permits, "SpatVector")

  ## check that CRS's are matching
  testthat::expect_equal(terra::crs(permits, proj = TRUE),
                         terra::crs(dem, proj = TRUE))
})
