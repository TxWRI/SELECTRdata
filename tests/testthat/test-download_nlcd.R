## tests needed:
## - returns terra rast
## - handles coordinate system inconsistency correctly
## [x] returns null if status code != 200
## [x] returns null and message if content type != tiff
## - successfully checks arg types



test_that("get_nlcd errors on certain args", {
  #testthat::skip_on_cran()
  testthat::skip_if_offline()

  landmasses <- c("HI", "AK")
  for(i in 1:length(landmasses)) {
    landmass <- landmasses[[i]]
    testthat::expect_error(download_nlcd(landmass, 2024, "LndCov"))
  }

  # check invalid years return error
  testthat::expect_error(download_nlcd("CU", 1980, "LndCov"))

  # non implemented endpoints should error
  datasets <- c("LndChg","LndCnf","FctImp","ImpDsc","SpcChg")
  for(i in 1:length(datasets)) {
    dataset <- datasets[[i]]
    testthat::expect_error(download_nlcd("CU", 2024, dataset))
  }
})

test_that("get_nlcd and friends correctly handle 400 and 500 web responses",{
  #testthat::skip_on_cran()
  testthat::skip_if_offline()

  dem <- system.file("extdata", "thompsoncreek.tif", package = "SELECTRdata")
  dem <- terra::rast(dem)

  mock_404 <- function(req) {
    httr2::response(status_code = 404)
  }

  ## should return invisible null with message
  testthat::expect_invisible(
    httr2::with_mocked_responses(mock_404,
                                 download_nlcd(template = dem, year = "2024")))


  ## returns 202 but with xml message and not requested tif file
  mock_content <- function(req) {
    httr2::response(status_code = 200,
             headers = c("Content-Type: application/xml"),
             body = charToRaw('<?xml version="1.0" encoding="UTF-8"?><ows:ExceptionReport xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:ows="http://www.opengis.net/ows/2.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" version="2.0.0" xsi:schemaLocation="http://www.opengis.net/ows/2.0 http://schemas.opengis.net/ows/2.0/owsExceptionReport.xsd">
  <ows:Exception exceptionCode="NoApplicableCode">
    <ows:ExceptionText>Failed to read the coverage mrlc_Land-Cover-Native_conus_year_data:Land-Cover-Native_conus_year_data
The coordinate reference system must be the same for all objects.</ows:ExceptionText>
  </ows:Exception>
</ows:ExceptionReport>
'))
  }


  testthat::expect_invisible(
    httr2::with_mocked_responses(mock_content,
                        SELECTRdata:::request_mrlc_download(resource = "https://dmsdata.cr.usgs.gov/geoserver/mrlc_Land-Cover-Native_conus_year_data/wcs",
                                                            coverage = "mrlc_Land-Cover-Native_conus_year_data:Land-Cover-Native_conus_year_data",
                                                            extent = terra::ext(dem),
                                                            time = "2023-01-01T00:00:00.000Z",
                                                            nlcd_epsg = "5070"
                        ))
  )
})
