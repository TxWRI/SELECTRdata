test_that("download_nass returns errors", {
  testthat::skip_if_not(has_nass_token())

  state_alpha = "TX"
  county_name = "Brazos"
  ## year must be character
  testthat::expect_error(
    download_nass_livestock(state_alpha,
                            county_name,
                            year = 2022)
  )

  ## year must be one of the census years
  testthat::expect_error(
    download_nass_livestock(state_alpha,
                            country_name,
                            year = "2018")
  )
})


test_that("download_nass returns expected objects", {
  testthat::skip_if_not(has_nass_token())

  x <- download_nass_livestock(state_alpha = "TX",
                               county_name = "Brazos",
                               year = "2022")
  testthat::expect_s3_class(x, "data.frame")

})
