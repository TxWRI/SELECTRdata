test_that("nass_token works", {

  skip_if_not_installed("withr", minimum_version = NULL)
  withr::with_envvar(new = c("NASSQS_TOKEN"="SECRET_CODE"),
                     {
                       nass_token <- has_nass_token()
                       testthat::expect_type(nass_token, "logical")
                       testthat::expect_equal(nass_token, TRUE)
                       })


  withr::with_envvar(new = c("NASSQS_TOKEN"=NA),
                     {
                       nass_token <- has_nass_token()
                       testthat::expect_equal(nass_token, FALSE)

                       testthat::expect_error(download_nass_livestock(state_alpha = "TX",
                                                                      county_name = "Brazos",
                                                                      year = "2022"))
                     })
})


test_that("check_connectivity works", {

  skip_on_cran()
  con <- expect_invisible(check_connectivity("192.0.2.10 "))
  testthat::expect_null(con)


  skip_if_offline()
  con <- check_connectivity("google.com")
  testthat::expect_type(con, "logical")
  testthat::expect_equal(con, TRUE)
})



test_that("check SpatRaster works", {
  x <- terra::rast(nrows=108, ncols=21, xmin=0, xmax=10)
  out <- expect_invisible(check_spat_ras(x))
  testthat::expect_null(out)

  x1 <- rbind(c(-180,-20), c(-140,55), c(10, 0), c(-140,-60))
  x2 <- rbind(c(-10,0), c(140,60), c(160,0), c(140,-55))
  x3 <- rbind(c(-125,0), c(0,60), c(40,5), c(15,-45))
  hole <- rbind(c(80,0), c(105,13), c(120,2), c(105,-13))
  z <- rbind(cbind(object=1, part=1, x1, hole=0), cbind(object=2, part=1, x3, hole=0),
             cbind(object=3, part=1, x2, hole=0), cbind(object=3, part=1, hole, hole=1))
  colnames(z)[3:4] <- c('x', 'y')

  x <- vect(z, "polygons")
  testthat::expect_error(check_spat_ras(x))

})

test_that("check_string works", {
  x <- "abc"
  out <- expect_invisible(check_string(x))
  testthat::expect_null(out)

  x <- list(1, FALSE, NULL)
  for(i in 1:length(x)) {
    testthat::expect_error(check_string(x[[i]]))
  }

  x <- "a"
  y <- c("a","b","c")
  out <- expect_invisible(check_string_contains(x, y))
  testthat::expect_null(out)

  y <- c("d","e","f")
  testthat::expect_error(check_string_contains(x, y))

})


test_that("arcgis helpers work",{

  testthat::skip_on_cran()
  furl <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services/Watershed_Boundary_Dataset_HUC_8s/FeatureServer"
  out <- expect_invisible(catch_arcgislayer_error(furl))
  testthat::expect_null(out)

  #broken url
  furl <-"https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services/Watershed_Boundary_Dataset_HUC/FeatureServer"
  testthat::expect_false(is.null(catch_arcgislayer_error(furl)))
})
