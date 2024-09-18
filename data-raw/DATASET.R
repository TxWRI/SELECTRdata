## code to prepare `DATASET` dataset goes here

## create example data

library(terra)
library(archive)
library(purrr)
library(sf)
library(dplyr)
## this function will download. extract, and read the nhd raster
download_nhd <- function(url,
                         rel_path) {
  # download the files
  tmpfile <- tempfile()
  ras <- download.file(url = url,
                       destfile = tmpfile,
                       method = "libcurl",
                       mode = "wb")

  # unzip the raster
  tmpdir <- tempdir()
  archive_extract(tmpfile, tmpdir)

  filepath <- paste0(tmpdir, rel_path)

  # reads the raster
  ras <- terra::rast(filepath)

  # deletes temp
  unlink(tmpdir)
  unlink(tmpfile)

  return(ras)
}

download_wbd <- function(url,
                         rel_path) {
  # download the files
  tmpfile <- tempfile()
  wbd <- download.file(url = url,
                       destfile = tmpfile,
                       method = "libcurl",
                       mode = "wb")

  # unzip the raster
  tmpdir <- tempdir()
  archive_extract(tmpfile, tmpdir)

  filepath <- paste0(tmpdir, rel_path)

  # reads the raster
  shp <- terra::vect(filepath)

  # deletes temp
  unlink(tmpdir)
  unlink(tmpfile)

  return(shp)
}

## note that the rel_path forward slashes should be escaped backslashes on windows systems
elevation <- download_nhd(url = "http://www.horizon-systems.com/NHDPlusData/NHDPlusV21/Data/NHDPlusTX/NHDPlusV21_TX_12_12b_Hydrodem_01.7z",
                          rel_path = "\\NHDPlusTX\\NHDPlus12\\NHDPlusHydrodem12b\\hydrodem")



wbd <- download_wbd(url = "http://www.horizon-systems.com/NHDPlusData/NHDPlusV21/Data/NHDPlusTX/NHDPlusV21_TX_12_WBDSnapshot_03.7z",
                    rel_path = "\\NHDPlusTX\\NHDPlus12\\WBDSnapshot\\WBD\\WBD_Subwatershed.shp")


wbd <- terra::project(wbd, elevation)

wbd <- wbd[wbd$HUC_12=="120701010702",]


elevation <- terra::crop(elevation, wbd, mask = FALSE)

## download assessmentunits
au <- download_wbd(url = "https://opendata.arcgis.com/datasets/175c3cb32f2840eca2bf877b93173ff9_4.zip?outSR=%7B%22falseM%22%3A-100000%2C%22xyTolerance%22%3A8.98315284119521e-9%2C%22mUnits%22%3A10000%2C%22zUnits%22%3A1%2C%22latestWkid%22%3A4269%2C%22zTolerance%22%3A2%2C%22wkid%22%3A4269%2C%22xyUnits%22%3A11258999068426.24%2C%22mTolerance%22%3A0.001%2C%22falseX%22%3A-400%2C%22falseY%22%3A-400%2C%22falseZ%22%3A0%7D",
                   rel_path = "\\Assessment_Units_-_Line.shp")
## subset AU to lines of interest
au <- au[au$AU_ID %in% c("1242D_01"),]
## project the AU
au <- terra::project(au, crs(elevation))
## this function will convert the lines points and get the ending coordinates of the line
## this assumes the line goes upstream to downstream

get_endpoints <- function(x) {
  nrow0 <- dim(geom(x))[1] - 10
  geom(x)[nrow0,c("x","y")]
}





endpoints <- au |>
  split("AU_ID") |>
  map(\(x) get_endpoints(x))

endpoints <- endpoints %>%
  map_df(~as_tibble(t(as.matrix(.x)))) %>% ## maps the coords by row
  mutate(id = names(endpoints)) ## this provides id by row

pourpoint <- terra::vect(endpoints, geom = c("x", "y"))

terra::writeVector(wbd,
                   "inst\\extdata\\thompsoncreek.gpkg",
                   layer = "wbd",
                   insert = TRUE)
terra::writeVector(pourpoint,
                   "inst\\extdata\\thompsoncreek.gpkg",
                   layer = "pourpoint",
                   insert = TRUE)

terra::writeRaster(elevation,
                   "inst\\extdata\\thompsoncreek.tif",
                   overwrite = TRUE,
                   gdal = c("COMPRESS=DEFLATE"))
