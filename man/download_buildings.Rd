% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_buildings.R
\name{download_buildings}
\alias{download_buildings}
\title{Download building footprints}
\usage{
download_buildings(template, return = "SpatVector")
}
\arguments{
\item{template}{A SpatRaster object. The extent of the returned object will match \code{template}.}

\item{return}{A character object, either \code{SpatVector} or \code{sf}. Defaults to \code{SpatVector}.}
}
\value{
A \code{SpatVector} or \code{sf} object with extents matching the \code{SpatRaster} object provided in the \code{template} argument. If API resources are not available an invisible \code{NULL} is returned.
}
\description{
Downloads building footprints and attribute data from FEMA's
USA Structures dataset (https://fema.maps.arcgis.com/home/item.html?id=0ec8512ad21e4bb987d7e848d14e7e24#overview).
}
\examples{
\donttest{
## This example requires an internet connection to run
dem <- system.file("extdata", "thompsoncreek.tif", package = "SELECTRdata")
dem <- terra::rast(dem)

buildings <- download_buildings(template = dem)
}

}
