% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_nlcd.R
\name{download_nlcd}
\alias{download_nlcd}
\title{Downlaod and write NLCD raster data to file.}
\usage{
download_nlcd(
  template,
  year = "2021",
  dataset = "land_cover",
  landmass = "l48",
  output = tempfile(fileext = ".tiff"),
  overwrite = FALSE,
  verbose = FALSE,
  ...
)
}
\arguments{
\item{template}{A SpatRaster object defining the spatial extent of the returned NLCD raster.}

\item{year}{character, expects one of: \code{c("2001", "2004", "2006", "2008", "2011", "2013", "2016", "2019", "2021")}. Note that not all years are available for all landmass areas. See https://www.mrlc.gov/data-services-page for data and area availability.}

\item{dataset}{Character, currently only supports \code{land_cover}. Future support for impervious and tree canopy cover is possible.}

\item{landmass}{Character, one of: \code{c("l48", "ak", "hi", "pr")}.}

\item{output}{A character file path specifying where the raster file should be stored. Defaults to a temporary file.}

\item{overwrite}{logical. If \code{TRUE}, filename is overwritten}

\item{verbose}{Logical, if \code{TRUE} informative messages will be printed.}

\item{...}{additional arguments for for writing files, see \code{terra::writeRaster()}}
}
\value{
A SpatRaster object with file written to \code{output}
}
\description{
Downloads and writes an NLCD SpatRaster to file with extents defined by \code{template}.
}
