# Changelog

## SELECTRdata 0.1.3 (2025-11-04)

### New Features

- template arguments accept SpatVector or SpatRaster objects.

## SELECTRdata 0.1.2 (2025-10-22)

### New Features

- Download USGS seamless DEMs with
  [`download_dem()`](https://txwri.github.io/SELECTRdata/reference/download_dem.md)

## SELECTRdata 0.1.1 (2025-10-01)

### Bug Fixes

- NLCD endpoint updated to the WCS geoserver, the S3 service used by the
  package was removed.

## SELECTRdata 0.1.0 (2025-03-05)

### New Features

- This is the first released version and includes functions to return
  SpatRaster, SpatVector or dataframe objects with data used in the
  Spatially Explicit Load Enrichment Calculation Tool (SELECT).
