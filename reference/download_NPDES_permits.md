# Download NPDES permits

Downloads NPDES permits from EPA ECHO API services within the bounds of
the SpatRast template.

## Usage

``` r
download_NPDES_permits(
  template,
  permit_component = "POT",
  permit_status = "EFF",
  output = tempfile(fileext = ".gpkg")
)
```

## Arguments

- template:

  A SpatRaster or SpatVector object. The extent of the returned object
  will match `template`.

- permit_component:

  A character vector with one or more of the following: `PRE`
  (pretreatment), `CAF` (CAFO), `CSO` (CSO), `POT` (Publicly Owned
  Treatment Works, the default), `BIO` (Biosolids), `SWS` (Stormwater
  Small MS4), `SWM` (Stormwater Medium/Large MS4), `SWI` (Stormwater
  Industrial), `SWC` (Stormwater Construction).

- permit_status:

  A character vector with one or more of the following: `EFF`
  (effective, the default), `EXP` (expired), `PND` (pending), `TRM`
  (terminated), `RET` (retired), `NON` (not needed), `ADC`
  (administratively continued).

- output:

  A character file path specifying where the raster file should be
  stored. Defaults to a temporary file.

## Value

SpatVector object

## Examples

``` r
dem <- system.file("extdata", "thompsoncreek.tif", package = "SELECTRdata")
dem <- terra::rast(dem)
download_NPDES_permits(dem)
#> ℹ Query returned 3 results!
#>  class       : SpatVector 
#>  geometry    : points 
#>  dimensions  : 3, 23  (geometries, attributes)
#>  extent      : -45160.96, -39459.16, 834830.5, 843617.7  (xmin, xmax, ymin, ymax)
#>  source      : file20bc2cd6ef84.gpkg
#>  coord. ref. : +proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs 
#>  names       :          CWPName  SourceID        CWPStreet       CWPCity
#>  type        :            <chr>     <chr>            <chr>         <chr>
#>  values      :  RELLIS CAMPUS ~ TX0076023  APPROX 3.0 MI ~ BRAZOS COUNTY
#>                STILL CREEK WWTP TX0025071  2028 QUALITY P~         BRYAN
#>                 THOMPSONS CREE~ TX0128554 7198 VERLOREN LN         BRYAN
#>  CWPState CWPStateDistrict CWPZip MasterExternalPermitNmbr EPASystem Statute
#>     <chr>            <chr>  <chr>                    <chr>     <chr>   <chr>
#>        TX               09  77807                       NA       ICP     CWA
#>        TX               09  77805                       NA       ICP     CWA
#>        TX               09  77807                       NA       ICP     CWA
#>  (and 13 more)
#>               
#>               
#>               
#>               
```
