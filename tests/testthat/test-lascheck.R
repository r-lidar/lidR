context("lascheck")

LASfile <- system.file("extdata", "example.laz", package = "rlas")
las0    <- readLAS(LASfile)

las1 <- las0
las1@data <- data.table::copy(las0@data)

las1@data[1, X := NA_real_]
las1@data[5,] = las1@data[6,]
las1@data[7,1:2] = las1@data[6, 1:2]
las1@data[5:7, Classification := 2L]
las1@data[1, Withheld_flag := TRUE]
las1@data[1, Synthetic_flag := TRUE]
las1@data[1, Keypoint_flag := TRUE]
las1@data$gpstime <- 0
las1@proj4string <- sp::CRS("+init=epsg:26917")
las1@header@PHB[["X scale factor"]] <- 0.123
las1@header@PHB[["Y scale factor"]] <- 0.123
las1@header@PHB[["Z scale factor"]] <- 0.123
las1@header@PHB[["Point Data Format ID"]] <- 25


LASfile <- system.file("extdata", "extra_byte.laz", package = "rlas")
las2     <- readLAS(LASfile, select = "xyz")
wkt(las2) <- rgdal::showWKT(sp::CRS("+init=epsg:26917")@projargs)
las2@proj4string <- sp::CRS()

las3 <- las1
epsg(las3) <- 2008
las3@proj4string <- sp::CRS("+init=epsg:26917")

las4 = las2
las4@proj4string <- sp::CRS("+init=epsg:2008")

LASfile <- system.file("extdata", "example.laz", package = "rlas")
ctg0 <- readLAScatalog(LASfile)

LASfile <- system.file("extdata", "", package = "lidR")
ctg1 <- readLAScatalog(LASfile)

ctg2 <- ctg1
ctg2@data$X.scale.factor <- 0.012
ctg2@data$Y.scale.factor <- 0.012
ctg2@data$Z.scale.factor <- 0.012
ctg2@data$Min.Z <- -1
ctg2@data$Point.Data.Format.ID = 12L

test_that("lascheck works without error with LAS", {
  sink(tempfile())
  expect_error(lascheck(las0), NA)
  expect_error(lascheck(las1), NA)
  expect_error(lascheck(las2), NA)
  expect_error(lascheck(las3), NA)
  expect_error(lascheck(las4), NA)
  sink(NULL)
})

test_that("lascheck works without error with LAScatalog", {
  sink(tempfile())
  expect_error(lascheck(ctg0), NA)
  expect_error(lascheck(ctg1), NA)
  expect_error(lascheck(ctg2), NA)
  sink(NULL)
})

test_that("lascheck CRS specific test", {

  sink(tempfile())

  las1 <- las0
  epsg(las1) <- 2008
  las1@header@PHB$`Global Encoding`$WKT <- TRUE

  las2 <- las0
  wkt(las2) <-  "COMPD_CS[\"Projected\", PROJCS[\"UTM_10N\", GEOGCS [ \"WGS84\", DATUM [ \"WGS84\", SPHEROID [\"WGS 84\", 6378137.000, 298.257223563 ], TOWGS84 [ 0.000, 0.000, 0.000, 0.0000000000, 0.0000000000, 0.0000000000, 0.0000000000 ] ], PRIMEM [ \"Greenwich\", 0.000000 ], UNIT [ \"metres\", 1.00000000] ], PROJECTION[\"Transverse_Mercator\"], PARAMETER[\"Latitude_of_Origin\",0.0000000000], PARAMETER[\"Central_Meridian\",-123.0000000000], PARAMETER[\"Scale_Factor\",0.9996000000], PARAMETER[\"False_Easting\",500000.000], PARAMETER[\"False_Northing\",0.000], UNIT [ \"metres\", 1.00000000]] ], VERT_CS[\"NAVD88 (Geoid03) ContUS\", VERT_DATUM[\"./Resources/CoordSysData/navd88_geo03_contus.bin\", 1 ], UNIT [ \"metres\", 1.00000000] ] ]"
  las2@header@PHB$`Global Encoding`$WKT <- FALSE

  expect_error(lascheck(las1), NA)
  expect_error(lascheck(las2), NA)

  epsg(las0) <- 2008
  las0@proj4string <- sp::CRS()

  expect_error(lascheck(las0), NA)

  las0@header@VLR$GeoKeyDirectoryTag$tags[[1]]$`value offset` <- 200800

  expect_error(lascheck(las0), NA)

  las2@header@VLR$`WKT OGC CS`$`WKT OGC COORDINATE SYSTEM` <- "INVALID"
  las2@header@PHB$`Global Encoding`$WKT <- TRUE

  expect_error(lascheck(las2), NA)

  sink(NULL)
})



