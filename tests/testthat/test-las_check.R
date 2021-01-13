context("las_check")

las0    <- example
las1 <- las0
las1@data <- data.table::copy(las0@data)

#las1@data[1, X := NA_real_]
las1@data[1, Z := -5]
las1@data[5,] = las1@data[6,]
las1@data[7,1:2] = las1@data[6, 1:2]
las1@data[5:7, Classification := 2L]
las1@data[1, Withheld_flag := TRUE]
las1@data[1, Synthetic_flag := TRUE]
las1@data[1, Keypoint_flag := TRUE]
las1@data[25, Z := 1234567890.1]
las1@data$gpstime <- 0
las1@proj4string <- sp::CRS("+init=epsg:26917")
las1@header@PHB[["X scale factor"]] <- 0.123
las1@header@PHB[["Y scale factor"]] <- 0.123
las1@header@PHB[["Z scale factor"]] <- 0.123
las1@header@PHB[["Point Data Format ID"]] <- 25


LASfile <- system.file("extdata", "extra_byte.laz", package = "rlas")
las2     <- readLAS(LASfile, select = "xyz")
las2@header@PHB$`Global Encoding`$WKT = TRUE
wkt(las2) <- rgdal::showWKT(sp::CRS("+init=epsg:26917")@projargs)
las2@proj4string <- sp::CRS()

las3 <- las1
epsg(las3) <- 2008
las3@proj4string <- sp::CRS("+init=epsg:26917")

las4 = las2
las4@proj4string <- sp::CRS("+init=epsg:2008")

ctg0 <- example_ctg

LASfile <- system.file("extdata", "", package = "lidR")
ctg1 <- readLAScatalog(LASfile)

ctg2 <- ctg1
ctg2@data$X.scale.factor <- 0.012
ctg2@data$Y.scale.factor <- 0.012
ctg2@data$Z.scale.factor <- 0.012
ctg2@data$Min.Z <- -1
ctg2@data$Point.Data.Format.ID = 12L

test_that("las_check works without error with LAS", {
  sink(tempfile())
  expect_error(las_check(las0), NA)
  expect_error(las_check(las1), NA)

  skip_on_cran()

  expect_error(las_check(las2), NA)
  expect_error(las_check(las3), NA)
  expect_error(las_check(las4), NA)
  sink(NULL)
})

test_that("las_check works without error with LAScatalog", {
  sink(tempfile())
  expect_error(las_check(ctg0), NA)
  expect_error(las_check(ctg1), NA)
  expect_error(las_check(ctg2), NA)
  sink(NULL)
})

test_that("las_check CRS specific test", {

  sink(tempfile())

  las1 <- las0
  epsg(las1) <- 2008
  las1@header@PHB$`Global Encoding`$WKT <- TRUE

  las2 <- las0
  las2@header@PHB$`Global Encoding`$WKT = TRUE
  wkt(las2) <- "PROJCS[\"RD_New\",GEOGCS[\"GCS_Amersfoort\",DATUM[\"D_Amersfoort\",SPHEROID[\"Bessel_1841\",6377397.155,299.1528128]],PRIMEM[\"Greenwich\",0.0],UNIT[\"Degree\",0.0174532925199433]],PROJECTION[\"Double_Stereographic\"],PARAMETER[\"False_Easting\",155000.0],PARAMETER[\"False_Northing\",463000.0],PARAMETER[\"Central_Meridian\",5.38763888888889],PARAMETER[\"Scale_Factor\",0.9999079],PARAMETER[\"Latitude_Of_Origin\",52.1561605555556],UNIT[\"Meter\",1.0]]"
  las2@header@PHB$`Global Encoding`$WKT <- FALSE

  expect_error(las_check(las1), NA)
  expect_error(las_check(las2), NA)

  epsg(las0) <- 2008
  las0@proj4string <- sp::CRS()

  expect_error(las_check(las0), NA)

  las0@header@VLR$GeoKeyDirectoryTag$tags[[1]]$`value offset` <- 200800

  expect_error(las_check(las0), NA)

  las2@header@VLR$`WKT OGC CS`$`WKT OGC COORDINATE SYSTEM` <- "INVALID"
  las2@header@PHB$`Global Encoding`$WKT <- TRUE

  expect_error(las_check(las2), NA)

  sink(NULL)
})

test_that("las_check quantization specific test", {

  sink(tempfile())

  x = las0@data[["X"]][5]
  y = las0@data[["Y"]][5]
  z = las0@data[["Z"]][5]

  las0@header@PHB$`Min X` <- las0@header@PHB$`Min X` + 0.0001
  las0@header@PHB$`Max Y` <- las0@header@PHB$`Max Y` + 0.0001
  las0@header@PHB$`Max Z` <- las0@header@PHB$`Max Z` + 0.0001
  las0@data[["X"]][5] <- x + 0.00123
  las0@data[["Y"]][5] <- y + 0.000123
  las0@data[["Z"]][5] <- z + 0.0000123

  expect_error(las_check(las0), NA)

  sink(NULL)
})

test_that("las_check performs a deep inspection of a LAScatalog", {

  ctg = lidR:::catalog_generator(2, 20)
  o1 = las_check(ctg, print = FALSE, deep = TRUE)

  expect_equal(length(o1), 2L)
})

test_that("las_check returns a list of troubleshooting", {

  report = las_check(las1, FALSE)

  expect_is(report, "list")
  expect_equal(names(report), c("warnings", "errors"))

  report = las_check(ctg1, FALSE)

  expect_is(report, "list")
  expect_equal(names(report), c("warnings", "errors"))
})
