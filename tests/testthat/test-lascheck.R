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


