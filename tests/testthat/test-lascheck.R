context("lascheck")

LASfile <- system.file("extdata", "example.laz", package = "rlas")
las1     <- readLAS(LASfile)
las1@data[1, X := NA_real_]
las1@data[5,] = las1@data[6,]
las1@data[5, Classification := 2L]
las1@data[6, Classification := 2L]
las1@data$gpstime <- 0
las1@proj4string <- sp::CRS("+init=epsg:26917")
las1@header@PHB[["X scale factor"]] <- 0.123
las1@header@PHB[["Y scale factor"]] <- 0.123
las1@header@PHB[["Z scale factor"]] <- 0.123
las1@header@PHB[["Point Data Format ID"]] <- 25


LASfile <- system.file("extdata", "extra_byte.laz", package = "rlas")
las2     <- readLAS(LASfile)
wkt(las2) <- rgdal::showWKT(sp::CRS("+init=epsg:26917")@projargs)
las2@proj4string <- sp::CRS()

LASfile <- system.file("extdata", "", package = "lidR")
ctg1 <- readLAScatalog(LASfile)

LASfile <- system.file("extdata", "example.laz", package = "rlas")
ctg2 <- readLAScatalog(LASfile)

test_that("lascheck works without error with LAS", {
  sink(tempfile())
  expect_error(lascheck(las1), NA)
  expect_error(lascheck(las2), NA)
  sink(NULL)
})

test_that("lascheck works without error with LAScatalog", {
  sink(tempfile())
  expect_error(lascheck(ctg1), NA)
  expect_error(lascheck(ctg2), NA)
  sink(NULL)
})


