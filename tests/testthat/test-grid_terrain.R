context("grid_terrain")
rgdal::set_thin_PROJ6_warnings(TRUE)

las <- lidR:::dummy_las(5000)
projection(las) <- sp::CRS("+init=epsg:4326")
las@data[, Z := round(Z + 0.1*X + 0.1*Y + sin(0.01*X) - sin(0.1*Y) + sin(0.003*X*Y),3)]

tdtm   <- lidR:::rOverlay(las, 1)
xy     <- raster::xyFromCell(tdtm, 1:raster::ncell(tdtm))
X      <- xy[,1]
Y      <- xy[,2]
tdtm[] <- round(0.1*X + 0.1*Y + sin(0.01*X) - sin(0.1*Y) + sin(0.003*X*Y),3)

test_that("grid_terrain works with knnidw", {

  dtm <- grid_terrain(las, 1, knnidw(k = 10L))

  expect_true(is(dtm, "RasterLayer"))
  expect_equal(raster::res(dtm), c(1,1))
  expect_equal(dim(dtm), dim(tdtm))
  expect_equal(raster::extent(dtm), raster::extent(tdtm))
  expect_equal(dtm@crs, las@proj4string)
  expect_equal(names(dtm), "Z")
  expect_equal(sum(is.na(dtm[])), 1L)

  error <- abs(dtm - tdtm)
  expect_equal(mean(error[], na.rm = TRUE), 0.1558, tolerance = 0.0001)

  z <- raster::extract(dtm, las@data[, .(X,Y)])
  expect_true(!anyNA(z))
})

test_that("grid_terrain works with delaunay", {

  dtm <- suppressWarnings(grid_terrain(las, 1, tin()))

  expect_true(is(dtm, "RasterLayer"))
  expect_equal(raster::res(dtm), c(1,1))
  expect_equal(dim(dtm), dim(tdtm))
  expect_equal(raster::extent(dtm), raster::extent(tdtm))
  expect_equal(dtm@crs, las@proj4string)
  expect_equal(names(dtm), "Z")
  expect_equal(sum(is.na(dtm[])), 1L)

  error <- abs(dtm - tdtm)
  expect_equal(mean(error[], na.rm = TRUE), 0.0739, tolerance = 0.0001)

  z <- raster::extract(dtm, las@data[, .(X,Y)])
  expect_true(!anyNA(z))
})

test_that("grid_terrain works with kriging", {

  dtm <- grid_terrain(las, 1, kriging(k = 10L))

  expect_true(is(dtm, "RasterLayer"))
  expect_equal(raster::res(dtm), c(1,1))
  expect_equal(dim(dtm), dim(tdtm))
  expect_equal(raster::extent(dtm), raster::extent(tdtm))
  expect_equal(dtm@crs, las@proj4string)
  expect_equal(names(dtm), "Z")
  expect_equal(sum(is.na(dtm[])), 1L)

  error <- abs(dtm - tdtm)
  expect_equal(mean(error[], na.rm = TRUE), 0.0604, tolerance = 0.0001)

  z <- raster::extract(dtm, las@data[, .(X,Y)])
  expect_true(!anyNA(z))
})

test_that("grid_terrain option keep_lowest works", {

  dtm <- grid_terrain(las, 1, tin(), keep_lowest = TRUE)

  expect_true(is(dtm, "RasterLayer"))
  expect_equal(raster::res(dtm), c(1,1))
  expect_equal(dim(dtm), dim(tdtm))
  expect_equal(raster::extent(dtm), raster::extent(tdtm))
  expect_equal(dtm@crs, las@proj4string)
  expect_equal(names(dtm), "Z")
  expect_equal(sum(is.na(dtm[])), 1L)

  error <- abs(dtm - tdtm)
  expect_equal(mean(error[], na.rm = TRUE), 0.0754, tolerance = 0.0001)

  z <- raster::extract(dtm, las@data[, .(X,Y)])
  expect_true(!anyNA(z))
})

las2 <- lasclipCircle(las, 50, 50, 40)

test_that("grid_terrain returns a circular dtm ", {

  dtm <- grid_terrain(las2, 1, tin())

  expect_true(is(dtm, "RasterLayer"))
  expect_equal(raster::res(dtm), c(1,1))
  expect_equal(dtm@crs, las@proj4string)
  expect_equal(names(dtm), "Z")

  error <- suppressWarnings(abs(dtm - tdtm))
  expect_equal(mean(error[], na.rm = TRUE), 0.065, tolerance = 0.0005)

  z <- raster::extract(dtm, las2@data[, .(X,Y)])
  expect_true(!anyNA(z))
})

file <- system.file("extdata", "Topography.laz", package = "lidR")
ctg  <- catalog(file)
las  <- readLAS(file)

opt_chunk_size(ctg)      <- 180
opt_chunk_buffer(ctg)    <- 30
opt_chunk_alignment(ctg) <- c(332400, 5238400)
opt_progress(ctg)        <- FALSE

test_that("grid_terrain returns the same both with LAScatalog and LAS", {

  dtm1 <- grid_terrain(las, 1, tin())
  dtm2 <- grid_terrain(ctg, 1, tin())

  bbox <- raster::extent(dtm1) - 8

  cdtm1 <- raster::crop(dtm1, bbox)
  cdtm2 <- raster::crop(dtm2, bbox)

  error <- abs(cdtm1 - cdtm2)
  error <- error[error > 0.01]

  expect_equal(mean(error), 0.0195, tolerance = 0.001)

  z <- raster::extract(dtm2, las@data[, .(X,Y)])
  expect_true(!anyNA(z))
})

test_that("grid_terrain fails in some specific case", {

  las@header@PHB$`X scale factor` <- 0.002
  las@header@PHB$`Y scale factor` <- 0.002

  expect_message(grid_terrain(las, 1, tin()), "reverted to the old slow method")

  las = data.frame(
    X = runif(10, 0,10),
    Y = runif(10, 0,10),
    Z = 1,
    Classification = 2L)

  las = LAS(las)

  expect_error(grid_terrain(las, 1, tin()), NA)

  las@data$Classification <- 1L
  expect_error(grid_terrain(las, 1, tin()), "No ground points found")

  expect_error(grid_terrain(las, 1, tin(), use_class = 1), NA)

  las@data$Classification <- NULL
  expect_error(grid_terrain(las, 1, tin()), "LAS object does not contain 'Classification' attribute")


})

