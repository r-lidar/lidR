context("grid_terrain")

las <- lidR:::dummy_las(5000)
projection(las) <- sp::CRS("+init=epsg:4326")
las@data[, Z := Z + 0.1*X + 0.1*Y + sin(0.01*X) - sin(0.1*Y) + sin(0.003*X*Y)]

tdtm   <- lidR:::make_overlay_raster(las, 1)
xy     <- raster::xyFromCell(tdtm, 1:raster::ncell(tdtm))
X      <- xy[,1]
Y      <- xy[,2]
tdtm[] <- 0.1*X + 0.1*Y + sin(0.01*X) - sin(0.1*Y) + sin(0.003*X*Y)

test_that("grid_terrain works with knnidw", {

  dtm <- grid_terrain(las, 1, knnidw(k = 10L))

  expect_true(is(dtm, "RasterLayer"))
  expect_equal(raster::res(dtm), c(1,1))
  expect_equal(dim(dtm), dim(tdtm))
  expect_equal(raster::extent(dtm), raster::extent(tdtm))
  expect_equal(dtm@crs, las@proj4string)
  expect_equal(names(dtm), "Z")

  error <- abs(dtm - tdtm)
  expect_equal(mean(error[], na.rm = TRUE), 0.155768, tolerance = 0.00001)

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

  error <- abs(dtm - tdtm)
  expect_equal(mean(error[], na.rm = TRUE), 0.0739201, tolerance = 0.00001)

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

  error <- abs(dtm - tdtm)
  expect_equal(mean(error[], na.rm = TRUE), 0.0603822, tolerance = 0.00001)

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
  expect_equal(mean(error[], na.rm = TRUE), 0.06584327, tolerance = 0.00001)

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

test_that("grid_terrain return the same both with LAScatalog and LAS", {

  dtm1 <- grid_terrain(las, 1, tin())
  dtm2 <- grid_terrain(ctg, 1, tin())

  bbox <- raster::extent(dtm1) - 8

  cdtm1 <- raster::crop(dtm1, bbox)
  cdtm2 <- raster::crop(dtm2, bbox)

  error <- abs(cdtm1 - cdtm2)
  error <- error[error > 0.01]

  # skip unreprodutible error on CRAN with 32 bits arch and Solaris
  #if (!is.na(mean(error) & !is.null(error)))
  #{
  #  expect_lt(length(error), raster::ncell(cdtm1)*0.002)
  #  expect_equal(mean(error, na.rm = TRUE), 0.048, tolerance = 0.0055)
  #}

  z <- raster::extract(dtm2, las@data[, .(X,Y)])
  expect_true(!anyNA(z))
})
