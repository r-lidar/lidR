context("grid_canopy")

test_that("grid_canopy with p2r() returns a georeferenced RasterLayer", {

  las <- lidR:::dummy_las(2000)
  x   <- grid_canopy(las, 4, p2r())

  expect_true(is(x, "RasterLayer"))
  expect_equal(raster::res(x), c(4,4))
  expect_equal(dim(x), c(25,25,1))
  expect_equal(raster::extent(x), raster::extent(0,100,0,100))
  expect_equal(projection(x), projection(las))
  expect_equal(names(x), "Z")
})

test_that("grid_canopy with p2r() returns the good bounding box", {

  dt  <- data.table::data.table(X = seq(0,10, 0.5), Y = seq(0,10, 0.5), Z = runif(21))
  las <- LAS(dt)

  x <- grid_canopy(las, 2, p2r())

  expect_true(is(x, "RasterLayer"))
  expect_equal(raster::extent(x), raster::extent(-2,12,-2,12))

  x <- grid_canopy(las, 1, p2r())

  expect_true(is(x, "RasterLayer"))
  expect_equal(raster::extent(x), raster::extent(-1,11,-1,11))
})

test_that("grid_canopy with p2r() works with subcircle option", {

  dt  <- data.table::data.table(X = 0, Y = 0, Z = 0)
  las <- LAS(dt)
  x   <- grid_canopy(las, 0.5, p2r(10))

  expect_true(is(x, "RasterLayer"))
  expect_equal(raster::extent(x), raster::extent(-10.5,10.5,-10.5,10.5))

  dt  <- data.table::data.table(X = c(0,10), Y = c(0,20), Z = c(0, 5))
  las <- suppressWarnings(LAS(dt))
  x   <- grid_canopy(las, 0.5, p2r(10))

  expect_true(is(x, "RasterLayer"))
  expect_equal(raster::extent(x), raster::extent(-10.5,20.5,-10.5,30.5))
})

test_that("grid_canopy with p2r() works with na.fill", {

  las <- lidR:::dummy_las(1000)
  x   <- grid_canopy(las, 4, p2r())

  expect_equal(sum(is.na(x[])), 118L)

  x   <- grid_canopy(las, 4, p2r(na.fill = knnidw(3)))

  expect_equal(sum(is.na(x[])), 0L)
})

LASfile <- system.file("extdata", "MixedConifer.laz", package = "lidR")
las <- readLAS(LASfile, select = "xyzr", filter = "-thin_with_grid 1")
ctg <- catalog(LASfile)

opt_filter(ctg)          <- "-thin_with_grid 1"
opt_chunk_size(ctg)      <- 50
opt_chunk_alignment(ctg) <- c(5,15)
opt_chunk_buffer(ctg)    <- 10
opt_progress(ctg)        <- FALSE

test_that("grid_canopy tin works both with LAS and LAScatalog", {

  f <- dsmtin()
  x <- grid_canopy(las, 1, f)
  y <- grid_canopy(ctg, 1, f)
  x <- crop(x, extent(x) - 2)
  y <- crop(y, extent(y) - 2)

  expect_true(is(x, "RasterLayer"))
  expect_equal(raster::res(x), c(1,1))
  expect_equal(dim(x), c(88,88,1))
  expect_equal(raster::extent(x), raster::extent(481261,481349,3812922,3813010))
  expect_equal(projection(x), projection(las))
  expect_equal(names(x), "Z")
  expect_equal(x, y, tolerance = 0.00065)
})

test_that("grid_canopy pit-free works both with LAS and LAScatalog", {

  f <- pitfree(thresholds = c(0,2,5,10,15), max_edge = c(0, 1.5))
  x <- grid_canopy(las, 1, f)
  y <- grid_canopy(ctg, 1, f)
  x <- crop(x, extent(x) - 2)
  y <- crop(y, extent(y) - 2)

  expect_true(is(x, "RasterLayer"))
  expect_equal(raster::res(x), c(1,1))
  expect_equal(dim(x), c(88,88,1))
  expect_equal(raster::extent(x), raster::extent(481261,481349,3812922,3813010))
  expect_equal(projection(x), projection(las))
  expect_equal(names(x), "Z")
  expect_equal(x, y, tolerance = 0.00079)
})

test_that("triangulation does not return negative values", {
  # See https://gis.stackexchange.com/questions/376261/negative-values-after-chm-rasterization-lidr
  LASfile <- system.file("extdata", "Megaplot.laz", package = "lidR")
  las <- readLAS(LASfile, select = "xyzr", filter = "-inside 684750 5017800 684850 5017900")
  chm = grid_canopy(las, 1, dsmtin())
  expect_equal(min(chm[], na.rm = T), 0)
})


