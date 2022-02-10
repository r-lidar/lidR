context("rasterize_canopy")

slr = lidR:::raster_class()

test_that("rasterize_canopy with p2r() returns a georeferenced RasterLayer", {

  las <- lidR:::generate_las(2000)
  x   <- rasterize_canopy(las, 4, p2r())

  expected_bbox = sf::st_bbox(c(xmin = 0,xmax = 100, ymin = 0, ymax = 100), crs = st_crs(las))

  expect_true(is(x, slr))
  expect_equal(lidR:::raster_res(x), c(4,4))
  expect_equivalent(lidR:::raster_size(x), c(25,25,1))
  expect_equivalent(sf::st_bbox(x), expected_bbox)
  expect_equal(lidR:::raster_names(x), "Z")
})

test_that("grid_canopy with p2r() is backward compatible", {

  las <- lidR:::generate_las(2000)
  x   <- grid_canopy(las, 4, p2r())

  expected_bbox = sf::st_bbox(c(xmin = 0,xmax = 100, ymin = 0, ymax = 100), crs = st_crs(las))

  expect_true(is(x, "RasterLayer"))
  expect_equal(lidR:::raster_res(x), c(4,4))
  expect_equal(lidR:::raster_size(x), c(25,25,1))
  expect_equivalent(sf::st_bbox(x), expected_bbox)
  expect_equal(lidR:::raster_names(x), "Z")
})


test_that("rasterize_canopy with p2r() returns the good bounding box", {

  dt  <- data.table::data.table(X = seq(0,10, 0.5), Y = seq(0,10, 0.5), Z = runif(21))
  las <- LAS(dt, crs = sf::st_crs(26917))

  x <- rasterize_canopy(las, 2, p2r())
  expected_bbox = sf::st_bbox(c(xmin = -2, xmax = 12, ymin = -2,ymax = 12), crs = st_crs(las))

  expect_true(is(x, slr))
  expect_equivalent(sf::st_bbox(x), expected_bbox )

  x <- rasterize_canopy(las, 1, p2r())
  expected_bbox <- sf::st_bbox(c(xmin = -1, xmax = 11, ymin = -1,ymax = 11), crs = st_crs(las))

  expect_true(is(x, slr))
  expect_equivalent(sf::st_bbox(x), expected_bbox )
})

test_that("rasterize_canopy with p2r() works with subcircle option", {

  dt  <- data.table::data.table(X = c(0,10), Y = c(0,20), Z = c(0, 5))
  las <- suppressWarnings(LAS(dt, crs = sf::st_crs(26917)))
  x   <- rasterize_canopy(las, 0.5, p2r(10))
  expected_bbox <- sf::st_bbox(c(xmin = -0.5, xmax = 10.5, ymin = -0.5,ymax = 20.5), crs = st_crs(las))

  expect_true(is(x, slr))
  expect_equivalent(sf::st_bbox(x), expected_bbox)
})


test_that("rasterize_canopy pit-free works subcircle", {

  skip_on_cran()

  las <- lidR:::generate_las(2000)

  f <- pitfree(thresholds = c(0,2,5,10,15), max_edge = c(0, 1.5), subcircle = 5)
  x <- rasterize_canopy(las, 1, f)
  expected_bbox = sf::st_bbox(c(xmin = 0,xmax = 100, ymin = 0, ymax = 100), crs = st_crs(las))

  expect_true(is(x, slr))
  expect_equal(lidR:::raster_res(x), c(1,1))
  expect_equivalent(sf::st_bbox(x), expected_bbox)
})

test_that("rasterize_canopy with p2r() works with na.fill", {

  las <- lidR:::generate_las(1000)
  x   <- rasterize_canopy(las, 4, p2r())

  expect_equal(sum(is.na(lidR:::raster_values(x))), 118L)

  x   <- rasterize_canopy(las, 4, p2r(na.fill = knnidw(3)))

  expect_equal(sum(is.na(lidR:::raster_values(x))), 0L)
})

las <- readLAS(mixedconifer_las_path, select = "xyzr", filter = "-thin_with_grid 1")
ctg <- mixedconifer_ctg

opt_filter(ctg)          <- "-thin_with_grid 1"
opt_chunk_size(ctg)      <- 100
opt_chunk_alignment(ctg) <- c(5,15)
opt_progress(ctg)        <- FALSE

test_that("rasterize_canopy tin works both with LAS and LAScatalog", {

  f <- dsmtin()
  x <- rasterize_canopy(las, 1, f)
  y <- rasterize_canopy(ctg, 1, f)
  b <- sf::st_bbox(sf::st_buffer(sf::st_as_sfc(st_bbox(x)), -1))
  x <- lidR:::raster_crop(x, b)
  y <- lidR:::raster_crop(y, b)
  expected_bbox =  sf::st_bbox(c(xmin = 481261, xmax = 481349, ymin = 3812922,ymax = 3813010), crs = st_crs(las))

  expect_true(is(x, slr))
  expect_equal(lidR:::raster_res(x), c(1,1))
  expect_equivalent(lidR:::raster_size(x), c(88,88,1))
  expect_equivalent(sf::st_bbox(x), expected_bbox)
  expect_equal(lidR:::raster_names(x), "Z")
  expect_equal(lidR:::raster_values(x), lidR:::raster_values(y), tolerance = 0.00065)
})

test_that("rasterize_canopy pit-free works both with LAS and LAScatalog", {

  f <- pitfree(thresholds = c(0,2,5,10,15), max_edge = c(0, 1.5))
  x <- rasterize_canopy(las, 1, f)
  y <- rasterize_canopy(ctg, 1, f)
  b <- sf::st_bbox(sf::st_buffer(sf::st_as_sfc(st_bbox(x)), -1))
  x <- lidR:::raster_crop(x, b)
  y <- lidR:::raster_crop(y, b)
  expected_bbox =  sf::st_bbox(c(xmin = 481261, xmax = 481349, ymin = 3812922,ymax = 3813010), crs = st_crs(las))

  expect_true(is(x, slr))
  expect_equal(lidR:::raster_res(x), c(1,1))
  expect_equivalent(lidR:::raster_size(x), c(88,88,1))
  expect_equivalent(sf::st_bbox(x), expected_bbox)
  expect_equal(lidR:::raster_names(x), "Z")
  expect_equal(lidR:::raster_values(x), lidR:::raster_values(y), tolerance = 0.00079)
})

test_that("triangulation does not return negative values", {
  # See https://gis.stackexchange.com/questions/376261/negative-values-after-chm-rasterization-lidr
  las <- clip_rectangle(megaplot, 684750, 5017800, 684850, 5017900)
  chm <- rasterize_canopy(las, 1, dsmtin())
  expect_equal(min(lidR:::raster_values(chm), na.rm = T), 0)
})


