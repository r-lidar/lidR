context("grid_metrics")

las <- lidR:::generate_las(2000)
projection(las) <- 4326

test_that("grid_metrics returns a named RasterLayer", {

  x <- grid_metrics(las, ~list(Zmean = mean(Z)))

  expect_true(is(x, "RasterLayer"))
  expect_equal(raster::res(x), c(20,20))
  expect_equal(dim(x), c(5,5,1))
  expect_equal(raster::extent(x), raster::extent(0,100,0,100))
  expect_equal(x@crs, las@proj4string)
  expect_equal(names(x), "Zmean")
})

test_that("grid_metrics returns a RasterLayer aligned with the start option", {

  x <- grid_metrics(las, ~length(Z), start = c(10,10))

  expect_true(is(x, "RasterLayer"))
  expect_equal(raster::res(x), c(20,20))
  expect_equal(dim(x), c(6,6,1))
  expect_equal(raster::extent(x), raster::extent(-10,110,-10,110))
  expect_equal(x@crs, las@proj4string)
  expect_equal(names(x), "V1")
})

test_that("grid_metrics returns a named multilayers RasterBrick", {

  x <- grid_metrics(las, ~list(meanZ = mean(Z), maxZ = max(Z)))

  expect_true(is(x, "RasterBrick"))
  expect_equal(raster::res(x), c(20,20))
  expect_equal(dim(x), c(5,5,2))
  expect_equal(raster::extent(x), raster::extent(0,100,0,100))
  expect_equal(x@crs, las@proj4string)
  expect_equal(names(x), c("meanZ", "maxZ"))
})

test_that("grid_metrics returns a named multilayers RasterBrick aligned with the start option", {

  x <- grid_metrics(las, ~list(meanZ = mean(Z), maxZ = max(Z)), start = c(10,10))

  expect_true(is(x, "RasterBrick"))
  expect_equal(raster::res(x), c(20,20))
  expect_equal(dim(x), c(6,6,2))
  expect_equal(raster::extent(x), raster::extent(-10,110,-10,110))
  expect_equal(x@crs, las@proj4string)
  expect_equal(names(x), c("meanZ", "maxZ"))
})

test_that("grid_metrics returns a RasterLayer -- tricky case", {

  las2 <- filter_poi(las, X < 20 | X > 70)
  out  <- grid_metrics(las2, ~max(Z))

  expect_equal(dim(out), c(5, 5, 1))
  expect_equal(raster::res(out), c(20, 20))

  las2 <- filter_poi(las, (X < 20 | X > 70) & (Y < 20 | Y > 70))
  out  <- grid_metrics(las2, ~max(Z), 10)

  expect_equal(dim(out), c(10, 10, 1))
  expect_equal(raster::res(out), c(10, 10))
})

test_that("grid_metrics return a RasterBrick -- tricky case", {

  las2 <- filter_poi(las, (X < 20 | X > 80) & (Y < 20 | Y > 80))
  out  <- suppressWarnings(grid_metrics(las2, ~list(mean(Z), max(Z)), 10))

  expect_true(is(out, "RasterBrick"))
  expect_equal(dim(out), c(10, 10, 2))
  expect_equal(raster::res(out), c(10, 10))
})

test_that("grid_metrics accepts both an expression or a formula", {

  x <- grid_metrics(las,  list(mean(Z), max(Z)), 20)
  y <- grid_metrics(las, ~list(mean(Z), max(Z)), 20)

  expect_equal(x, y)
})

test_that("grid_metrics splits by echo type", {

  x <- grid_metrics(las, ~list(Zmean = mean(Z)), by_echo = "first")

  expect_true(is(x, "RasterLayer"))
  expect_equal(raster::res(x), c(20,20))
  expect_equal(dim(x), c(5,5,1))
  expect_equal(raster::extent(x), raster::extent(0,100,0,100))
  expect_equal(x@crs, las@proj4string)
  expect_equal(names(x), "Zmean.first")

  x <- grid_metrics(las, ~list(Zmean = mean(Z)), by_echo = c("first", "lastofmany"))

  expect_true(is(x, "RasterBrick"))
  expect_equal(raster::res(x), c(20,20))
  expect_equal(dim(x), c(5,5,2))
  expect_equal(raster::extent(x), raster::extent(0,100,0,100))
  expect_equal(x@crs, las@proj4string)
  expect_equal(names(x), c("Zmean.first", "Zmean.lastofmany"))

  x <- grid_metrics(las, ~list(Zmean = mean(Z)), by_echo = c("all", "first", "lastofmany"))

  expect_true(is(x, "RasterBrick"))
  expect_equal(raster::res(x), c(20,20))
  expect_equal(dim(x), c(5,5,3))
  expect_equal(raster::extent(x), raster::extent(0,100,0,100))
  expect_equal(x@crs, las@proj4string)
  expect_equal(names(x), c("Zmean", "Zmean.first", "Zmean.lastofmany"))
})

test_that("3 way to compute with first returns are giving the same", {

  x1 = grid_metrics(filter_last(megaplot), mean(Z), 20)
  x2 = grid_metrics(megaplot, mean(Z), 20, filter = ~ReturnNumber == NumberOfReturns)
  #x3 = grid_metrics(megaplot, mean(Z), 20, by_echos = "last")
  #names(x3) <- names(x1)

  expect_equal(x1, x2)
  #expect_equal(x1, x3) hey cannot be the same because by_echo = "last" does not consider single
})

las <- filter_first(megaplot)
las@data$Intensity <- NULL
ctg <- megaplot_ctg

opt_chunk_size(ctg)      <- 260
opt_chunk_alignment(ctg) <- c(160, 160)
opt_chunk_buffer(ctg)    <- 0
opt_progress(ctg)        <- FALSE
opt_select(ctg)          <- "xyz"
opt_filter(ctg)          <- "-keep_first"

test_that("grid_metric throw an error for missing attributes", {

  expect_error(grid_metrics(ctg, mean(Intensity), 20), "Intensity")
  expect_error(grid_metrics(las, mean(Intensity), 20), "Intensity")
})

test_that("grid_metric returns the same both with LAScatalog and LAS", {

  m1 <- grid_metrics(ctg, ~length(Z), 20)
  m2 <- grid_metrics(las, ~length(Z), 20)
  expect_equal(m1, m2)

  m1 <- grid_metrics(ctg, ~list(length(Z), mean(Z)), 20)
  m2 <- grid_metrics(las, ~list(length(Z), mean(Z)), 20)
  m1@data@isfactor <- m2@data@isfactor
  expect_equivalent(m1, m2)
})

test_that("grid_metric return the same both with catalog and las + grid alignment", {

  m1 <- grid_metrics(ctg, ~length(Z), 20, start = c(10,10))
  m2 <- grid_metrics(las, ~length(Z), 20, start = c(10,10))
  expect_equal(m1, m2)
})

test_that("grid_metric works with a RasterLayer as input instead of a resolution", {

  # --- partially matching bbox

  bb = round(extent(las))
  bb@xmin = bb@xmin - 160
  bb@xmax = bb@xmax - 160
  r <- raster::raster(bb)
  raster::res(r) <- 15
  raster::crs(r) <- crs(las)

  m1 <- grid_metrics(las, ~length(Z), r)

  expect_equal(raster::extent(m1), raster::extent(r))
  expect_equal(sum(!is.na(m1[])), 80L)
  expect_equal(sum(is.na(m1[])), 160L)

  # --- no matching bbox

  bb = round(extent(las))
  bb@xmin = bb@xmin - 360
  bb@xmax = bb@xmax - 360
  r <- raster::raster(bb)
  raster::res(r) <- 15
  raster::crs(r) <- crs(las)

  m1 <- suppressWarnings(grid_metrics(las, ~length(Z), r))

  expect_equal(raster::extent(m1), raster::extent(r))
  expect_equal(sum(is.na(m1[])), raster::ncell(r))
  expect_warning(grid_metrics(las, ~length(Z), r), "Bounding boxes are not intersecting")
})


test_that("predefined metric set work both with a LAS and LAScatalog", {

  las <- random_500_points

  expect_error(grid_metrics(las, .stdmetrics_z), NA)
  expect_error(grid_metrics(las, .stdmetrics_i), NA)
  expect_error(grid_metrics(las, .stdmetrics_rn), NA)
  expect_error(grid_metrics(las, .stdmetrics_ctrl), NA)
  expect_error(grid_metrics(las, .stdshapemetrics), NA)
  expect_error(grid_metrics(ctg, .stdmetrics_z), NA)
})

test_that("Using a non empty layout return correct output (#318)", {

  ldr = filter_poi(megaplot, ScanAngleRank >= -3, ScanAngleRank <= 3 )
  ref = lidR:::rOverlay(ldr, 20)
  suppressWarnings(ref[] <- 10)
  m = grid_metrics(ldr, mean(Z), ref)

  expect_equal(sum(is.na(m[])), 52L)
})




