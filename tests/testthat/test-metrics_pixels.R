las <- lidR:::generate_las(2000)
f1  <- ~list(Zmean = mean(Z))
f2  <- ~list(meanZ = mean(Z), maxZ = max(Z))

expected_bbox  <- sf::st_bbox(c(xmin = 0, xmax = 100, ymin = 0, ymax = 100), crs = st_crs(las))
expected_bbox2 <- sf::st_bbox(c(xmin = -10, xmax = 110, ymin = -10, ymax = 110), crs = st_crs(las))

slr = lidR:::raster_class()
mlr = lidR:::raster_multilayer_class()

test_that("pixel_metrics returns a named raster", {

  x <- pixel_metrics(las, f1)

  expect_true(is(x, slr))
  expect_equal(lidR:::raster_res(x), c(20,20))
  expect_equivalent(lidR:::raster_size(x), c(5,5,1))
  expect_equivalent(sf::st_bbox(x), expected_bbox)
  expect_equal(lidR:::raster_names(x), "Zmean")
})

test_that("pixel_metrics returns a named multilayers raster", {

  x <- pixel_metrics(las, f2)
  y <- pixel_metrics(las, f1)

  expect_true(is(x, mlr))
  expect_equal(lidR:::raster_res(x), c(20,20))
  expect_equivalent(lidR:::raster_size(x), c(5,5,2))
  expect_equivalent(sf::st_bbox(x), expected_bbox)
  expect_equal(lidR:::raster_names(x), c("meanZ", "maxZ"))
})

test_that("pixel_metrics return raster terra or stars", {

  x <- pixel_metrics(las, f2, pkg = "raster")
  expect_true(is(x, "RasterBrick"))

  x <- pixel_metrics(las, f2, pkg = "terra")
  expect_true(is(x, "SpatRaster"))

  x <- pixel_metrics(las, f2, pkg = "stars")
  expect_true(is(x, "stars"))
})

test_that("pixel_metrics returns a raster aligned with the start option", {

  x <- pixel_metrics(las, f1, start = c(10,10))

  expect_true(is(x, slr))
  expect_equal(lidR:::raster_res(x), c(20,20))
  expect_equivalent(lidR:::raster_size(x), c(6,6, 1))
  expect_equivalent(sf::st_bbox(x), expected_bbox2)
  expect_equal(lidR:::raster_names(x), "Zmean")
})

test_that("pixel_metrics returns a named multilayers raster", {

  x <- pixel_metrics(las, f2)

  expect_true(is(x, mlr))
  expect_equal(lidR:::raster_res(x), c(20,20))
  expect_equivalent(lidR:::raster_size(x), c(5,5,2))
  expect_equivalent(sf::st_bbox(x), expected_bbox)
  expect_equal(lidR:::raster_names(x), c("meanZ", "maxZ"))
})

test_that("pixel_metrics returns a named multilayers raster aligned with the start option", {

  x <- pixel_metrics(las, f2, start = c(10,10))

  expect_true(is(x, mlr))
  expect_equal(lidR:::raster_res(x), c(20,20))
  expect_equivalent(lidR:::raster_size(x), c(6,6,2))
  expect_equivalent(sf::st_bbox(x), expected_bbox2)
  expect_equal(lidR:::raster_names(x), c("meanZ", "maxZ"))
})

test_that("pixel_metrics returns a raster  -- tricky case", {

  las2 <- filter_poi(las, X < 20 | X > 70)
  out  <- pixel_metrics(las2, f1)

  expect_equivalent(lidR:::raster_size(out), c(5,5,1))
  expect_equal(lidR:::raster_res(out), c(20, 20))

  las2 <- filter_poi(las, (X < 20 | X > 70) & (Y < 20 | Y > 70))
  out  <- pixel_metrics(las2, f1, 10)

  expect_equivalent(lidR:::raster_size(out), c(10,10,1))
  expect_equal(lidR:::raster_res(out), c(10, 10))
})

test_that("pixel_metrics return a raster -- tricky case", {

  las2 <- filter_poi(las, (X < 20 | X > 80) & (Y < 20 | Y > 80))
  out  <- pixel_metrics(las2, f2, 10)

  expect_equivalent(lidR:::raster_size(out), c(10,10,2))
  expect_equal(lidR:::raster_res(out), c(10, 10))
})

test_that("pixel_metrics accepts both an expression or a formula", {

  x <- pixel_metrics(las,  list(mean(Z), max(Z)), 20)
  y <- pixel_metrics(las, ~list(mean(Z), max(Z)), 20)

  expect_equal(x, y)
})

test_that("pixel_metrics splits by echo type", {

  x <- pixel_metrics(las, f1, by_echo = c("first"))

  expect_equal(lidR:::raster_res(x), c(20,20))
  expect_equivalent(lidR:::raster_size(x), c(5,5,1))
  expect_equivalent(sf::st_bbox(x), expected_bbox)
  expect_equal(lidR:::raster_names(x), "Zmean.first")

  x <- pixel_metrics(las, f1, by_echo = c("first", "lastofmany"))

  expect_equal(lidR:::raster_res(x), c(20,20))
  expect_equivalent(lidR:::raster_size(x), c(5,5,2))
  expect_equivalent(sf::st_bbox(x), expected_bbox)
  expect_equal(lidR:::raster_names(x), c("Zmean.first", "Zmean.lastofmany"))

  x <- pixel_metrics(las, f2, by_echo = c("all", "first", "lastofmany"))

  expect_equal(lidR:::raster_res(x), c(20,20))
  expect_equivalent(lidR:::raster_size(x), c(5,5,6))
  expect_equivalent(sf::st_bbox(x), expected_bbox)
  expect_equal(lidR:::raster_names(x), c("meanZ", "maxZ", "meanZ.first", "maxZ.first", "meanZ.lastofmany", "maxZ.lastofmany"))
})


test_that("3 way to compute with first returns are giving the same", {

  x1 <- pixel_metrics(filter_last(megaplot), f1, 20)
  x2 <- pixel_metrics(megaplot, f1, 20, filter = ~ReturnNumber == NumberOfReturns)

  expect_equal(x1, x2)
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

bb <- as.list(round(st_bbox(las)))
bb$xmin <- bb$xmin - 160
bb$xmax <- bb$xmax - 160
bb <- sf::st_bbox(unlist(bb), crs <- st_crs(las))
stars1  <- stars::st_as_stars(bb, dx = 15, inside = TRUE)
raster1 <- as(stars1, "Raster")

bb <- as.list(st_bbox(las))
bb$xmin <- bb$xmin - 360
bb$xmax <- bb$xmax - 360
bb <- sf::st_bbox(unlist(bb), crs <- st_crs(las))
stars2  <- stars::st_as_stars(bb, dx = 15, inside = TRUE)
raster2 <- as(stars2, "Raster")

test_that("pixel_metric returns the same both with LAScatalog and LAS", {

  # We need to add a tolerance because stars::st_mosaic writes to float32 files and
  # some accuracy is lost.
  m1 <- pixel_metrics(ctg, f1, 20)
  m2 <- pixel_metrics(las, f1, 20)
  expect_equivalent(m1, m2, tolerance = 3e-8)

  m1 <- pixel_metrics(ctg, f2, 20)
  m2 <- pixel_metrics(las, f2, 20)
  expect_equivalent(m1, m2, tolerance = 3e-8)

  m1 <- pixel_metrics(ctg, f1, 20, start = c(10,10))
  m2 <- pixel_metrics(las, f1, 20, start = c(10,10))
  expect_equivalent(m1, m2, tolerance = 3e-8)
})

test_that("pixel_metric works with a RasterLayer as input instead of a resolution", {

  # --- partially matching bbox

  m <- pixel_metrics(las, f1, raster1)

  expect_true(is(m, "RasterLayer"))
  expect_equivalent(sf::st_bbox(m), sf::st_bbox(raster1))
  expect_equal(sum(!is.na(m[])), 75L)
  expect_equal(sum(is.na(m[])), 150L)

  # --- no matching bbox

  expect_warning(m <- pixel_metrics(las, f1, raster2), "Bounding boxes are not intersecting")
  expect_true(is(m, "RasterLayer"))
  expect_equivalent(sf::st_bbox(m), sf::st_bbox(raster2))
  expect_equal(sum(is.na(m[])), 225)
})

test_that("pixel_metric works with a stars as input instead of a resolution", {

  # --- partially matching bbox

  m <- pixel_metrics(las, f1, stars1)

  expect_true(is(m, "stars"))
  expect_equal(sf::st_bbox(m), sf::st_bbox(raster1))
  expect_equal(sum(!is.na(m[[1]])), 75L)
  expect_equal(sum(is.na(m[[1]])), 150L)

  # --- no matching bbox

  expect_warning(m <- pixel_metrics(las, f1, stars2), "Bounding boxes are not intersecting")
  expect_equal(sf::st_bbox(m), sf::st_bbox(raster2))
  expect_equal(sum(is.na(m[[1]])), 225)
})

test_that("predefined metric set work both with a LAS and LAScatalog", {

  las <- random_500_points

  expect_error(pixel_metrics(las, .stdmetrics_z), NA)
  expect_error(pixel_metrics(las, .stdmetrics_i), NA)
  expect_error(pixel_metrics(las, .stdmetrics_rn), NA)
  expect_error(pixel_metrics(las, .stdmetrics_ctrl), NA)
  expect_error(pixel_metrics(las, .stdshapemetrics), NA)
  expect_error(pixel_metrics(ctg, .stdmetrics_z), NA)
})

test_that("Using a non empty layout return correct output (#318)", {

  ldr <- filter_poi(megaplot, ScanAngleRank >= -3, ScanAngleRank <= 3 )
  ref <- lidR:::raster_layout(ldr, 20, format = "stars")
  ref[[1]][] <- 10

  m <- pixel_metrics(ldr, mean(Z), ref)

  expect_equal(sum(is.na(m[[1]])), 52L)
})

test_that("grid_metrics is backward compatible", {

  x <- grid_metrics(las, f1)

  expect_true(is(x, "RasterLayer"))
  expect_equal(lidR:::raster_res(x), c(20,20))
  expect_equal(dim(x), c(13,12,1))
  expect_true(sf::st_crs(x) == st_crs(las))
  expect_equal(names(x), "Zmean")
  expect_equal(mean(x[], na.rm = T), 13.459, tolerance = 0.001)

  x <- grid_metrics(las, f2)

  expect_true(is(x, "RasterBrick"))
  expect_equal(lidR:::raster_res(x), c(20,20))
  expect_equal(dim(x), c(13,12,2))
  expect_true(sf::st_crs(x) == st_crs(las))
  expect_equal(names(x), c("meanZ", "maxZ"))
  expect_equal(mean(x$meanZ[], na.rm = T), 13.459, tolerance = 0.001)

  x <- grid_metrics(las, f1, by_echo = c("first", "lastofmany"))

  expect_true(is(x, "RasterBrick"))
  expect_equal(lidR:::raster_res(x), c(20,20))
  expect_equal(dim(x), c(13,12,2))
  expect_true(sf::st_crs(x) == st_crs(las))
  expect_equal(names(x), c("Zmean.first", "Zmean.lastofmany"))
  expect_equal(mean(x$Zmean.first[], na.rm = T), 13.459, tolerance = 0.001)
  expect_true(all(is.na(x$Zmean.lastofmany[])))


  m1 <- grid_metrics(ctg, f1, 20)
  m2 <- grid_metrics(las, f1, 20)
  expect_equivalent(m1, m2)
  expect_equal(names(m2), "Zmean")

  m1 <- grid_metrics(ctg, f2, 20)
  m2 <- grid_metrics(las, f2, 20)
  expect_equivalent(m1, m2)
  expect_equal(names(m2), c("meanZ", "maxZ"))
})




