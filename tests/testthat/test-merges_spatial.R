context("merge_spatial")

las     <- decimate_points(megaplot, highest(7))
sflakes <- sf::st_read(system.file("extdata", "", package="lidR"), "lake_polygons_UTM17", quiet = TRUE)
sf::st_agr(sflakes) <- "constant"
sflakes <- sf::st_crop(sflakes, st_bbox(las))
sflakes$ID <- 1L
splakes <- sf::as_Spatial(sflakes)
zmean   <- pixel_metrics(las, mean(Z))
rgb     <- stars::st_as_stars(st_bbox(las), dx = 10, dy = 10, nz = 3L, values = runif(300, 0, 255))
rgbi    <- stars::st_as_stars(st_bbox(las), dx = 20, dy = 20, nz = 4L, values = 10)

test_that("merge_spatial works with SpatialPolygonsDataFrame", {

  las <- merge_spatial(las, splakes, "inlakes")
  cn <- names(las)

  expect_true("inlakes" %in% cn)
  expect_true(is.logical(las$inlakes))
  expect_equivalent(as.numeric(table(las$inlakes)), c(966, 216))

  las <- merge_spatial(las, splakes, "LAKENAME_1")
  cn <- names(las)

  expect_true("LAKENAME_1" %in% cn)
  expect_equal(typeof(las$LAKENAME_1), typeof(splakes$LAKENAME_1))
  expect_equivalent(as.numeric(table(las$LAKENAME_1)), c(216))

  las <- merge_spatial(las, splakes)
  cn <- names(las)

  expect_true("id" %in% cn)
  expect_true(is.integer(las$id))
  expect_equivalent(as.numeric(table(las$id)), c(216))
})

test_that("merge_spatial works with sf", {

  las <- merge_spatial(las, sflakes, "inlakes")
  cn  <- names(las)

  expect_true("inlakes" %in% cn)
  expect_true(is.logical(las$inlakes))
  expect_equivalent(as.numeric(table(las$inlakes)), c(966, 216))

  las <- merge_spatial(las, sflakes, "LAKENAME_1")
  cn <- names(las)

  expect_true("LAKENAME_1" %in% cn)
  expect_equal(typeof(las$LAKENAME_1), typeof(sflakes$LAKENAME_1))
  expect_equivalent(as.numeric(table(las$LAKENAME_1)), c(216))

  las <- merge_spatial(las, sflakes)
  cn <- names(las)

  expect_true("id" %in% cn)
  expect_true(is.integer(las$id))
  expect_equivalent(as.numeric(table(las$id)), c(216))
})

test_that("merge_spatial preserve storage mode", {

  las <- merge_spatial(las, sflakes, "ID")
  cn  <- names(las)

  expect_equal(storage.mode(las$ID), storage.mode(sflakes$ID))
})


test_that("merge_spatial works with SpatialPolygons", {

  lakes <- as(splakes, "SpatialPolygons")

  las <- merge_spatial(las, lakes)
  cn <- names(las)

  expect_true("id" %in% cn)
  expect_true(is.integer(las$id))
  expect_equivalent(as.numeric(table(las$id)), c(216))
})

test_that("merge_spatial works with sfc", {

  las <- merge_spatial(las, sf::st_geometry(sflakes), "inlakes")
  cn <- names(las)

  expect_true("inlakes" %in% cn)
  expect_true(is.logical(las$inlakes))
  expect_equivalent(as.numeric(table(las$inlakes)), c(966, 216))

  las <- merge_spatial(las, sf::st_geometry(sflakes))
  cn <- names(las)

  expect_true("id" %in% cn)
  expect_true(is.integer(las$id))
  expect_equivalent(as.numeric(table(las$id)), c(216))
})

test_that("merge_spatial does not fail if no polygon encompass the points", {

  shift <- sf::st_sfc(sf::st_point(c(2000,200)), crs = sf::st_crs(sflakes))
  lakes <- sf::st_geometry(sflakes) + shift

  las <- merge_spatial(las, lakes)
  cn  <- names(las)

  expect_true("id" %in% cn)
  expect_true(is.integer(las$id))
  expect_true(all(is.na(las$id)))
})

test_that("merge_spatial works with raster", {

  las <- merge_spatial(las, as(zmean, "Raster"), "Zmean")
  cn  <- names(las)

  expect_true("Zmean" %in% cn)
  expect_true(is.numeric(las$Zmean))
  expect_equal(mean(las$Zmean), 17.9, tol = 0.01)
})

test_that("merge_spatial works with stars", {

  las <- merge_spatial(las, zmean, "Zmean")
  cn  <- names(las)

  expect_true("Zmean" %in% cn)
  expect_true(is.numeric(las$Zmean))
  expect_equal(mean(las$Zmean), 17.9, tol = 0.01)
})

test_that("merge_spatial works a RGB RasterBrick", {

  las <- merge_spatial(las, as(rgb, "Raster"))
  cn  <- names(las)

  expect_true(all(c("R", "G", "B") %in% cn))
  expect_true(is.integer(las$R))
  expect_equal(las[["Point Data Format ID"]], 3L)
})

test_that("merge_spatial works a RGB stars", {

  las <- merge_spatial(las, rgb)
  cn  <- names(las)

  expect_true(all(c("R", "G", "B") %in% cn))
  expect_true(is.integer(las$R))
  expect_equal(las[["Point Data Format ID"]], 3L)
})

test_that("merge_spatial works a RGB SpatRaster", {

  las <- merge_spatial(las, as(rgb, "SpatRaster"))
  cn  <- names(las)

  expect_true(all(c("R", "G", "B") %in% cn))
  expect_true(is.integer(las$R))
  expect_equal(las[["Point Data Format ID"]], 3L)
})


test_that("merge_spatial fails with too much bands", {

  expect_error(merge_spatial(las, rgbi), "rasters must have 1 or 3 bands")
  expect_error(merge_spatial(las, as(rgbi, "Raster")), "rasters must have 1 or 3 bands")
})

test_that("merge_spatial fails with unknown input type", {

  expect_error(merge_spatial(las, 4), "No method for this source format")
})

test_that("merge_spatial do not fail with 1 point (#347)", {

  one_in <- filter_poi(las, Intensity == 105)

  res1 <- merge_spatial(one_in, sflakes)
  res2 <- merge_spatial(one_in, splakes)
  res3 <- merge_spatial(one_in, sf::st_geometry(sflakes))

  expect_equal(res1$id, 1L)
  expect_equal(res2$id, 1L)
  expect_equal(res3$id, 1L)
})


