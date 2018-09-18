context("tree_detection")

LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
las = readLAS(LASfile, select = "xyz", filter = "-drop_z_below 0")
ctg = catalog(LASfile)
set_cores(ctg) <- 1
set_progress(ctg) <- FALSE
set_alignment(ctg) <- c(-10,3812970)
set_tiling_size(ctg) <- 60
set_buffer(ctg) <- 20

test_that("tree_detection LMF works with a LAS", {

  ttops = tree_detection(las, lmf(5))

  expect_is(ttops, "SpatialPointsDataFrame")
  expect_equal(dim(ttops@data), c(175,2))
  expect_equal(ttops@proj4string, las@proj4string)

  f = function(x) { x * 0.07 + 3}
  ttops = tree_detection(las, lmf(f))

  expect_is(ttops, "SpatialPointsDataFrame")
  expect_equal(dim(ttops@data), c(205,2))
  expect_equal(ttops@proj4string, las@proj4string)
})

test_that("tree_detection LMF works with a RasterLayer", {

  chm = grid_canopy(las, 1, p2r(0.15))

  ttops = tree_detection(chm, lmf(5))

  expect_is(ttops, "SpatialPointsDataFrame")
  expect_equal(dim(ttops@data), c(157,2))
  expect_equal(ttops@proj4string, chm@crs)

  # variable windows size
  f = function(x) { x * 0.07 + 3 }
  ttops = tree_detection(chm, lmf(f))

  expect_is(ttops, "SpatialPointsDataFrame")
  expect_equal(dim(ttops@data), c(183,2))
  expect_equal(ttops@proj4string, chm@crs)
})

test_that("tree_detection LMF works with a LAScatalog", {

  ttops = tree_detection(ctg, lmf(5))

  expect_is(ttops, "SpatialPointsDataFrame")
  expect_equal(dim(ttops@data), c(175,2))
  expect_equal(ttops@proj4string, ctg@proj4string)
})

#-------------------------------------------------

test_that("tree_detection mutltichm works with a LAS", {

  ttops = tree_detection(las, multichm(res = 2, layer_thickness = 2, ws = 5))

  expect_is(ttops, "SpatialPointsDataFrame")
  expect_equal(dim(ttops@data), c(243,2))
  expect_equal(ttops@proj4string, las@proj4string)
})

test_that("tree_detection multichm works with a LAScatalog", {

  ttops = tree_detection(ctg, multichm(res = 2, layer_thickness = 2, ws = 5))

  expect_is(ttops, "SpatialPointsDataFrame")
  expect_equal(dim(ttops@data), c(243,2))
  expect_equal(ttops@proj4string, ctg@proj4string)
})

#-------------------------------------------------

test_that("tree_detection ptree works with a LAS", {

  ttops = tree_detection(las, ptrees(k = c(30,15)))

  expect_is(ttops, "SpatialPointsDataFrame")
  expect_equal(dim(ttops@data), c(233,2))
  expect_equal(ttops@proj4string, las@proj4string)
})

test_that("tree_detection ptree works with a LAScatalog", {
  ttops = tree_detection(ctg, ptrees(k = c(30,15)))

  expect_is(ttops, "SpatialPointsDataFrame")
  expect_equal(dim(ttops@data), c(231,2))
  expect_equal(ttops@proj4string, ctg@proj4string)
})
