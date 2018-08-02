context("tree_detection")

lidr_options(progress = FALSE)

LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
las = readLAS(LASfile, select = "xyz", filter = "-drop_z_below 0")
las@crs = sp::CRS("+init=epsg:26748")
ctg = catalog(LASfile)
ctg@proj4string = sp::CRS("+init=epsg:26748")
cores(ctg) <- 1
progress(ctg) <- FALSE
tiling_size(ctg) <- 60
buffer(ctg) <- 20


test_that("tree_detection LMF works with a LAS", {

  ttops = tree_detection_lmf(las, 5)

  expect_is(ttops, "SpatialPointsDataFrame")
  expect_equal(dim(ttops@data), c(175,2))
  expect_equal(ttops@proj4string, las@crs)

  f = function(x) { x * 0.07 + 3}
  ttops = tree_detection_lmf(las, f)

  expect_is(ttops, "SpatialPointsDataFrame")
  expect_equal(dim(ttops@data), c(205,2))
  expect_equal(ttops@proj4string, las@crs)
})

test_that("tree_detection LMF works with a RasterLayer", {

  chm = grid_canopy(las, 1, subcircle = 0.15)
  chm = as.raster(chm)
  chm@crs = sp::CRS("+init=epsg:26917")

  ttops = tree_detection_lmf(chm, 5)

  expect_is(ttops, "SpatialPointsDataFrame")
  expect_equal(dim(ttops@data), c(156,2))
  expect_equal(ttops@proj4string, chm@crs)

  # variable windows size
  f = function(x) { x * 0.07 + 3 }
  ttops = tree_detection_lmf(chm, f)

  expect_is(ttops, "SpatialPointsDataFrame")
  expect_equal(dim(ttops@data), c(183,2))
  expect_equal(ttops@proj4string, chm@crs)
})

test_that("tree_detection LMF works with a LAScatalog", {

  ttops = tree_detection_lmf(ctg, 5)

  expect_is(ttops, "SpatialPointsDataFrame")
  expect_equal(dim(ttops@data), c(175,2))
  expect_equal(ttops@proj4string, ctg@proj4string)
})

#-------------------------------------------------

test_that("tree_detection mutltichm works with a LAS", {

  ttops = tree_detection_multichm(las, 2, layer_thickness = 2)

  expect_is(ttops, "SpatialPointsDataFrame")
  expect_equal(dim(ttops@data), c(235,2))
  expect_equal(ttops@proj4string, las@crs)
})

test_that("tree_detection LMF works with a LAScatalog", {

  ttops = tree_detection_multichm(ctg, 2, layer_thickness = 2)

  expect_is(ttops, "SpatialPointsDataFrame")
  expect_equal(dim(ttops@data), c(235,2))
  expect_equal(ttops@proj4string, ctg@proj4string)
})

#-------------------------------------------------

test_that("tree_detection ptree works with a LAS", {

  ttops = tree_detection_ptrees(las, k = c(30,20,15,10))

  expect_is(ttops, "SpatialPointsDataFrame")
  expect_equal(dim(ttops@data), c(271,2))
  expect_equal(ttops@proj4string, las@crs)
})

test_that("tree_detection ptree works with a LAScatalog", {
  buffer(ctg) <- 30
  ttops = tree_detection_ptrees(ctg, k = c(30,20,15,10))

  expect_is(ttops, "SpatialPointsDataFrame")
  expect_equal(dim(ttops@data), c(271,2))
  expect_equal(ttops@proj4string, ctg@proj4string)
})