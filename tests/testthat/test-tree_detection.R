context("tree_detection")

LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
las = readLAS(LASfile, select = "xyzt", filter = "-drop_z_below 0")
ctg = catalog(LASfile)

opt_progress(ctg) <- FALSE
opt_chunk_alignment(ctg) <- c(-10,3812970)
opt_chunk_size(ctg) <- 60
opt_chunk_buffer(ctg) <- 20

test_that("tree_detection LMF works with a LAS", {

  ttops = tree_detection(las, lmf(5))

  expect_is(ttops, "SpatialPointsDataFrame")
  expect_true(is.integer(ttops$treeID))
  expect_equal(dim(ttops@data), c(177,2))
  expect_equal(ttops@proj4string, las@proj4string)

  f = function(x) { x * 0.07 + 3}
  ttops = tree_detection(las, lmf(f))

  expect_is(ttops, "SpatialPointsDataFrame")
  expect_true(is.integer(ttops$treeID))
  expect_equal(dim(ttops@data), c(205,2))
  expect_equal(ttops@proj4string, las@proj4string)
})

test_that("tree_detection LMF works with a RasterLayer", {

  chm = grid_canopy(las, 1, p2r(0.15))

  ttops = tree_detection(chm, lmf(5))

  expect_is(ttops, "SpatialPointsDataFrame")
  expect_equal(dim(ttops@data), c(160,2))
  expect_equal(ttops@proj4string, chm@crs)

  # variable windows size
  f = function(x) { x * 0.07 + 3 }
  ttops = tree_detection(chm, lmf(f))

  expect_is(ttops, "SpatialPointsDataFrame")
  expect_true(is.integer(ttops$treeID))
  expect_equal(dim(ttops@data), c(186,2))
  expect_equal(ttops@proj4string, chm@crs)
})

test_that("tree_detection LMF works with a LAScatalog", {

  ttops = tree_detection(ctg, lmf(5))

  expect_is(ttops, "SpatialPointsDataFrame")
  expect_true(is.integer(ttops$treeID))
  expect_equal(dim(ttops@data), c(177,2))
  expect_equal(ttops@proj4string, ctg@proj4string)
})

test_that("find_trees supports different unicity strategies", {

  ttops = find_trees(las, lmf(5), uniqueness = "gpstime")

  expect_true(is.double(ttops$treeID))
  expect_equal(mean(ttops$treeID), 151436.2, tol = 0.01)

  ttops = find_trees(las, lmf(5), uniqueness = "bitmerge")

  expect_true(is.double(ttops$treeID))
  expect_equal(mean(ttops$treeID), 206718581340247136)
})


test_that("Special test for R-devel with gcc8", {

  layout <- lidR:::rOverlay(las, 1, buffer = 0.15)
  lidR.context <- "grid_canopy"

  i  = c(38, 228,545)
  expected = c(NA, 17, 14.87)
  z <- p2r(0.15)(las, layout)

  expect_equal(z[i], expected)
})

#test_that("tree_detection LMFauto works with a LAS", {
#
#  ttops = tree_detection(las, lidR:::lmfauto())
#
#  expect_is(ttops, "SpatialPointsDataFrame")
#  expect_equal(dim(ttops@data), c(231,2))
#  expect_equal(ttops@proj4string, ctg@proj4string)
#})
