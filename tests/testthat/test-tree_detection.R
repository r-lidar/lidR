context("tree_detection")

LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
las = readLAS(LASfile, select = "xyz", filter = "-drop_z_below 0")
ctg = catalog(LASfile)

opt_progress(ctg) <- FALSE
opt_chunk_alignment(ctg) <- c(-10,3812970)
opt_chunk_size(ctg) <- 60
opt_chunk_buffer(ctg) <- 20

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
