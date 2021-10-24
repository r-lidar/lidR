context("find_trees")

las = filter_poi(mixedconifer, Z >= 0)
ctg = mixedconifer_ctg

opt_progress(ctg) <- FALSE
opt_chunk_alignment(ctg) <- c(50,60)
opt_chunk_size(ctg) <- 100
opt_chunk_buffer(ctg) <- 15

test_that("find_trees LMF works with a LAS", {

  ttops = find_trees(las, lmf(5))

  expect_is(ttops, "SpatialPointsDataFrame")
  expect_true(is.integer(ttops$treeID))
  expect_equal(dim(ttops@data), c(177,2))
  expect_equal(ttops@proj4string, las@proj4string)

  f = function(x) { x * 0.07 + 3}
  ttops = find_trees(las, lmf(f))

  expect_is(ttops, "SpatialPointsDataFrame")
  expect_true(is.integer(ttops$treeID))
  expect_equal(dim(ttops@data), c(205,2))
  expect_equal(ttops@proj4string, las@proj4string)
})

test_that("find_trees LMF works with a RasterLayer", {

  chm = grid_canopy(las, 1, p2r(0.15))

  ttops = find_trees(chm, lmf(5))

  expect_is(ttops, "SpatialPointsDataFrame")
  expect_equal(dim(ttops@data), c(171,2))
  expect_equal(ttops@proj4string, chm@crs)

  # variable windows size
  f = function(x) { x * 0.07 + 3 }
  ttops = find_trees(chm, lmf(f))

  expect_is(ttops, "SpatialPointsDataFrame")
  expect_true(is.integer(ttops$treeID))
  expect_equal(dim(ttops@data), c(207,2))
  expect_equal(ttops@proj4string, chm@crs)
})

test_that("find_trees LMF works with a LAScatalog", {

  ttops = find_trees(ctg, lmf(5))

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
  expect_equal(mean(ttops$treeID), 2.067186e17, tolerance = 1e-5)
})


test_that("Special test for R-devel with gcc8", {

  layout <- lidR:::rOverlay(las, 1, buffer = 0.15)
  lidR.context <- "grid_canopy"

  i  = c(38, 228,545)
  expected = c(NA, 17, 14.87)
  z <- p2r(0.15)(las, layout)

  expect_equal(z[i], expected)
})

#test_that("find_trees LMFauto works with a LAS", {
#
#  ttops = find_trees(las, lidR:::lmfauto())
#
#  expect_is(ttops, "SpatialPointsDataFrame")
#  expect_equal(dim(ttops@data), c(231,2))
#  expect_equal(ttops@proj4string, ctg@proj4string)
#})
