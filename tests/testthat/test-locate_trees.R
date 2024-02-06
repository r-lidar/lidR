context("locate_trees")

las = filter_poi(mixedconifer, Z >= 0)
ctg = mixedconifer_ctg

opt_progress(ctg) <- FALSE
opt_chunk_alignment(ctg) <- c(50,60)
opt_chunk_size(ctg) <- 100
opt_chunk_buffer(ctg) <- 15

test_that("locate_trees LMF works with a LAS", {

  ttops = locate_trees(las, lmf(5))

  expect_is(ttops, "sf")
  expect_true(is.integer(ttops$treeID))
  expect_equal(dim(ttops), c(177,3))
  expect_true(sf::st_crs(ttops) == st_crs(las))

  f = function(x) { x * 0.07 + 3}
  ttops = locate_trees(las, lmf(f))

  expect_is(ttops, "sf")
  expect_true(is.integer(ttops$treeID))
  expect_equal(dim(ttops), c(205,3))
  expect_true(sf::st_crs(ttops) == st_crs(las))
})

test_that("find_trees is backward compatible", {
  skip_if_not_installed("sp")

  ttops = find_trees(las, lmf(5))

  expect_is(ttops, "SpatialPointsDataFrame")
  expect_true(is.integer(ttops$treeID))
  expect_equal(dim(ttops), c(177,2))
  expect_true(sf::st_crs(ttops) == st_crs(las))

  f = function(x) { x * 0.07 + 3}
  ttops = find_trees(las, lmf(f))

  expect_is(ttops, "SpatialPointsDataFrame")
  expect_true(is.integer(ttops$treeID))
  expect_equal(dim(ttops), c(205,2))
  expect_true(sf::st_crs(ttops) == st_crs(las))
})


test_that("locate_trees LMF works with a RasterLayer ", {
  skip_if_not_installed("raster")
  chm = grid_canopy(las, 1, p2r(0.15))

  ttops = locate_trees(chm, lmf(5))

  expect_is(ttops, "sf")
  expect_equal(dim(ttops), c(167,3))
  expect_true(sf::st_crs(ttops) == sf::st_crs(chm))

  # variable windows size
  f = function(x) { x * 0.07 + 3 }
  ttops = locate_trees(chm, lmf(f))

  expect_is(ttops, "sf")
  expect_true(is.integer(ttops$treeID))
  expect_equal(dim(ttops), c(204,3))
  expect_true(sf::st_crs(ttops) == sf::st_crs(chm))
})

test_that("locate_trees LMF works with a stars ", {

  chm = rasterize_canopy(las, 1, p2r(0.15), pkg = "stars")

  ttops = locate_trees(chm, lmf(5))

  expect_is(ttops, "sf")
  expect_equal(dim(ttops), c(167,3))
  expect_true(sf::st_crs(ttops) == sf::st_crs(chm))

  # variable windows size
  f = function(x) { x * 0.07 + 3 }
  ttops = locate_trees(chm, lmf(f))

  expect_is(ttops, "sf")
  expect_true(is.integer(ttops$treeID))
  expect_equal(dim(ttops), c(204,3))
  expect_true(sf::st_crs(ttops) == sf::st_crs(chm))
})

test_that("locate_trees LMF works with a LAScatalog", {

  ttops = locate_trees(ctg, lmf(5))

  expect_is(ttops, "sf")
  expect_true(is.integer(ttops$treeID))
  expect_equal(dim(ttops), c(177,3))
  expect_true(sf::st_crs(ttops) == st_crs(ctg))
})

test_that("locate_trees supports different unicity strategies", {

  ttops = locate_trees(las, lmf(5), uniqueness = "gpstime")

  expect_true(is.double(ttops$treeID))
  expect_equal(mean(ttops$treeID), 151436.2, tol = 0.01)

  ttops = locate_trees(las, lmf(5), uniqueness = "bitmerge")

  expect_true(is.double(ttops$treeID))
  expect_equal(mean(ttops$treeID), 2.067186e17, tolerance = 1e-5)
})

