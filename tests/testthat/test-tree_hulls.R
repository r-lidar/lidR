context("tree_hulls")

LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
las = readLAS(LASfile, "0", filter = "-keep_xy 481270 3812930 481310 3812970")

test_that("tree_hulls works with convex hulls", {
  hulls = tree_hulls(las)

  expect_is(hulls, "SpatialPolygonsDataFrame")
  expect_equal(dim(hulls), c(length(unique(las$treeID))-1,1))
})

test_that("tree_hulls works with bbox hulls", {
  hulls = tree_hulls(las, "bbox")

  expect_is(hulls, "SpatialPolygonsDataFrame")
  expect_equal(dim(hulls), c(length(unique(las$treeID))-1,1))
})

test_that("tree_hulls works with concave hulls", {
  hulls = tree_hulls(las, "concave")

  expect_is(hulls, "SpatialPolygonsDataFrame")
  expect_equal(dim(hulls), c(length(unique(las$treeID))-1,1))
})

test_that("tree_hulls supports custom metrics", {
  hulls = tree_hulls(las, "bbox", func = ~max(Z))

  expect_is(hulls, "SpatialPolygonsDataFrame")
  expect_equal(dim(hulls), c(length(unique(las$treeID))-1,2))
})
