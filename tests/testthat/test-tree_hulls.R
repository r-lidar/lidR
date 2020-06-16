context("tree_hulls")

LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
las = readLAS(LASfile, "0", filter = "-keep_xy 481270 3812930 481310 3812970")

test_that("tree_hulls works with convex hulls", {
  hulls = tree_hulls(las)

  expect_is(hulls, "SpatialPolygonsDataFrame")
  expect_equal(dim(hulls), c(length(unique(las$treeID))-1,4))
})

test_that("tree_hulls works with bbox hulls", {
  hulls = tree_hulls(las, "bbox")

  expect_is(hulls, "SpatialPolygonsDataFrame")
  expect_equal(dim(hulls), c(length(unique(las$treeID))-1,4))
})

test_that("tree_hulls works with concave hulls", {

  # Added if in 3.0.1 because of some flavour misteriously
  # no longer have the package
  if (requireNamespace("concaveman", quietly = TRUE))
  {
    hulls = tree_hulls(las, "concave")

    expect_is(hulls, "SpatialPolygonsDataFrame")
    expect_equal(dim(hulls), c(length(unique(las$treeID))-1,4))
  }
})

test_that("tree_hulls supports custom metrics", {
  hulls = tree_hulls(las, "bbox", func = ~max(Z))

  expect_is(hulls, "SpatialPolygonsDataFrame")
  expect_equal(dim(hulls), c(length(unique(las$treeID))-1,5))
})

LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
ctg = readLAScatalog(LASfile)
opt_select(ctg) = "0"
opt_chunk_size(ctg) = 100
opt_chunk_alignment(ctg)  <- c(0,20)
opt_progress(ctg) <- FALSE

test_that("tree_hulls works with a custom metrics", {
  hulls = tree_hulls(ctg, func = ~max(Z))

  expect_is(hulls, "SpatialPolygonsDataFrame")
  expect_equal(dim(hulls), c(200,5))
})
