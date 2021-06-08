context("delineate_crowns")

las = clip_rectangle(mixedconifer, 481270, 3812930, 481310, 3812970)

test_that("delineate_crowns works with convex hulls", {
  hulls = delineate_crowns(las)

  expect_is(hulls, "SpatialPolygonsDataFrame")
  expect_equal(dim(hulls), c(length(unique(las$treeID))-1,4))
})

test_that("delineate_crowns works with bbox hulls", {
  hulls = delineate_crowns(las, "bbox")

  expect_is(hulls, "SpatialPolygonsDataFrame")
  expect_equal(dim(hulls), c(length(unique(las$treeID))-1,4))
})

test_that("delineate_crowns works with concave hulls", {

  # Added if in 3.0.1 because of some flavour misteriously
  # no longer have the package
  if (requireNamespace("concaveman", quietly = TRUE))
  {
    hulls = delineate_crowns(las, "concave")

    expect_is(hulls, "SpatialPolygonsDataFrame")
    expect_equal(dim(hulls), c(length(unique(las$treeID))-1,4))
  }
})

test_that("delineate_crowns returns NULL for no trees", {
  las@data$treeID <- NA_integer_
  expect_warning(delineate_crowns(las), "NULL returned")
  expect_true(is.null(suppressWarnings(delineate_crowns(las))))
})

test_that("delineate_crowns supports custom metrics", {
  hulls = delineate_crowns(las, "bbox", func = ~max(Z))

  expect_is(hulls, "SpatialPolygonsDataFrame")
  expect_equal(dim(hulls), c(length(unique(las$treeID))-1,5))
})

ctg = mixedconifer_ctg
opt_select(ctg) = "0"
opt_chunk_size(ctg) = 100
opt_chunk_alignment(ctg)  <- c(0,20)
opt_progress(ctg) <- FALSE

test_that("delineate_crowns works with a custom metrics", {
  hulls = delineate_crowns(ctg, func = ~max(Z))

  expect_is(hulls, "SpatialPolygonsDataFrame")
  expect_equal(dim(hulls), c(200,5))
})

