context("tree_metrics")

LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
las = readLAS(LASfile, select = "ia0", filter = "-drop_z_below 0")
ctg = catalog(LASfile)

opt_progress(ctg) <- FALSE
opt_chunk_size(ctg) <- 120
opt_chunk_buffer(ctg) <- 20

test_that("tree_metrics works with a LAS", {

  metrics = tree_metrics(las, list(Z = max(Z), `Mean Z` = mean(Z), `Max I` = max(Intensity)))

  expect_is(metrics, "SpatialPointsDataFrame")
  expect_equal(names(metrics@data), c("treeID", "Z", "Mean Z", "Max I"))
  expect_equal(dim(metrics@data), c(205,4))
  expect_equal(metrics@proj4string, las@proj4string)

  metrics = tree_metrics(las, .stdtreemetrics)

  expect_is(metrics, "SpatialPointsDataFrame")
  expect_equal(nrow(metrics@data), 205)
  expect_equal(metrics@proj4string, las@proj4string)
})

test_that("tree_metrics works with a LAScatalog", {

  metrics = tree_metrics(ctg, list(Z = max(Z), `Mean Z` = mean(Z), `Max I` = max(Intensity)))

  expect_is(metrics, "SpatialPointsDataFrame")
  expect_equal(names(metrics@data), c("treeID", "Z", "Mean Z", "Max I"))
  expect_equal(dim(metrics@data), c(205,4))
  expect_equal(metrics@proj4string, las@proj4string)

  metrics = tree_metrics(ctg, .stdtreemetrics)

  expect_is(metrics, "SpatialPointsDataFrame")
  expect_equal(nrow(metrics@data), 205)
  expect_equal(metrics@proj4string, las@proj4string)
})

LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
las = readLAS(LASfile, select = "ia0", filter = "-drop_z_below 0")
ctg = catalog(LASfile)

opt_progress(ctg) <- FALSE
opt_chunk_size(ctg) <- 120
opt_chunk_buffer(ctg) <- 20

test_that("tree_metrics throw an error if no treeID", {

  expect_error(tree_metrics(las, list(`Mean Z` = mean(Z))), "not segmented")
  expect_error(tree_metrics(ctg, list(`Mean Z` = mean(Z))), "not segmented")
})


