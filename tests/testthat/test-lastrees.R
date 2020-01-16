context("lastrees")

LASfile <- system.file("extdata", "MixedConifer.laz", package = "lidR")
las = readLAS(LASfile, select = "xyzr", filter = "-drop_z_below 0 -keep_xy 481250 3812980 481300 3813030")

chm = grid_canopy(las, 0.5, pitfree())
kernel = matrix(1,3,3)
chm = raster::focal(chm, w = kernel, fun = mean)


test_that("Dalponte's methods works", {
  ttops = suppressWarnings(tree_detection(chm, lmf(3, 2)))
  las <- lastrees(las, dalponte2016(chm, ttops))

  expect_true("treeID" %in% names(las@data))
  expect_equal(sort(unique(las@data$treeID)), 1:40L)
  expect_true(is.integer(las@data$treeID))

  # Test if a seed is not in the chm
  old = ttops@coords
  ttops@coords[1,1] <- 0
  expect_warning(lastrees(las, dalponte2016(chm, ttops)), "outside")

  # Test if seed IDs are propagated
  ttops@coords <- old
  ttops@data$treeID = 1:40*2L

  las <- lastrees(las, dalponte2016(chm, ttops))
  expect_equal(sort(unique(las@data$treeID)), 1:40L*2L)
})

test_that("Li's method works", {
  las <- lastrees(las, li2012(speed_up = 5), attribute = "TID")

  expect_true("TID" %in% names(las@data))
  expect_equal(sort(unique(las@data$TID)), 1:48L)
  expect_true(is.integer(las@data$TID))
})

test_that("Silvas's methods works", {
  ttops = suppressWarnings(tree_detection(chm, lmf(3, 2)))
  las <- lastrees(las, silva2016(chm, ttops))

  expect_true("treeID" %in% names(las@data))
  expect_true(is.integer(las@data$treeID))
})

# test_that("MC watershed methods works", {
#   ttops = suppressWarnings(tree_detection(chm, lmf(3, 2)))
#   las <- lastrees(las, mcwatershed(chm, ttops))
#
#   expect_true("treeID" %in% names(las@data))
#   expect_true(is.integer(las@data$treeID))
# })

test_that("lastrees can store in a user defined column", {
  las <- lastrees(las, li2012(speed_up = 5), attribute = "plop")
  expect_true("plop" %in% names(las@data))
})

test_that("tree_metrics works", {
  las <- lastrees(las, li2012(speed_up = 5), attribute = "plop")
  expect_error(tree_metrics(las, max(Z)), "not segmented")
  Y <- tree_metrics(las, max(Z), attribute = "plop")
  expect_error(tree_metrics(las, max(Z), attribute = "abc"), "trees are not segmented")
})

# Commented because CRAN doesn't like to call Bioconductor package
# test_that("Watershed's methods works", {
#   las@data[, treeID := NULL]
#
#   lastrees_watershed(las, chm)
#   expect_true("treeID" %in% names(las@data))
# })
