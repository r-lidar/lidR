context("lastrees")

LASfile <- system.file("extdata", "MixedConifer.laz", package = "lidR")
las = readLAS(LASfile, select = "xyzr", filter = "-drop_z_below 0 -keep_xy 481250 3812980 481300 3813030")

chm = grid_tincanopy(las, res = 0.5)
chm = as.raster(chm)
kernel = matrix(1,3,3)
chm = raster::focal(chm, w = kernel, fun = mean)


test_that("Dalponte's methods works", {
  ttops = suppressWarnings(tree_detection_lmf(chm, 3, 2))
  seg1 = lastrees_dalponte(las, chm, ttops, extra = T)

  expect_true(is(seg1, "RasterLayer"))
  expect_true("treeID" %in% names(las@data))
  expect_equal(sort(unique(seg1[])), 1:39)

  # Test if a seed is not in the chm
  old = ttops@coords
  ttops@coords[1,1] <- 0
  expect_warning(lastrees_dalponte(las, chm, ttops, extra = T), "outside")

  # Test if seed IDs are propagated
  ttops@coords <- old
  ttops@data$treeID = 1:39*2

  seg3 = lastrees_dalponte(las, chm, ttops, extra = T)
  expect_equal(sort(unique(seg3[])), 1:39*2)
})

test_that("Li's method works", {
  las@data[, treeID := NULL]

  lastrees_li2(las, speed_up = 5)
  expect_true("treeID" %in% names(las@data))
})

test_that("Silvas's methods works", {
  las@data[, treeID := NULL]

  ttops = suppressWarnings(tree_detection_lmf(chm, 3, 2))
  seg1 = lastrees_silva(las, chm, ttops, extra = TRUE)

  expect_true(is(seg1, "RasterLayer"))
  expect_true("treeID" %in% names(las@data))
})

test_that("lastrees can store in a user defined column", {
  lastrees_li2(las, speed_up = 5, field = "plop")
  expect_true("plop" %in% names(las@data))
})


test_that("tree_metrics works", {
  X = tree_metrics(las, max(Z))
  Y = tree_metrics(las, max(Z), field = "plop")
  expect_error(tree_metrics(las, max(Z), field = "abc"), "trees are not segmented")
})



# Commented because CRAN doesn't like to call Bioconductor package
# test_that("Watershed's methods works", {
#   las@data[, treeID := NULL]
#
#   lastrees_watershed(las, chm)
#   expect_true("treeID" %in% names(las@data))
# })
