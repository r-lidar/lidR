context("lastrees")

LASfile <- system.file("extdata", "MixedConifer.laz", package = "lidR")
las = readLAS(LASfile, select = "xyzr", filter = "-drop_z_below 0")

chm = grid_tincanopy(las, res = 0.5)
chm = as.raster(chm)
kernel = matrix(1,3,3)
chm = raster::focal(chm, w = kernel, fun = mean)


test_that("Dalponte's methods works", {
  ttops = tree_detection(chm, 3, 2)
  lastrees_dalponte(las, chm, ttops)
  expect_true("treeID" %in% names(las@data))
})

test_that("Li's method works", {
  las@data[, treeID := NULL]

  lastrees_li(las, R = 5)
  expect_true("treeID" %in% names(las@data))
  expect_true(all(!is.na(las@data$treeID)))
})

test_that("Silvas's methods works", {
  las@data[, treeID := NULL]

  ttops = tree_detection(las, 3, 2)
  SPDF = lastrees_silva(las, ttops, extra = TRUE)
  expect_true(is(SPDF, "SpatialPolygonsDataFrame"))
  expect_true("treeID" %in% names(las@data))
})

test_that("Watershed's methods works", {
  las@data[, treeID := NULL]

  lastrees_watershed(las, chm)
  expect_true("treeID" %in% names(las@data))
})


#plot(las, color = "treeID", colorPalette = pastel.colors(100))
