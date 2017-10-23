context("lastrees")

LASfile <- system.file("extdata", "MixedConifer.laz", package = "lidR")
las = readLAS(LASfile, select = "xyz", filter = "-drop_z_below 0")

chm = grid_canopy(las, res = 0.5, subcircle = 0.2, na.fill = "knnidw", k = 4)
chm = as.raster(chm)
kernel = matrix(1,3,3)
chm = raster::focal(chm, w = kernel, fun = mean)

test_that("Dalponte's's methods works", {
  lastrees_dalponte(las, chm, th = 5)
  expect_true("treeID" %in% names(las@data))
})

test_that("Li's method works", {
  lastrees_li(las, R = 5)
  expect_true("treeID" %in% names(las@data))
  expect_true(all(!is.na(las@data$treeID)))
})