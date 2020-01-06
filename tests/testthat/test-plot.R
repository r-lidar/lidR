context("plot")

LASfile <- system.file("extdata", "example.laz", package = "rlas")
las     <- readLAS(LASfile)
LASfile  <- system.file("extdata", "Megaplot.laz", package = "lidR")
ctg     <- readLAScatalog(LASfile)

test_that("plot LAS works", {
  expect_error(plot(las, axis = TRUE), NA)
  rgl::rgl.close()
})

test_that("plot LAS works with attributes", {

  las@data$treeID <- sample(1:3, npoints(las), T)
  las@data$R <- 255
  las@data$G <- 125
  las@data$B <- 125

  expect_error(plot(las, color = "Intensity"), NA)
  rgl::rgl.close()

  expect_error(plot(las, color = "Classification"), NA)
  rgl::rgl.close()

  expect_error(plot(las, color = "ScanAngleRank"), NA)
  rgl::rgl.close()

  expect_error(plot(las, color = "ReturnNumber"), NA)
  rgl::rgl.close()

  expect_error(plot(las, color = "Synthetic_flag"), NA)
  rgl::rgl.close()

  expect_error(plot(las, color = "treeID"), NA)
  rgl::rgl.close()

  expect_error(plot(las, color = "RGB", nbits = 8), NA)
  rgl::rgl.close()
})

test_that("plot LAS does not work with missing attributes", {
  expect_error(plot(las, color = "Plop"), "color' should refer to an attribute")
})

test_that("plot LAS works with artifact", {
  expect_error(plot(las, clear_artifact = FALSE), NA)
})

test_that("plot LAS does not works with 0 points", {
  expect_error(plot(lasfilter(las, Z > 1000)), "Cannot display an empty point cloud")
})

test_that("plot LAS checks the arguments", {
  expect_error(plot(las, color = c("Z", "Intensity")), "'color' should contain a single value")

  expect_error(plot(las, color = "RGB"), "No 'RGB' attributes found")
})

test_that("plot LASheader works", {
  expect_error(plot(las@header), NA)
})

test_that("plot LAScatalog works", {
  expect_error(plot(ctg), NA)

  ctg$process <- 1
  expect_warning(plot(ctg))
})

test_that("plot lasmetrics3d works", {
  x = voxel_metrics(las, ~length(Z), 5)
  expect_error(plot(x), NA)
  rgl::rgl.close()
})

test_that("plot dtm3 works", {
  x = grid_terrain(las, 1, knnidw())
  expect_error(plot_dtm3d(x), NA)
  rgl::rgl.close()
})

test_that("add dtm3d works", {
  x = grid_terrain(las, 1, knnidw())
  expect_error({y = plot(las) ; add_dtm3d(y, x)}, NA)
  rgl::rgl.close()
})

test_that("add treetop3d works", {
  x = tree_detection(las, lmf(3))
  expect_error({y = plot(las) ; add_treetops3d(y, x)}, NA)
  rgl::rgl.close()
})
