context("plot")

LASfile <- system.file("extdata", "example.laz", package = "rlas")
las     <- readLAS(LASfile)
LASfile  <- system.file("extdata", "Megaplot.laz", package = "lidR")
ctg     <- readLAScatalog(LASfile)

test_that("plot LAS works", {
  expect_error(plot(las), NA)
  rgl::rgl.close()
})

test_that("plot LAS works with legend", {
  expect_error(plot(las, legend = TRUE, axis = TRUE), NA)
  rgl::rgl.close()
})

test_that("plot LAS works with artifact", {
  expect_error(plot(las, clear_artifact = FALSE), NA)
  rgl::rgl.close()
})

test_that("plot LASheader works", {
  expect_error(plot(las@header), NA)
})

test_that("plot LAScatalog works", {
  expect_error(plot(ctg), NA)
})

test_that("plot lasmetrics3d works", {
  x = grid_metrics3d(las, ~length(Z), 5)
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
