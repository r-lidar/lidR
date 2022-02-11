context("plot")

las     <- example
ctg     <- megaplot_ctg

test_that("plot LAS works", {
  skip_on_cran()

  expect_error(plot(las), NA)
  rgl::rgl.close()
})

test_that("plot LAS works with attributes", {

  skip_on_cran()

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
  skip_on_cran()
  expect_error(plot(las, clear_artifact = FALSE), NA)
})

test_that("plot LAS does not works with 0 points", {
  expect_error(plot(filter_poi(las, Z > 1000)), "Cannot display an empty point cloud")
})

test_that("plot LAS checks the arguments", {
  expect_error(plot(las, color = c("Z", "Intensity")), "'color' should contain a single value")

  expect_error(plot(las, color = "RGB"), "No 'RGB' attributes found")
})

test_that("plot LASheader works", {
  expect_error(plot(las@header), NA)
})

test_that("Plot LAScatalog object works", {
  expect_error(plot(ctg), NA)
  expect_error(plot(ctg, chunk = TRUE), NA)
  expect_error(plot(ctg, col = "red"), NA)
  expect_error(plot(ctg@data["Min.Z"]), NA)

  skip_on_cran()
  skip_on_os("windows")
  expect_error(plot(ctg, mapview = TRUE), NA)
})

test_that("plot lasmetrics3d works", {
  skip_on_cran()
  x = voxel_metrics(las, ~length(Z), 5)
  expect_error(plot(x), NA)
  rgl::rgl.close()
})

test_that("plot dtm3 works", {
  skip_on_cran()
  x = grid_terrain(las, 1, knnidw())
  expect_error(plot_dtm3d(x), NA)
  rgl::rgl.close()
})

test_that("add dtm3d works", {
  skip_on_cran()
  x = grid_terrain(las, 1, knnidw())
  expect_error({y = plot(las) ; add_dtm3d(y, x)}, NA)
  rgl::rgl.close()
})

test_that("add treetop3d works", {
  skip_on_cran()
  x = locate_trees(las, lmf(3))
  expect_error({y = plot(las) ; add_treetops3d(y, x)}, NA)
  rgl::rgl.close()
})

test_that("add = x overlay a second point cloud", {
  skip_on_cran()
  gnd = filter_poi(las, Classification == LASGROUND)
  ngnd = filter_poi(las, Classification != LASGROUND)

  x = plot(ngnd)
  expect_error(plot(gnd, color = "Classification", add = x), NA)
  rgl::rgl.close()

})

test_that("plot voxels", {
  skip_on_cran()

  v <- voxel_metrics(example, func = ~length(Z))

  expect_error(plot(v, voxel = TRUE), NA)
  rgl::rgl.close()

  expect_error(plot(v, voxel = 2), NA)
  rgl::rgl.close()

  attr(v, "res") = NULL

  expect_error(plot(v, voxel = TRUE), NA)
  rgl::rgl.close()
})

