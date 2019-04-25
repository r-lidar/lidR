context("algorithm dispatch")

f <- lmf(2)
g <- shp_plane()
h <- li2012()
k <- tin()

las <- lidR:::dummy_las(10)

test_that("An error is thrown if algorithms are called in wrong context", {
  expect_error(f(), "The 'lmf' algorithm has not been called in the correct context.")
  expect_error(g(), "The 'shp_plane' algorithm has not been called in the correct context.")
  expect_error(h(), "The 'li2012' algorithm has not been called in the correct context.")
  expect_error(k(), "The 'tin' algorithm has not been called in the correct context.")

  expect_error(tree_detection(las, h), "The algorithm used is not an algorithm for individual tree detection.")
  expect_error(grid_terrain(las, 1, g), "The algorithm used is not an algorithm for spatial interpolation.")
  expect_error(lastrees(las, k), "The algorithm used is not an algorithm for individual tree segmentation.")
  expect_error(lasdetectshape(las, h), "The algorithm used is not an algorithm for shape detection.")
})

test_that("An error is thrown if the input is not an algorithm", {
  expect_error(tree_detection(las, mean), "Invalid function provided as algorithm.")
  expect_error(tree_detection(las, 3), "Invalid function provided as algorithm.")
  expect_error(lastrees(las, quantile), "Invalid function provided as algorithm.")
  expect_error(lasdetectshape(las, 12), "Invalid function provided as algorithm.")
})
