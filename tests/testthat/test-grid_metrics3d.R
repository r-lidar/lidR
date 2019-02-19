context("grid_metrics3d")

las <- lidR:::dummy_las(500)

test_that("grid_metrics3d returns a named data.frame", {

  x <- grid_metrics3d(las, list(Imean = mean(Intensity)), 5)

  expect_true(is(x, "data.frame"))
  expect_equal(names(x)[4], "Imean")
})

test_that("grid_metrics3d accepts both an expression or a formula", {

  x <- grid_metrics3d(las,  list(Imean = mean(Intensity)), 5)
  y <- grid_metrics3d(las, ~list(Imean = mean(Intensity)), 5)

  expect_equal(x, y)
})
