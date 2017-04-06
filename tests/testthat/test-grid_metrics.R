context("grid_metrics")

las = lidR:::dummy_las(2000)

test_that("grid_metrics works", {
  x = grid_metrics(las, mean(Z))
  expect_equal(dim(x), c(25,3))
})

test_that("grid_metrics debug mode works", {
  expect_error(grid_metrics(las, LAD(Z), debug = TRUE))
})

test_that("grid_metrics return an error if splitline and no flightlineID", {
  expect_error(grid_metrics(las, mean(Z), splitlines = T))
})

las@data[, flightlineID := c(rep(1,500), rep(2,500))]

test_that("grid_metrics splitline work", {
  x = grid_metrics(las, mean(Z), splitlines = T)
  expect_equal(dim(x), c(50,4))
})