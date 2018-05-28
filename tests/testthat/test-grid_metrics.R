context("grid_metrics")

las = lidR:::dummy_las(2000)

test_that("grid_metrics works", {
  x = grid_metrics(las, mean(Z))
  expect_equal(dim(x), c(25,3))
})

test_that("grid_metrics debug mode works", {
  lidr_options(debug = TRUE)
  expect_error(grid_metrics(las, LAD(Z)), "A single number")
})

test_that("grid_metrics return an error if splitline and no flightlineID", {
  expect_error(grid_metrics(las, mean(Z), splitlines = T))
})

las@data[, flightlineID := c(rep(1,500), rep(2,500))]

test_that("grid_metrics splitline work", {
  x = grid_metrics(las, mean(Z), splitlines = T)
  expect_equal(dim(x), c(50,4))
})

test_that("predefined metric set work", {
  las = lidR:::dummy_las(10000)
  las@data[, ScanAngle := runif(.N)]

  expect_error(grid_metrics(las, .stdmetrics_z), NA)
  expect_error(grid_metrics(las, .stdmetrics_i), NA)
  expect_error(grid_metrics(las, .stdmetrics_rn), NA)
  expect_error(grid_metrics(las, .stdmetrics_ctrl), NA)
})

test_that("grid_metric debug mode works", {
  las = lidR:::dummy_las(10000)
  las@data[, ScanAngle := runif(.N)]

  lidr_options(debug = TRUE)

  expect_error(grid_metrics(las, LAD(Z)), "A single number or a list of single number is expected")
  expect_error(grid_metrics(las, quantile(Z)), "A single number or a list of single number is expected")
})