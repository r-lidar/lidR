context("lasgrid")

las = lidR:::dummy_las(5000)

test_that("grid in grid_canopy is appropriated", {
  x0 = grid_canopy(las, 0.5)
  x1 = grid_canopy(las, 1)
  x2 = grid_canopy(las, 2)

  expect_equal(min(x0$X), 0.25)
  expect_equal(min(x1$X), 0.5)
  expect_equal(min(x2$X), 1)
  expect_equal(max(x0$X), 99.75)
  expect_equal(max(x1$X), 99.5)
  expect_equal(max(x2$X), 99)

  expect_equal(min(x0$Y), 0.25)
  expect_equal(min(x1$Y), 0.5)
  expect_equal(min(x2$Y), 1)
  expect_equal(max(x0$Y), 99.75)
  expect_equal(max(x1$Y), 99.5)
  expect_equal(max(x2$Y), 99)
})

test_that("grid in grid_metric is appropriated", {
  x = grid_metrics(las, length(Z), 20)

  expect_equal(min(x$X), 10)
  expect_equal(max(x$X), 90)
  expect_equal(min(x$Y), 10)
  expect_equal(max(x$Y), 90)

  x = grid_metrics(las, length(Z), 10)

  expect_equal(min(x$X), 5)
  expect_equal(max(x$X), 95)
  expect_equal(min(x$Y), 5)
  expect_equal(max(x$Y), 95)

  x = grid_metrics(las, length(Z), 15.5)

  expect_equal(min(x$X), 7.75)
  expect_equal(max(x$X), 100.75)
  expect_equal(min(x$Y), 7.75)
  expect_equal(max(x$Y), 100.75)
})

test_that("grid in grid_metric is appropriated even with start", {
  x = grid_metrics(las, length(Z), 10, start = c(-5,-5))

  expect_equal(min(x$X), 0)
  expect_equal(max(x$X), 100)
  expect_equal(min(x$Y), 0)
  expect_equal(max(x$Y), 100)

  x = grid_metrics(las, length(Z), 20, start = c(-5,8))

  expect_equal(min(x$X), 5)
  expect_equal(max(x$X), 105)
  expect_equal(min(x$Y), -2)
  expect_equal(max(x$Y), 98)
})

test_that("make_grid make an appropriated grid", {

  x = grid_metrics(las, length(Z), 10)
  g = lidR:::make_grid(min(las@data$X), max(las@data$X), min(las@data$Y), max(las@data$X), 10)

  expect_equal(sum(x$X %in% g$X), length(x$X))

  x = grid_metrics(las, length(Z), 20)
  g = lidR:::make_grid(min(las@data$X), max(las@data$X), min(las@data$Y), max(las@data$X), 20)

  expect_equal(sum(x$X %in% g$X), length(x$X))

  x = grid_metrics(las, length(Z), 20, start = c(-5,10))
  g = lidR:::make_grid(min(las@data$X), max(las@data$X), min(las@data$Y), max(las@data$X), 20, start = c(-5,10))

  expect_equal(sum(x$X %in% g$X), length(x$X))

  x = grid_metrics(las, length(Z), 10, start = c(-5,8))
  g = lidR:::make_grid(min(las@data$X), max(las@data$X), min(las@data$Y), max(las@data$X), 10, start = c(-5,8))

  expect_equal(sum(x$X %in% g$X), length(x$X))

  x = grid_canopy(las, 2)
  g = lidR:::make_grid(min(las@data$X), max(las@data$X), min(las@data$Y), max(las@data$X), 2)

  x = grid_canopy(las, 1)
  g = lidR:::make_grid(min(las@data$X), max(las@data$X), min(las@data$Y), max(las@data$X), 1)

  expect_equal(sum(x$X %in% g$X), length(x$X))
})
