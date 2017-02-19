context("grid_hexametrics")

las = lidR:::dummy_las(2000)

test_that("grid_metrics works", {
  x = grid_hexametrics(las, mean(Z))
  expect_equal(dim(x), c(39,3))
})

test_that("grid_metrics space hexa properly", {
  x = grid_hexametrics(las, mean(Z))
  y = unique(diff(sort(unique(x$X))))
  expect_equal(length(y), 1)
  expect_equal(y, 10)
  expect_true(0 %in% x$X)
  expect_true(100 %in% x$X)
})

test_that("grid_metrics debug mode works", {
  expect_error(grid_hexametrics(las, LAD(Z)))
})

test_that("grid_metrics return an error if splitline and no flightlineID", {
  expect_error(grid_hexametrics(las, mean(Z), splitlines = T))
})

las@data[, flightlineID := c(rep(1,500), rep(2,500))]

test_that("grid_metrics splitline work", {
  x = grid_hexametrics(las, mean(Z), splitlines = T)
  expect_equal(dim(x), c(78,4))
})


