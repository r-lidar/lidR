context("colors")

test_that("set.colors attribute the proper color", {
  x = 1:10
  c = lidR:::set.colors(x, height.colors(50))

  expect_equal(length(unique(c)), length(x))

  x = rep(1,10)
  c = lidR:::set.colors(x, height.colors(50))

  expect_equal(c, "#0000FF")
})
