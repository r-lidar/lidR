context("test-catalog")

folder <- system.file("extdata", "", package="lidR")

test_that("build catalog works", {
  ctg = catalog(folder)
  expect_equal(dim(ctg)[1], 3)
  expect_equal(dim(ctg)[2], 34)
})

test_that("catalog queries works", {
  catalog_options(multicore = 1)

  x = c(684850, 684880)
  y = c(5017850, 5017880)
  r = 20
  n = c("plot1", "pouik2")
  ctg = catalog(folder)
  req = catalog_queries(ctg, x, y, r, roinames = n)

  expect_equal(length(req), 2)
  expect_true(is(req[[1]], "LAS"))
  expect_true(is(req[[2]], "LAS"))
  expect_equal(diff(range(req[[1]]@data$X)), 2*r, tolerance = .5)
  expect_equal(diff(range(req[[1]]@data$Y)), 2*r, tolerance = .5)
})