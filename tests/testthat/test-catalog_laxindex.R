context("catalog_laxindex")

test_that("catalog_laxindex works", {

  ctg <- random_2files_250points
  expect_error(lidR:::catalog_laxindex(ctg), NA)
  expect_true(is.indexed(ctg))
})
