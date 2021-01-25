context("catalog_laxindex")

test_that("catalog_laxindex works", {
  skip_on_cran() # ASAN/USBAN to fix in rlas

  ctg <- random_2files_250points
  expect_error(lidR:::catalog_laxindex(ctg), NA)
  expect_true(is.indexed(ctg))
})
