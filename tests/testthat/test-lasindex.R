test_that("catalog_laxindex works", {

  ctg <- lidR:::catalog_generator(20)
  expect_error(lidR:::catalog_laxindex(ctg), NA)
  expect_true(is.indexed(ctg))
})
