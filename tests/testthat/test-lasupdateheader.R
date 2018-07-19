context("lasupdateheader")

test_that("header is updated by reference", {
  las = lidR:::dummy_las(100)

  Zm = las@header@PHB$`Max Z`
  las@header@PHB$`Max Z` = 5

  lidR:::lasupdateheader(las)

  expect_equal(las@header@PHB$`Max Z`, Zm)
})
