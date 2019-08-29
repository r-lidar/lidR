context("lasupdateheader")

las <- lidR:::dummy_las(100)

test_that("header is updated by reference", {
  Zm <- las@header@PHB$`Max Z`
  bbox <- las@bbox
  las@header@PHB$`Max Z` <- 5
  las@bbox <- matrix(0, 2, 2)

  las <- lidR:::lasupdateheader(las)

  expect_equal(las@header@PHB$`Max Z`, Zm)
  expect_equivalent(las@bbox, bbox)
})
