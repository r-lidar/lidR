context("lasupdateheader")

las <- random_10_points

test_that("header bbox is updated", {
  Zm <- las@header@PHB$`Max Z`
  bbox <- las@bbox
  las@header@PHB$`Max Z` <- 5
  las@bbox <- matrix(0, 2, 2)

  las <- lidR:::lasupdateheader(las)

  expect_equal(las@header@PHB$`Max Z`, Zm)
  expect_equivalent(las@bbox, bbox)
})
