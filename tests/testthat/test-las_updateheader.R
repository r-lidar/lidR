context("lasupdateheader")

las <- random_10_points

test_that("header bbox is updated", {
  Zm <- las@header@PHB$`Max Z`
  las@header@PHB$`Max Z` <- 5

  las <- las_update(las)

  expect_equal(las@header@PHB$`Max Z`, Zm)
})
