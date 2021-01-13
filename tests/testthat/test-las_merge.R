context("las_merge")

las1 = random_10_points
las2 = random_10_points
las2 = las_rescale(las2, 0.1)
las3 = random_10_points
las4 = random_10_points
epsg(las4) <- 32619

test_that("rbind merge 2 las", {

  o <- rbind(las1, las3)

  expect_equal(npoints(o), 20)
  expect_equal(o@header@PHB[["X scale factor"]], 0.001)
  expect_equal(o@header@PHB[["Y scale factor"]], 0.001)
})

test_that("rbind warns about incompatibilities", {

  expect_warning(rbind(las1, las2), "different scales")

  o <- suppressWarnings(rbind(las1, las2))

  expect_equal(npoints(o), 20)
  expect_equal(o@header@PHB[["X scale factor"]], 0.001)
  expect_equal(o@header@PHB[["Y scale factor"]], 0.001)

  expect_warning(rbind(las2, las1), "different scales")

  o <- suppressWarnings(rbind(las2, las1))

  expect_equal(npoints(o), 20)
  expect_equal(o@header@PHB[["X scale factor"]], 0.1)
  expect_equal(o@header@PHB[["Y scale factor"]], 0.001)
})


test_that("rbind fails with different CRS", {

  expect_error(rbind(las1, las4), "Different CRS")
})
