context("segment_shapes")

set.seed(1)
las <- decimate_points(megaplot, random(0.1))
las@data = las@data[,c(1:4, 10)]

test_that("segment_shapes works with shp_coplanar", {
  las <- segment_shapes(las, shp_plane(k = 15), "Coplanar")

  cn <- names(las@data)

  expect_true("Coplanar" %in% cn)
  expect_true(is.logical(las@data$Coplanar))
  expect_equivalent(as.numeric(table(las$Coplanar)), c(4980, 331))
})

test_that("segment_shapes works with shp_hcoplanar", {
  las <- segment_shapes(las, shp_hplane(k = 15), "Hcoplanar")

  cn <- names(las@data)

  expect_true("Hcoplanar" %in% cn)
  expect_true(is.logical(las@data$Hcoplanar))
  expect_equivalent(as.numeric(table(las$Hcoplanar)), c(4990, 321))
})


test_that("segment_shapes works with shp_hline", {
  las <- segment_shapes(las, shp_line(k = 8), "line")

  cn <- names(las@data)

  expect_true("line" %in% cn)
  expect_true(is.logical(las@data$line))
  expect_equivalent(as.numeric(table(las$line)), c(5269, 42))
})

test_that("filter argument works", {
  las <- segment_shapes(las, shp_plane(k = 15), "Coplanar", filter = ~Classification != 2L)

  cn <- names(las@data)

  expect_true("Coplanar" %in% cn)
  expect_true(is.logical(las@data$Coplanar))
  expect_equivalent(as.numeric(table(las$Coplanar)), c(5281, 30))
  expect_error(segment_shapes(las, shp_plane(k = 15), "Coplanar", filter = ~Intensity > 200), "'Intensity'")
})

