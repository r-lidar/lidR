context("lasdetectshape")

LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
las <- readLAS(LASfile, filter = "-thin_with_grid 2", select = "c")

test_that("lasdetectshape works with shp_coplanar", {
  las <- lasdetectshape(las, shp_plane(k = 15), "Coplanar")

  cn <- names(las@data)

  expect_true("Coplanar" %in% cn)
  expect_true(is.logical(las@data$Coplanar))
  expect_equivalent(as.numeric(table(las$Coplanar)), c(11230, 1664))
})

test_that("filter argument works", {
  las <- lasdetectshape(las, shp_plane(k = 15), "Coplanar", filter = ~Classification != 2L)

  cn <- names(las@data)

  expect_true("Coplanar" %in% cn)
  expect_true(is.logical(las@data$Coplanar))
  expect_equivalent(as.numeric(table(las$Coplanar)), c(12572, 322))
  expect_error(lasdetectshape(las, shp_plane(k = 15), "Coplanar", filter = ~Intensity > 200), "'Intensity'")
})

