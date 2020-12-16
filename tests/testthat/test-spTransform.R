context("spTransform")

LASfile <- system.file("extdata", "Megaplot.laz", package = "lidR")
las <- readLAS(LASfile, select = "xyz")
crs <- sp::CRS(SRS_string = "EPSG:26918")

las2 <- spTransform(las, crs)

test_that("Datum tranformation works", {
  expect_equal(mean(las$X), 684879,  tol = 1)
  expect_equal(mean(las$Y), 5017900, tol = 1)
  expect_equal(mean(las2$X), 214383,  tol = 1)
  expect_equal(mean(las2$Y), 5021652, tol = 1)
})

test_that("Bounding box is updated", {
  expect_equivalent(bbox(las), matrix(c(684766.39, 5017773.08, 684993.29, 5018007.25), ncol = 2))
  expect_equivalent(bbox(las2), matrix(c(214261.66, 5021517.24, 214504.91, 5021767.46), ncol = 2), tol = 0.01)
})

test_that("Bounding header is updated", {
  expect_equivalent(unlist(las@header@PHB[c("Min X", "Min Y", "Max X", "Max Y")]), c(684766.39, 5017773.08, 684993.29, 5018007.25))
  expect_equivalent(unlist(las2@header@PHB[c("Min X", "Min Y", "Max X", "Max Y")]), c(214261.66, 5021517.24, 214504.91, 5021767.46), tol = 0.1)
})
