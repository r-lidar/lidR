context("spTransform")

LASfile <- system.file("extdata", "Megaplot.laz", package = "lidR")
las <- readLAS(LASfile, select = "xyz")

crs  <- as(sf::st_crs(26918), 'CRS')
las2 <- sf::st_transform(las, crs)

test_that("Datum tranformation works", {

  expect_equal(mean(las$X), 684879,  tol = 1)
  expect_equal(mean(las$Y), 5017900, tol = 1)
  expect_equal(mean(las2$X), 214383,  tol = 1)
  expect_equal(mean(las2$Y), 5021652, tol = 1)
})

test_that("Bounding box is updated", {

  expect_equivalent(as.numeric(st_bbox(las)), c(684766.39, 5017773.08, 684993.29, 5018007.25))
  expect_equivalent(as.numeric(st_bbox(las2)), c(214261.66, 5021517.24, 214504.91, 5021767.46), tol = 0.01)
})

test_that("Bounding header is updated", {

  expect_equivalent(unlist(las@header@PHB[c("Min X", "Min Y", "Max X", "Max Y")]), c(684766.39, 5017773.08, 684993.29, 5018007.25))
  expect_equivalent(unlist(las2@header@PHB[c("Min X", "Min Y", "Max X", "Max Y")]), c(214261.66, 5021517.24, 214504.91, 5021767.46), tol = 0.1)
})
