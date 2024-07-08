context("print")

las     <- example

LASfile <- system.file("extdata", "extra_byte.laz", package = "rlas")
las2     <- readLAS(LASfile)
las2@header@EVLR = las2@header@VLR

las3 <- random_10_points
las3@data$X <- las3@data$X * 1000
las3@data$Y <- las3@data$Y * 1000
las3 = las_update(las3)

folder = system.file("extdata", "", package = "lidR")
ctg = readLAScatalog(folder)

test_that("print works with LAS", {
  sink(tempfile())
  expect_error(print(las), NA)
  sink(NULL)
})

test_that("summary works with LAS", {
  sink(tempfile())
  expect_error(summary(las), NA)
  sink(NULL)
})

test_that("print works with LAS and extra bytes", {
  sink(tempfile())
  expect_error(print(las2), NA)
  sink(NULL)
})

test_that("print works with LAS and extra bytes", {
  sink(tempfile())
  expect_error(print(las3), NA)
  sink(NULL)
})

test_that("summary works with LAS and extra bytes", {
  sink(tempfile())
  expect_error(summary(las2), NA)
  sink(NULL)
})

test_that("print works with LAScatalog", {
  sink(tempfile())
  expect_error(show(ctg), NA)
  expect_error(summary(ctg), NA)
  sink(NULL)
})

test_that("print works with LAScluster", {
  sink(tempfile())
  cl = engine_chunks(ctg)[[1]]
  expect_error(show(cl), NA)
  sink(NULL)
})

test_that("print works with lidR algorithms ", {

  f <- lmf(2)
  g <- shp_plane()
  h <- li2012()
  k <- tin()
  j <- p2r(2)
  l <- random(2)
  m <- pmf(3,4)
  #o <- wing2015()
  p <- Roussel2020()

  sink(tempfile())
  expect_error(print(f), NA)
  expect_error(print(g), NA)
  expect_error(print(h), NA)
  expect_error(print(k), NA)
  expect_error(print(j), NA)
  expect_error(print(l), NA)
  expect_error(print(m), NA)
  #expect_error(print(o), NA)
  expect_error(print(p), NA)
  sink(NULL)

})

test_that("print raster_template works", {
  sink(tempfile())
  tpl <- lidR:::raster_layout(megaplot, 10)
  expect_error(print(tpl), NA)
  sink(NULL)
})


