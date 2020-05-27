context("print")

LASfile <- system.file("extdata", "example.laz", package = "rlas")
las     <- readLAS(LASfile)

LASfile <- system.file("extdata", "extra_byte.laz", package = "rlas")
las2     <- readLAS(LASfile)

las3 <- lidR:::dummy_las(50)
las3@data$X <- las3@data$X * 1000
las3@data$Y <- las3@data$Y * 1000

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
  cl = lidR:::catalog_makecluster(ctg)[[1]]
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
  m <- csf()
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

