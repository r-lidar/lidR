context("LAS")

LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
las     <- readLAS(LASfile)

test_that("print a LAS object  works", {
  sink(tempfile())
  expect_error(summary(las), NA)
  sink(NULL)
})

test_that("build a LAS object from data only works", {
  data = data.frame(X = runif(10), Y = runif(10), Z = runif(10))
  las2 = LAS(data)

  expect_is(las2, "LAS")
  expect_equal(las2@header@PHB$`Version Minor`, 2L)
  expect_equal(las2@header@PHB$`Point Data Format ID`, 0L)
  expect_equal(las2@header@PHB$`Min X`, min(data$X))
  expect_equal(las2@header@PHB$`Min Y`, min(data$Y))
  expect_equal(las2@header@PHB$`Min Z`, min(data$Z))
  expect_equal(las2@header@PHB$`Max X`, max(data$X))
  expect_equal(las2@header@PHB$`Max Y`, max(data$Y))
  expect_equal(las2@header@PHB$`Max Z`, max(data$Z))

  data$gpstime   = runif(10, 0,100)
  data$Intensity = as.integer(runif(10, 0 ,100))
  las2 = LAS(data)

  expect_is(las2, "LAS")
  expect_equal(las2@header@PHB$`Version Minor`, 2L)
  expect_equal(las2@header@PHB$`Point Data Format ID`, 1L)
  expect_equal(las2@header@PHB$`Min X`, min(data$X))
  expect_equal(las2@header@PHB$`Min Y`, min(data$Y))
  expect_equal(las2@header@PHB$`Min Z`, min(data$Z))
  expect_equal(las2@header@PHB$`Max X`, max(data$X))
  expect_equal(las2@header@PHB$`Max Y`, max(data$Y))
  expect_equal(las2@header@PHB$`Max Z`, max(data$Z))

  data$R = as.integer(runif(10,0,2^16))
  data$G = as.integer(runif(10,0,2^16))
  data$B = as.integer(runif(10,0,2^16))
  las2 = LAS(data)

  expect_is(las2, "LAS")
  expect_equal(las2@header@PHB$`Version Minor`, 2L)
  expect_equal(las2@header@PHB$`Point Data Format ID`, 3L)
})

test_that("build a LAS object from data and header works", {
  data = data.frame(X = runif(10), Y = runif(10), Z = runif(10))
  las2 = LAS(data, las@header)

  expect_is(las2, "LAS")
  expect_equal(las2@header@PHB$`Version Minor`, las@header@PHB$`Version Minor`)
  expect_equal(las2@header@PHB$`Point Data Format ID`, las@header@PHB$`Point Data Format ID`)
  expect_equal(las2@header@PHB$`Min X`, min(data$X))
  expect_equal(las2@header@PHB$`Min Y`, min(data$Y))
  expect_equal(las2@header@PHB$`Min Z`, min(data$Z))
  expect_equal(las2@header@PHB$`Max X`, max(data$X))
  expect_equal(las2@header@PHB$`Max Y`, max(data$Y))
  expect_equal(las2@header@PHB$`Max Z`, max(data$Z))
})

test_that("build a LAS invalid LAS object works as expected", {
  data = data.frame(X = runif(10), Y = runif(10), Z = runif(10))
  data$R = as.integer(runif(10, 0, 2^16))
  data$G = as.integer(runif(10, 0, 2^16))
  data$B = as.integer(runif(10, 0, 2^16))

  expect_warning(LAS(data, las@header), "point data format")

  header = las@header
  header@PHB$`Point Data Format ID` = 3L
  header@PHB$`X scale factor` = 0.412

  expect_warning(LAS(data, header), "X scale factor")

  data$R = as.integer(runif(10, 0, 2^8))
  data$G = as.integer(runif(10, 0, 2^8))
  data$B = as.integer(runif(10, 0, 2^8))

  header = las@header
  header@PHB$`Point Data Format ID` = 3L

  expect_warning(LAS(data, header), "8 bits")

  data$X = as.integer(runif(10, 1, 100))
  expect_error(LAS(data), "double")

  data$X = NULL
  expect_error(LAS(data), "missing coordinates")

  data$X = runif(10)
  data$R = NULL
  data$G = NULL
  data$B = NULL
  data$ReturnNumber = as.integer(rep(1, 10L))
  data$ReturnNumber[5] = 0L

  expect_warning(LAS(data), "return number")

  set.seed(0)
  data$ReturnNumber = as.integer(sample(1:3, 10, replace = TRUE))
  set.seed(1)
  data$NumberOfReturns = as.integer(sample(1:3, 10, replace = TRUE))

  expect_warning(LAS(data), "number of returns")

  header = las@header
  header@PHB$`Version Minor` <- NULL
  data = data.frame(X = runif(10), Y = runif(10), Z = runif(10))

  expect_error(LAS(data, header), "Version minor")
})

test_that("LAS redefined behavior of $, [, and [[", {

  las = lidR:::dummy_las(10)

  expect_true(is.numeric(las$Z))
  expect_equal(length(las$Z), 10)

  expect_true(is.null(las$U))

  expect_error(las$Z <- 1:10, "replace data of type double")
  expect_error(las$U <- 1:10, "Addition of a new column")

  expect_true(is.numeric(las[["Z"]]))
  expect_equal(length(las[["Z"]]), 10)

  expect_true(is.null(las[["U"]]))

  expect_error(las[["Z"]] <- 1:10, "replace data of type double")
  expect_error(las[["U"]] <- 1:10, "Addition of a new column")
})
