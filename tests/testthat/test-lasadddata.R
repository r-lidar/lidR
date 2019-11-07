context("lasadddata")

LASfile <- system.file("extdata", "example.laz", package = "rlas")
las     <- readLAS(LASfile, "xyz")
x = runif(30)
k = sample(1:100, 30)

test_that("lasadddata works", {
   las2 = lasadddata(las, x, "plop")
   n = names(las2@data)

   expect_true("plop" %in% n)
   expect_equal(las@header, las2@header)
})

test_that("lasaddextrabytes works", {
  las2 = lasaddextrabytes(las, x, "plop", "extra attr")
  n = names(las2@data)

  expect_true("plop" %in% n)
  expect_true(!is.null(las2@header@VLR$Extra_Bytes))
  expect_true(!is.null(las2@header@VLR$Extra_Bytes$`Extra Bytes Description`$plop))
})

test_that("lasaddextrabytes manual works", {
  las2 = lasaddextrabytes_manual(las, x, "plop", "extra attr", "float")
  n = names(las2@data)

  expect_true("plop" %in% n)
  expect_true(!is.null(las2@header@VLR$Extra_Bytes))
  expect_true(!is.null(las2@header@VLR$Extra_Bytes$`Extra Bytes Description`$plop))
})

test_that("lasremoveextrabyes works", {
  las2 = lasaddextrabytes(las, x, "plop", "extra attr")
  las3 = lasremoveextrabytes(las2, "plop")

  expect_equal(las3@header, las@header)
})

