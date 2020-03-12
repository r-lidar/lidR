context("lasadddata")

LASfile <- system.file("extdata", "example.laz", package = "rlas")
las     <- readLAS(LASfile, "xyz")
x = runif(30)
k = sample(1:100, 30)
n = npoints(las)
R = sample(1:255, n, TRUE)
G = sample(1:255, n, TRUE)
B = sample(1:255, n, TRUE)

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

test_that("add_lasrgb works", {
  las2 = add_lasrgb(las, R, G, B)

  expect_equal(las2@header@PHB$`Point Data Format ID`, 2L)
  expect_gt(mean(las2$R), 255)
})

