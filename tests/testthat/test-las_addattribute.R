context("add_attribute")

las     <- example
las@data <- lidR:::coordinates3D(las)
x = runif(30)
k = sample(1:100, 30)
n = npoints(las)
R = sample(1:255, n, TRUE)
G = sample(1:255, n, TRUE)
B = sample(1:255, n, TRUE)

test_that("add_attribute works", {
   las2 = add_attribute(las, x, "plop")
   n = names(las2@data)

   expect_true("plop" %in% n)
   expect_equal(las@header, las2@header)
})

test_that("add_lasattribute works", {
  las2 = add_lasattribute(las, x, "plop", "extra attr")
  n = names(las2@data)

  expect_true("plop" %in% n)
  expect_true(!is.null(las2@header@VLR$Extra_Bytes))
  expect_true(!is.null(las2@header@VLR$Extra_Bytes$`Extra Bytes Description`$plop))
})

test_that("add_lasattribute manual works", {
  las2 = add_lasattribute_manual(las, x, "plop", "extra attr", "float")
  n = names(las2@data)

  expect_true("plop" %in% n)
  expect_true(!is.null(las2@header@VLR$Extra_Bytes))
  expect_true(!is.null(las2@header@VLR$Extra_Bytes$`Extra Bytes Description`$plop))
})

test_that("lasremoveextrabyes works", {
  las2 = add_lasattribute(las, x, "plop", "extra attr")
  las3 = remove_lasattribute(las2, "plop")

  expect_equal(las3@header, las@header)
})

test_that("add_lasrgb works", {
  las2 = add_lasrgb(las, R, G, B)

  expect_equal(las2@header@PHB$`Point Data Format ID`, 2L)
  expect_gt(mean(las2$R), 255)
})

test_that("header extrabyte is updated", {
  x = runif(n)

  las <- add_attribute(las, x, "x")
  expect_true("x" %in% names(las))
  expect_true(is.null(las@header@VLR$Extra_Bytes))


  las <- add_lasattribute(las, name = "x", desc = "test")
  expect_true("x" %in% names(las))
  expect_true(!is.null(las@header@VLR$Extra_Bytes))
  expect_equal(las@header@VLR$Extra_Bytes$`Extra Bytes Description`$x$data_type, 10L)

  las <- add_lasattribute_manual(las, name = "x", desc = "test", type = "int")
  expect_true("x" %in% names(las))
  expect_true(!is.null(las@header@VLR$Extra_Bytes))
  expect_equal(las@header@VLR$Extra_Bytes$`Extra Bytes Description`$x$data_type, 6L)
})

test_that("header extrabyte fails", {

  expect_error(add_lasattribute(las, name = "y", desc = "test"), "y is not an attribute of the LAS object.")
  expect_error(add_lasattribute_manual(las, name = "y", desc = "test", type = "uint"), "y is not an attribute of the LAS object.")

  x = logical(n)

  expect_error(add_lasattribute(las, x, name = "y", desc = "test"), "LAS format specifications do not allow storing of 'logical' extra bytes")
  expect_error(add_lasattribute_manual(las, x, name = "y", desc = "test", type = "uint"), "LAS format specifications do not allow storing of 'logical' extra bytes")
})

