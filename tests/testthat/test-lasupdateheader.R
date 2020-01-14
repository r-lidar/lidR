context("lasupdateheader")

las <- lidR:::dummy_las(100)

test_that("header bbox is updated", {
  Zm <- las@header@PHB$`Max Z`
  bbox <- las@bbox
  las@header@PHB$`Max Z` <- 5
  las@bbox <- matrix(0, 2, 2)

  las <- lidR:::lasupdateheader(las)

  expect_equal(las@header@PHB$`Max Z`, Zm)
  expect_equivalent(las@bbox, bbox)
})

test_that("header extrabyte is updated", {
  x = runif(100)

  las <- lasadddata(las, x, "x")
  expect_true("x" %in% names(las@data))
  expect_true(is.null(las@header@VLR$Extra_Bytes))


  las <- lasaddextrabytes(las, name = "x", desc = "test")
  expect_true("x" %in% names(las@data))
  expect_true(!is.null(las@header@VLR$Extra_Bytes))
  expect_equal(las@header@VLR$Extra_Bytes$`Extra Bytes Description`$x$data_type, 10L)

  las <- lasaddextrabytes_manual(las, name = "x", desc = "test", type = "int")
  expect_true("x" %in% names(las@data))
  expect_true(!is.null(las@header@VLR$Extra_Bytes))
  expect_equal(las@header@VLR$Extra_Bytes$`Extra Bytes Description`$x$data_type, 6L)
})

test_that("header extrabyte fails", {

  expect_error(lasaddextrabytes(las, name = "y", desc = "test"), "y is not an attribute of the LAS object.")
  expect_error(lasaddextrabytes_manual(las, name = "y", desc = "test", type = "uint"), "y is not an attribute of the LAS object.")
  x = logical(100)

  expect_error(lasaddextrabytes(las, x, name = "y", desc = "test"), "LAS format specifications do not allow storing of 'logical' extra bytes")
  expect_error(lasaddextrabytes_manual(las, x, name = "y", desc = "test", type = "uint"), "LAS format specifications do not allow storing of 'logical' extra bytes")
})
