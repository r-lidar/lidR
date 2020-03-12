context("readLAS")

LASfile <- system.file("extdata", "example.laz", package = "rlas")
las <- readLAS(LASfile, select = "xyzrn")

test_that("readLAS works", {
  expect_is(las, "LAS")
})

test_that("readLAS returns a error for non-existing file", {
  expect_error(readLAS("unexistingfile.las"), regexp = "File does not exist")
})

test_that("read only XYZ attributes works", {
  expect_equal(names(las@data), c("X", "Y", "Z", "ReturnNumber", "NumberOfReturns"))
  expect_equal(ncol(las@data), 5)
})

test_that("read multiple files works", {
  las2 = readLAS(rep(LASfile, 3))
  expect_equal(3*dim(las@data)[1], dim(las2@data)[1])
})

test_that("read multiple files warn about incompatibilities", {

  LASfile1 <- system.file("extdata", "Megaplot.laz", package="lidR")
  LASfile2 <- system.file("extdata", "Topography.laz", package="lidR")

  expect_warning(readLAS(c(LASfile1, LASfile2)), " scale factors and are incompatible")
})

test_that("readLAS throw warning for invalid files", {
  las@header@PHB$`X scale factor` <- 0.01234
  las@header@PHB$`Y scale factor` <- 0.01234
  las@data$ReturnNumber[1] <- 2L
  f <- tempfile(fileext = ".las")
  rlas:::C_writer(f, as.list(las@header), las@data)
  las  <- suppressWarnings(readLAS(f))

  expect_warning(readLAS(f), "X scale factors")
  expect_warning(readLAS(f), "Y scale factors")
  expect_warning(readLAS(f), "return number")
  expect_equal(las@header@PHB$`X scale factor`, 0.01234)
  expect_equal(las@header@PHB$`Y scale factor`, 0.01234)
})

las1 = lidR:::lasgenerator(10, 1)
las2 = lidR:::lasgenerator(10, 2)
las3 = lidR:::lasgenerator(10, 3)
f1 = tempfile(fileext = ".las")
f2 = tempfile(fileext = ".las")
f3 = tempfile(fileext = ".las")
writeLAS(las1, f1)
writeLAS(las2, f2)
writeLAS(las3, f3)

test_that("read multispectral works", {
  las = readMSLAS(f1, f2, f3)
  expect_equal(dim(las@data), c(30, 16))
  expect_true("ScannerChannel" %in% names(las@data))
  expect_equal(las@header@PHB$`Version Minor`, 4L)
  expect_equal(las@header@PHB$`Point Data Format ID`, 6L)
})

test_that("readLAS LAScluster throw warning when select/filter is used", {

  LASfile <- system.file("extdata", "Megaplot.laz", package = "lidR")
  ctg <- readLAScatalog(LASfile, chunk_size = 100)
  cls = lidR:::catalog_makecluster(ctg)

  expect_warning(readLAS(cls[[4]], select = "i"), "Argument 'select' is not used")
  expect_warning(readLAS(cls[[4]], filter = "-drop_z_below 0"), "Argument 'filter' is not used")
})
