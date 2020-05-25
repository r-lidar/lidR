context("readLAS")

LASfile <- system.file("extdata", "example.laz", package = "rlas")
ctg <- readLAScatalog(LASfile, chunk_size = 100, select = "xyzrn")
cls <- lidR:::catalog_makecluster(ctg)

test_that("readLAS reads files", {
  las <- readLAS(LASfile, select = "xyzrn")

  expect_is(las, "LAS")
  expect_equal(npoints(las), 30L)
  expect_equal(names(las@data), c("X", "Y", "Z", "ReturnNumber", "NumberOfReturns"))
})

test_that("readLAS reads LAScatalog", {
  las <- readLAS(ctg)

  expect_is(las, "LAS")
  expect_equal(npoints(las), 30L)
  expect_equal(names(las@data), c("X", "Y", "Z", "ReturnNumber", "NumberOfReturns"))
})

test_that("readLAS reads LAScluster", {
  las <- readLAS(cls[[1]])

  expect_is(las, "LAS")
  expect_equal(npoints(las), 30L)
  expect_equal(names(las@data), c("X", "Y", "Z", "ReturnNumber", "NumberOfReturns", "buffer"))
  expect_true((all(las$buffer == 0)))
})

test_that("readLAS returns a error for non-existing file", {
  expect_error(readLAS("unexistingfile.las"), regexp = "File does not exist")
})

test_that("readLAS reads multiple files", {
  las2 = readLAS(rep(LASfile, 3))
  expect_equal(npoints(las2), 90)
})

test_that("readLAS reads multiple files and warn about incompatibilities", {

  LASfile1 <- system.file("extdata", "Megaplot.laz", package = "lidR")
  LASfile2 <- system.file("extdata", "Topography.laz", package = "lidR")

  expect_warning(readLAS(c(LASfile1, LASfile2)), " scale factors and are incompatible")
})

test_that("readLAS throws warning for invalid files", {
  las <- readLAS(LASfile, select = "xyzrn")
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

test_that("readLAS LAScluster throw warning when select/filter is used", {

  expect_warning(readLAS(cls[[1]], select = "i"), "Argument 'select' is not used")
  expect_warning(readLAS(cls[[1]], filter = "-drop_z_below 0"), "Argument 'filter' is not used")
})

test_that("readLAS LAScatalog gives precedence to argument select and filter", {

  opt_select(ctg) <- "xyz"
  opt_filter(ctg) <- "-keep_class 2"

  las = readLAS(ctg)

  expect_equal(names(las@data), c("X", "Y", "Z"))
  expect_equal(npoints(las), 3)

  las = readLAS(ctg, select = "i",  filter = "-drop_z_below 975")

  expect_equal(names(las@data), c("X", "Y", "Z", "Intensity"))
  expect_equal(npoints(las), 19)
})

test_that("readMSLAS reads multispectral data", {

  las1 = lidR:::lasgenerator(10, 1)
  las2 = lidR:::lasgenerator(10, 2)
  las3 = lidR:::lasgenerator(10, 3)
  f1 = tempfile(fileext = ".las")
  f2 = tempfile(fileext = ".las")
  f3 = tempfile(fileext = ".las")
  writeLAS(las1, f1)
  writeLAS(las2, f2)
  writeLAS(las3, f3)

  las = readMSLAS(f1, f2, f3)

  expect_equal(dim(las@data), c(30, 16))
  expect_true("ScannerChannel" %in% names(las@data))
  expect_equal(las@header@PHB$`Version Minor`, 4L)
  expect_equal(las@header@PHB$`Point Data Format ID`, 6L)
})

