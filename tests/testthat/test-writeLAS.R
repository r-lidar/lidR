context("writeLAS")

LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
i = readLAS(LASfile)
ofile = paste0(tempfile(), ".las")

test_that("Test if I/O are equal", {
  writeLAS(i, ofile)
  o = readLAS(ofile)

  # Because those field are expepected to be different
  i@header@data["Generating Software"] <- NULL
  o@header@data["Generating Software"] <- NULL

  expect_equal(i@data, o@data)
  expect_equal(i@header, o@header)
})

