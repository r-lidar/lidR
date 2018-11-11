context("writeLAS")

LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
i = readLAS(LASfile)
ofile = paste0(tempfile(), ".las")

test_that("Test if I/O are equal", {
  writeLAS(i, ofile)
  o = readLAS(ofile)

  # Because those field are expepected to be different
  i@header@PHB["Generating Software"] <- NULL
  o@header@PHB["Generating Software"] <- NULL

  i@header@PHB["System Identifier"] <- NULL
  o@header@PHB["System Identifier"] <- NULL

  i@header@PHB["Number of variable length records"] <- NULL
  o@header@PHB["Number of variable length records"] <- NULL

  i@header@PHB["Offset to point data"] <- NULL
  o@header@PHB["Offset to point data"] <- NULL

  expect_equal(i@data, o@data)
  expect_equal(i@header@PHB, o@header@PHB)
})

