context("lasfilterduplicates")

LASfile <- system.file("extdata", "example.laz", package = "rlas")
las     <- readLAS(LASfile)

test_that("lasfilterduplicates works with LAS", {
   las@data <- rbind(las@data, las@data)
   las = lidR:::lasupdateheader(las)
   las = lasfilterduplicates(las)
   expect_equal(npoints(las), 30L)
})

#test_that("lasfilterduplicates works with LAScatalog", {
#
#})
