context("filter_duplicates")

las     <- example

test_that("filter_duplicates works with LAS", {
   las@data <- rbind(las@data, las@data)
   las = lidR:::lasupdateheader(las)
   las = filter_duplicates(las)
   expect_equal(npoints(las), 30L)
})

#test_that("filter_duplicates works with LAScatalog", {
#
#})
