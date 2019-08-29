context("LAScatalog")

LASfile <- system.file("extdata", "Megaplot.laz", package = "lidR")
ctg     <- catalog(LASfile)

opt_chunk_size(ctg)      <- 140
opt_chunk_alignment(ctg) <- c(684760, 5017760)
opt_chunk_buffer(ctg)    <- 3

test_that("Print LAScatalog object works", {
  sink(tempfile())
  expect_error(print(ctg), NA)
  expect_error(summary(ctg), NA)
  sink(NULL)
})

test_that("Plot LAScatalog object works", {
  expect_error(plot(ctg), NA)
  expect_error(plot(ctg, chunk = TRUE), NA)
  expect_error(plot(ctg, mapview = TRUE), NA)
  expect_error(plot(ctg, col = "red"), NA)
  expect_error(spplot(ctg, "Min.Z"), NA)
})

test_that("LAScatalog redefined behavior of $, [, and [[", {

  expect_true(is.character(ctg$File.Signature))
  expect_equal(length(ctg$File.Signature), 1)

  expect_true(is.null(ctg$U))

  expect_error(ctg$File.Signature <- 1, "cannot be modified")

  expect_true(is.character(ctg[["File.Signature"]]))
  expect_equal(length(ctg[["File.Signature"]]), 1)

  expect_true(is.null(ctg[["U"]]))

  expect_error(ctg[1:2], "not allowed")
  expect_error(ctg[1:2,1:2], "not allowed")
})

test_that("LAScatalog conversion to SpatialPolygonsDataFrame works", {

  spctg <- as.spatial(ctg)

  expect_true(is(spctg, "SpatialPolygonsDataFrame"))
  expect_reference(ctg@data, spctg@data)
})


