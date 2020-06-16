context("filter_surfacepoints")

LASfile <- system.file("extdata", "Megaplot.laz", package = "lidR")
las     <-  readLAS(LASfile)
ctg     <-  catalog(LASfile)

test_that("filter_surfacepoints works", {

  opt_progress(ctg) <- FALSE
  opt_chunk_size(ctg) <- 180
  opt_output_files(ctg) <- paste0(tempdir(), "/file_{ID}")
  ctgdec <-  filter_surfacepoints(ctg, 1)

  lasdec  <- filter_surfacepoints(las, 1)
  lasdec2 <- readLAS(ctgdec)

  data.table::setorder(lasdec@data, gpstime, ReturnNumber)
  data.table::setorder(lasdec2@data, gpstime, ReturnNumber)

  expect_equal(nrow(lasdec@data), 44401L)
  expect_equal(lasdec@data, lasdec2@data)
})

test_that("filter_surfacepoints fails with unproper catalog options", {

  expect_error(filter_surfacepoints(ctg, 1), "output file")
})

