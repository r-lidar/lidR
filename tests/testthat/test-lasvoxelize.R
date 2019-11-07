context("lasvoxelize")

LASfile <- system.file("extdata", "Megaplot.laz", package = "lidR")
las     <-  readLAS(LASfile)
ctg     <-  catalog(LASfile)

test_that("lasfiltersurfacepoints works", {

  opt_progress(ctg) <- FALSE
  opt_chunk_size(ctg) <- 180
  opt_output_files(ctg) <- paste0(tempdir(), "/file_{ID}")
  ctgdec <-  lasvoxelize(ctg, 2)

  vox  <- lasvoxelize(las, 2)
  vox2 <- readLAS(ctgdec, select = "i")

  data.table::setorder(vox@data, X,Y,Z)
  data.table::setorder(vox2@data, X,Y,Z)

  expect_equal(nrow(vox@data), 43076L)
  expect_equal(vox@data, vox2@data)
})

test_that("lasfiltersurfacepoints fails with unproper catalog options", {

  expect_error(lasvoxelize(ctg, 1), "output file")
})

