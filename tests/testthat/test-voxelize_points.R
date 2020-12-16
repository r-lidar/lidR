context("voxelize_points")

LASfile <- system.file("extdata", "Megaplot.laz", package = "lidR")
las     <-  readLAS(LASfile)
ctg     <-  catalog(LASfile)

test_that("filter_surfacepoints works", {

  opt_progress(ctg) <- FALSE
  opt_chunk_size(ctg) <- 300
  opt_chunk_alignment(ctg)<- c(120, 100)
  opt_output_files(ctg) <- paste0(tempdir(), "/file_{ID}")
  ctgdec <-  voxelize_points(ctg, 4)

  vox  <- voxelize_points(las, 4)
  vox2 <- readLAS(ctgdec)

  data.table::setorder(vox@data, X,Y,Z)
  data.table::setorder(vox2@data, X,Y,Z)

  expect_equal(nrow(vox@data), 13822L)
  expect_equal(vox@data, vox2@data)
})

test_that("filter_surfacepoints fails with unproper catalog options", {

  expect_error(voxelize_points(ctg, 1), "output file")
})

