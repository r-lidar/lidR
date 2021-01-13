context("voxelize_points")

las     <-  megaplot
ctg     <-  megaplot_ctg

test_that("filter_surfacepoints works", {

  vox  <- voxelize_points(las, 4)

  expect_equal(npoints(vox), 13822L)
})

test_that("filter_surfacepoints works with a LAScatalog", {

  skip_on_cran()

  opt_progress(ctg) <- FALSE
  opt_chunk_size(ctg) <- 300
  opt_chunk_alignment(ctg)<- c(120, 100)
  opt_output_files(ctg) <- paste0(tempdir(), "/file_{ID}")

  ctgdec <-  voxelize_points(ctg, 4)
  expect_equal(npoints(ctgdec), 13822)
})

test_that("filter_surfacepoints fails with unproper catalog options", {

  expect_error(voxelize_points(ctg, 1), "output file")
})



