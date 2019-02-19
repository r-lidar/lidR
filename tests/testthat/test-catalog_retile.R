context("catalog_retile")

options(lidR.interactive = FALSE)
folder <- system.file("extdata", "Megaplot.laz", package="lidR")
ctg <- catalog(folder)

opt_chunk_buffer(ctg)    <- 0
opt_chunk_size(ctg)      <- 80
opt_progress(ctg)        <- FALSE
opt_chunk_alignment(ctg) <- c(684440, 90)
opt_output_files(ctg)    <- paste0(tempfile(), "/test_{XLEFT}_{YBOTTOM}")

test_that("catalog_retile preserves the number of points", {
  ctg2 <- catalog_retile(ctg)

  unlink(dirname(opt_output_files(ctg)), recursive = T)

  expect_equal(sum(ctg@data$`Number of point records`), sum(ctg2@data$`Number of point records`))
  expect_equal(nrow(ctg2@data), 9L)
})
