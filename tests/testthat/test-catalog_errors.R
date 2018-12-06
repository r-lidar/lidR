context("catalog_errors")

file <- system.file("extdata", "Megaplot.laz", package = "lidR")
ctg = catalog(file)

opt_cores(ctg) <- 1
opt_chunk_size(ctg) <- 160
ctg@chunk_options$alignment = c(684750, 5017760)
opt_chunk_buffer(ctg) <- 0

test_that("functions throw error were needed", {
  expect_error(lasnormalize(ctg, tin()), "buffer")
  expect_error(lasground(ctg, csf()), "buffer")

  opt_chunk_buffer(ctg) <- 10

  expect_error(lasnormalize(ctg, tin()), "output file")
  expect_error(lasground(ctg, csf()), "output file")
  expect_error(lasfilterdecimate(ctg, homogenize(5)), "output file")
})
