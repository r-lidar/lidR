context("catalog_retile")

lidr_options(interactive = FALSE)
folder <- system.file("extdata", "Megaplot.laz", package="lidR")
ctg = catalog(folder)
set_cores(ctg) <- 1
set_buffer(ctg) <- 0
set_tiling_size(ctg) = 80
set_progress(ctg) <- FALSE
set_alignment(ctg) <- c(684440, -30)
set_output_files(ctg) = paste0(tempfile(), "/test_{XLEFT}_{YBOTTOM}")

test_that("catalog reshape works", {
  ctg2 = catalog_retile(ctg)

  unlink(dirname(get_output_files(ctg)), recursive = T)

  expect_equal(sum(ctg@data$`Number of point records`), sum(ctg2@data$`Number of point records`))
  expect_equal(nrow(ctg2@data), 12)
})
