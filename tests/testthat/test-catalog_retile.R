context("catalog_retile")

lidr_options(interactive = FALSE)
folder <- system.file("extdata", "Megaplot.laz", package="lidR")
ctg = catalog(folder)
cores(ctg) <- 1
buffer(ctg) <- 0
tiling_size(ctg) = 80
progress(ctg) <- FALSE

test_that("catalog reshape works", {
  temp = tempfile()

  ctg2 = catalog_retile(ctg, temp, prefix = "test_")

  unlink(temp, recursive = T)

  expect_equal(sum(ctg@data$`Number of point records`), sum(ctg2@data$`Number of point records`))
  expect_equal(nrow(ctg2@data), 16)
})
