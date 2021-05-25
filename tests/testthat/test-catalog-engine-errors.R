context("catalog_errors")

ctg  <- megaplot_ctg
opt_progress(ctg) <- FALSE

opt_chunk_size(ctg) <- 160
ctg@chunk_options$alignment <- c(684750, 5017760)
opt_chunk_buffer(ctg) <- 0

test_that("functions throw error were needed", {
  expect_error(normalize_height(ctg, tin()), "buffer")
  expect_error(classify_ground(ctg, csf()), "buffer")

  opt_chunk_buffer(ctg) <- 10

  expect_error(normalize_height(ctg, tin()), "output file")
  expect_error(classify_ground(ctg, csf()), "output file")
  expect_error(decimate_points(ctg, homogenize(5)), "output file")
})

test_that("User cannot use one of readXXXLAS in catalog_apply", {
  myfun <- function(cluster, ...) {
    las <- readTLSLAS(cluster)
    if (is.empty(las)) return(NULL)
    return(0)
  }

  expect_error(catalog_apply(megaplot_ctg, myfun), "Use readLAS")
})
