context("lasground")

file <- system.file("extdata", "Topography.laz", package="lidR")
las = readLAS(file, select = "xyzrn")
ctg = catalog(file)

opt_chunk_size(ctg) <- 160
ctg@chunk_options$alignment = c(332550, 5238450)
opt_chunk_buffer(ctg) <- 0
opt_progress(ctg) <- FALSE

test_that("lasground pmf works", {
  ws = seq(3,21, 5)
  th = seq(0.1, 2, length.out = length(ws))
  f  = pmf(ws, th)

  las <- lasground(las, f)

  n = names(las@data)

  expect_true("Classification" %in% n)
  expect_equal(unique(las@data$Classification), c(1L, 2L))
  expect_equal(sum(las@data$Classification == 2L), 13421L)

  expect_error(lasground(ctg, f), "buffer")

  opt_chunk_buffer(ctg) <- 30

  expect_error(lasground(ctg, f), "output file")

  opt_output_files(ctg) <- paste0(tmpDir(), "file_{XLEFT}_{YBOTTOM}")

  ctg2 = lasground(ctg, f)
  las2 = readLAS(ctg2)

  expect_equal(sum(las2@data$Classification == 2L), 13421L)
  expect_equal(nrow(las2@data), nrow(las@data))

  las@data[, Classification := NULL]
})

test_that("lasground csf works", {
  las <- lasground(las, csf())

  n = names(las@data)

  expect_true("Classification" %in% n)
  expect_equal(unique(las@data$Classification), c(1L, 2L))
  expect_equal(sum(las@data$Classification == 2L), 12836L)

  opt_output_files(ctg) <- paste0(tmpDir(), "file_{XLEFT}_{YBOTTOM}_ground")
  opt_chunk_buffer(ctg) <- 30

  ctg2 = lasground(ctg, csf())
  las2 = readLAS(ctg2)

  expect_equal(sum(las2@data$Classification == 2L), 12168L)
  expect_equal(nrow(las2@data), nrow(las@data))
})

test_that("lasground csf works with last_returns = FALSE", {
  las <- lidR:::dummy_las(500)
  las <- lasground(las, csf(), last_returns = FALSE)

  n = names(las@data)

  expect_true("Classification" %in% n)
  expect_equal(unique(las@data$Classification), c(1L, 2L))
  expect_equal(sum(las@data$Classification == 2L), 105L)
})

