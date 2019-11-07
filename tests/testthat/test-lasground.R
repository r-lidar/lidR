context("lasground")

file <- system.file("extdata", "Topography.laz", package="lidR")
las = readLAS(file, select = "xyzrn")
ctg = catalog(file)

opt_chunk_size(ctg) <- 160
ctg@chunk_options$alignment = c(273340, 5274340)
opt_chunk_buffer(ctg) <- 0
opt_progress(ctg) <- FALSE

ws = seq(3,21, 5)
th = seq(0.1, 2, length.out = length(ws))

mypmf = pmf(ws, th)
mycsf = csf(TRUE, 1, 1, time_step = 1)

test_that("lasground pmf works", {

  las <- lasground(las, mypmf)

  n = names(las@data)

  expect_true("Classification" %in% n)
  expect_equal(unique(las@data$Classification), c(1L, 2L))
  expect_equal(sum(las@data$Classification == 2L), 19383L)

  expect_error(lasground(ctg, mypmf), "buffer")

  opt_chunk_buffer(ctg) <- 30

  expect_error(lasground(ctg, mypmf), "output file")

  opt_output_files(ctg) <- paste0(tmpDir(), "file_{XLEFT}_{YBOTTOM}")

  ctg2 = lasground(ctg, mypmf)
  las2 = readLAS(ctg2)

  expect_equal(sum(las2@data$Classification == 2L), 19383L)
  expect_equal(nrow(las2@data), nrow(las@data))

  las@data[, Classification := NULL]
})

test_that("lasground csf works", {
  las <- lasground(las, mycsf)

  n = names(las@data)

  expect_true("Classification" %in% n)
  expect_equal(unique(las@data$Classification), c(2L, 1L))
  expect_equal(sum(las@data$Classification == 2L), 26715L)

  opt_output_files(ctg) <- paste0(tmpDir(), "file_{XLEFT}_{YBOTTOM}_ground")
  opt_chunk_buffer(ctg) <- 30

  ctg2 = lasground(ctg, mycsf)
  las2 = readLAS(ctg2)

  expect_equal(sum(las2@data$Classification == 2L), 26715L-862L)
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

test_that("lasground works with last_returns = TRUE but attribute not properly populated", {
  las <- lidR:::dummy_las(500)
  las@data$ReturnNumber <- 0
  las@data$Classification <- NULL
  las <- suppressWarnings(lasground(las, csf(), last_returns = TRUE))

  n = names(las@data)

  expect_true("Classification" %in% n)
  expect_equal(unique(las@data$Classification), c(1L, 2L))
  expect_equal(sum(las@data$Classification == 2L), 105L)
})

test_that("makeZhangParam works", {
  expect_error(util_makeZhangParam(), NA)
})

