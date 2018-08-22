context("lasground")

file <- system.file("extdata", "Topography.laz", package="lidR")
las = readLAS(file, select = "xyzrn")
ctg = catalog(file)
cores(ctg) <- 1
tiling_size(ctg) <- 160
ctg@clustering_options$alignment = c(332550, 5238450)
buffer(ctg) <- 0
progress(ctg) <- FALSE

test_that("lasground pmf works", {
  ws = seq(3,21, 5)
  th = seq(0.1, 2, length.out = length(ws))

  lasground(las, "pmf", ws, th)

  n = names(las@data)

  expect_true("Classification" %in% n)
  expect_equal(unique(las@data$Classification), c(0L, 2L))
  expect_equal(sum(las@data$Classification == 2L), 13276L)

  expect_error(lasground(ctg, "pmf", ws, th), "buffer")

  buffer(ctg) <- 30

  expect_error(lasground(ctg, "pmf", ws, th), "output file")

  output_files(ctg) <- paste0(tmpDir(), "file_{XLEFT}_{YBOTTOM}")

  ctg2 = lasground(ctg, "pmf", ws, th)
  las2 = readLAS(ctg2)

  expect_equal(sum(las2@data$Classification == 2L), 13276L)
  expect_equal(nrow(las2@data), nrow(las@data))

  las@data[, Classification := NULL]
})

test_that("lasground csf works", {
  lasground(las, "csf")

  n = names(las@data)

  expect_true("Classification" %in% n)
  expect_equal(unique(las@data$Classification), c(0L, 2L))
  expect_equal(sum(las@data$Classification == 2L), 12836L)

  output_files(ctg) <- paste0(tmpDir(), "file_{XLEFT}_{YBOTTOM}_ground")
  buffer(ctg) <- 30

  ctg2 = lasground(ctg, "csf")
  las2 = readLAS(ctg2)

  expect_equal(sum(las2@data$Classification == 2L), 12168L)
  expect_equal(nrow(las2@data), nrow(las@data))
})

