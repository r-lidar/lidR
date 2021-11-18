context("classify_noise")

las = clip_rectangle(topography, 273450, 5274350, 273550, 5274450)

set.seed(314)
id = round(runif(20, 0, npoints(las)))
set.seed(42)
err = runif(20, -50, 50)
las$Z[id] = las$Z[id] + err

f = tempfile(fileext = ".las")
writeLAS(las, f)

ctg = readLAScatalog(f)
opt_chunk_size(ctg) <- 300
ctg@chunk_options$alignment = c(50, 200)
opt_chunk_buffer(ctg) <- 0
opt_progress(ctg) <- FALSE

mysor = sor(15,7)
myivf = ivf(5,2)

test_that("classify_noise sor works", {

  las <- classify_noise(las, mysor)

  n = names(las)

  expect_true("Classification" %in% n)
  expect_equal(sort(unique(las@data$Classification)), c(1L, 2L, 9L, 18L))
  expect_equal(sum(las@data$Classification == LASNOISE), 13L)

  expect_error(classify_noise(ctg, mysor), "buffer")

  opt_chunk_buffer(ctg) <- 30

  expect_error(classify_noise(ctg, mysor), "output file")

  opt_output_files(ctg) <- paste0(tempdir(), "/file_{XLEFT}_{YBOTTOM}")

  ctg2 = classify_noise(ctg, mysor)
  las2 = readLAS(ctg2)

  expect_equal(sum(las2@data$Classification == 18L), 13)
  expect_equal(nrow(las2@data), nrow(las@data))
})

test_that("classify_noise sor with quantiles", {

  las <- classify_noise(las, sor(15,0.999,TRUE))

  expect_equal(sum(las@data$Classification == 18L), 11L)
})

test_that("classify_noise ivf works", {

  las <- classify_noise(las, myivf)

  n = names(las)

  expect_true("Classification" %in% n)
  expect_equal(sort(unique(las@data$Classification)), c(1L, 2L, 9L, 18L))
  expect_equal(sum(las@data$Classification == LASNOISE), 12L)
})

