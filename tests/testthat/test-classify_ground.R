context("classify_ground")

las = clip_rectangle(topography, 273450, 5274350, 273550, 5274450)
ctg = topography_ctg

opt_chunk_size(ctg) <- 300
ctg@chunk_options$alignment = c(50, 200)
opt_chunk_buffer(ctg) <- 0
opt_progress(ctg) <- FALSE

ws = seq(3,21, 5)
th = seq(0.1, 2, length.out = length(ws))

mypmf = pmf(ws, th)
mycsf = csf(TRUE, 1, 1, time_step = 1)

test_that("classify_ground pmf works with LAS", {

  las <- classify_ground(las, mypmf)

  n = names(las)

  expect_true("Classification" %in% n)
  expect_equal(sort(unique(las@data$Classification)), c(1L, 2L))
  expect_equal(sum(las@data$Classification == 2L), 1933L)

  expect_error(classify_ground(ctg, mypmf), "buffer")
})

test_that("classify_ground pmf works with LAScatalog", {
  opt_chunk_buffer(ctg) <- 30

  expect_error(classify_ground(ctg, mypmf), "output file")

  opt_output_files(ctg) <- paste0(tempdir(), "/file_{XLEFT}_{YBOTTOM}")

  ctg2 = classify_ground(ctg, mypmf)
  las2 = readLAS(ctg2)

  expect_equal(sum(las2@data$Classification == 2L), 19472L)
})

test_that("classify_ground csf works with LAS", {
  las <- classify_ground(las, mycsf)

  n = names(las)

  expect_true("Classification" %in% n)
  expect_equal(sort(unique(las@data$Classification)), c(1L, 2L))
  expect_equal(sum(las@data$Classification == 2L), 2605L)
})

test_that("classify_ground csf works with LAScatalog", {
  skip_on_cran()

  opt_output_files(ctg) <- paste0(tempdir(), "/file_{XLEFT}_{YBOTTOM}_ground")
  opt_chunk_buffer(ctg) <- 30

  ctg2 = classify_ground(ctg, mycsf)
  las2 = readLAS(ctg2)

  expect_equal(sum(las2@data$Classification == 2L), 26715L-450L)
})

test_that("classify_ground csf works with last_returns = FALSE", {
  las <- lidR:::generate_las(500)
  las <- classify_ground(las, csf(), last_returns = FALSE)

  n = names(las)

  expect_true("Classification" %in% n)
  expect_equal(unique(las@data$Classification), c(1L, 2L))
  expect_equal(sum(las@data$Classification == 2L), 105L)
})

test_that("classify_ground works with last_returns = TRUE but attribute not properly populated", {
  las <- lidR:::generate_las(500)
  las@data$ReturnNumber <- 0
  las@data$Classification <- NULL
  las <- suppressWarnings(classify_ground(las, csf(), last_returns = TRUE))

  n = names(las)

  expect_true("Classification" %in% n)
  expect_equal(unique(las@data$Classification), c(1L, 2L))
  expect_equal(sum(las@data$Classification == 2L), 105L)
})

test_that("makeZhangParam works", {
  expect_error(util_makeZhangParam(), NA)
})

test_that("classify_ground does not erase former classification (but new ground points)", {

  las <- topography
  las <- filter_poi(las, X < mean(X), Y < mean(Y))
  las$Classification[las$Classification == LASGROUND] <- LASUNCLASSIFIED
  las <- classify_ground(las, mypmf)
  expect_equal(names(table(las$Classification)), c("1", "2", "9"))
})

