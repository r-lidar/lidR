context("decimate_points")

las = megaplot
ctg = megaplot_ctg

test_that("decimate_points homogenize algorithm works", {
  lasdec = decimate_points(las, homogenize(0.5,5))
  xdec = rasterize_density(lasdec, res = 5)

  expect_true(data.table::between(median(xdec[], na.rm = TRUE), 0.5-sd(xdec[], na.rm = TRUE), 0.5+sd(xdec[], na.rm = TRUE) ))
})

test_that("decimate_points random algorithm works", {
  lasdec = decimate_points(las, random(0.5))
  xdec = rasterize_density(lasdec, res = 5)

  expect_true(data.table::between(median(xdec[], na.rm = TRUE), 0.5-sd(xdec[], na.rm = TRUE), 0.5+sd(xdec[], na.rm = TRUE) ))
})

test_that("decimate_points highest algorithm works", {
  lasdec = decimate_points(las, highest(2))
  xdec = rasterize_density(lasdec, res = 5)
  zmean = mean(lasdec$Z)

  expect_true(data.table::between(median(xdec[], na.rm = TRUE), 0.23, 0.25))
  expect_equal(zmean, 16.25, tolerance = 0.01)
})

test_that("decimate_points lowest algorithm works", {
  lasdec = decimate_points(las, lowest(2))
  xdec = rasterize_density(lasdec, res = 5)
  zmean = mean(lasdec$Z)

  expect_true(data.table::between(median(xdec[], na.rm = TRUE), 0.23, 0.25))
  expect_equal(zmean, 7.42, tolerance = 0.01)
})

test_that("decimate_points fails with unproper catalog options", {

  expect_error(decimate_points(ctg, homogenize(0.8,5)), "output file")
})

test_that("decimate_points works with a LAScatalog", {

  ctg <- random_2files_250points
  opt_output_files(ctg) <- "{tempdir()}/{ID}"
  ctg2 <- decimate_points(ctg, highest(10))

  expect_is(ctg2, "LAScatalog")
  expect_gt(npoints(ctg), npoints(ctg2))
})

test_that("homogenize with complete_pulses keeps or removes whole pulses (#823)", {
  set.seed(1)
  seed <- .Random.seed
  dec  <- decimate_points(megaplot, homogenize(0.5, 5, use_pulse = TRUE, complete_pulses = TRUE))

  # The user's random number generator is not affected
  expect_identical(.Random.seed, seed)

  # Every kept pulse has all its returns
  n_in  <- megaplot@data[, .N, by = gpstime]
  n_out <- dec@data[, .N, by = gpstime]
  expect_equal(n_out$N, n_in[n_out, on = "gpstime"]$N)

  # At most n = 0.5 * 5^2 = 12 pulses per cell
  np <- pixel_metrics(dec, ~length(unique(gpstime)), 5, filter = ~ReturnNumber == NumberOfReturns)
  expect_equal(max(np[], na.rm = TRUE), 12)
})

test_that("homogenize with complete_pulses gives the same result with a LAScatalog (#823)", {
  ctg <- megaplot_ctg
  opt_progress(ctg) <- FALSE
  opt_chunk_size(ctg) <- 100
  opt_output_files(ctg) <- paste0(tempdir(), "/dec823_{ID}")

  alg <- homogenize(0.5, 5, use_pulse = TRUE, complete_pulses = TRUE)
  out <- unique(readLAS(decimate_points(ctg, alg))$gpstime)
  ref <- unique(decimate_points(megaplot, alg)$gpstime)

  # Points on the outer border of a catalog may be dropped by the catalog engine when a buffer is
  # used, independently of the decimation. Ignore the pulses touching the border.
  edge <- megaplot@data[X < min(X) + 0.2 | X > max(X) - 0.2 | Y < min(Y) + 0.2 | Y > max(Y) - 0.2]$gpstime
  expect_setequal(setdiff(out, edge), setdiff(ref, edge))
})
