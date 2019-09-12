context("progress bar")

test_that("progress bar work at C++ level", {
  sink(tempfile())
  las = lidR:::dummy_las(500)
  options(lidR.progress = TRUE)
  options(lidR.progress.delay = 0)
  expect_error(lassmooth(las, 3), NA)
  options(lidR.progress = FALSE)
  options(lidR.progress.delay = 2)
  sink(NULL)
})
