context("voxel_metrics")

las <- lidR:::dummy_las(500)

test_that("voxel_metrics returns a named data.frame", {

  x <- voxel_metrics(las, list(Imean = mean(Intensity)), 5)

  expect_true(is(x, "data.frame"))
  expect_equal(names(x)[4], "Imean")
})

test_that("voxel_metrics accepts both an expression or a formula", {

  x <- voxel_metrics(las,  list(Imean = mean(Intensity)), 5)
  y <- voxel_metrics(las, ~list(Imean = mean(Intensity)), 5)

  expect_equal(x, y)
})
