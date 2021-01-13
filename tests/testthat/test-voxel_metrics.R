context("voxel_metrics")

las <- random_500_points

test_that("voxel_metrics returns a named data.frame", {

  x <- voxel_metrics(las, list(Imean = mean(Intensity)), 5)

  expect_true(is(x, "data.frame"))
  expect_equal(names(x)[4], "Imean")
  expect_equal(nrow(x), 444)
})

test_that("voxel_metrics works with all_voxels", {

  x <- voxel_metrics(las, list(Imean = mean(Intensity)), 5, all_voxels = TRUE)

  expect_true(is(x, "data.frame"))
  expect_equal(names(x)[4], "Imean")
  expect_equal(nrow(x), 2400)
})

test_that("voxel_metrics accepts both an expression or a formula", {

  x <- voxel_metrics(las,  list(Imean = mean(Intensity)), 5)
  y <- voxel_metrics(las, ~list(Imean = mean(Intensity)), 5)

  expect_equal(x, y)
})
