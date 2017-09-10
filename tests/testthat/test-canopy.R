context("canopy")


las = lidR:::dummy_las(10000)

test_that("grid_canopy works", {
  chm1 = grid_canopy(las, 4)
  expect_equal(dim(chm1), c((100/4)^2, 3))
})

test_that("grid_canopy works with subcircle", {
  chm1 = grid_canopy(las, 4, subcircle = 0.1)
  expect_equal(dim(chm1), c((100/4)^2, 3))
})

test_that("grid_tincanopy works", {
  chm1 = grid_tincanopy(las, 4)
  chm1 = grid_tincanopy(las, 4, subcircle = 0.5)
})
