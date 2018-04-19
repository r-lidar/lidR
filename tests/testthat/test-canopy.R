context("canopy")


test_that("grid_canopy works", {
  las = lidR:::dummy_las(10000)
  chm = grid_canopy(las, 4)
  expect_equal(dim(chm), c((100/4)^2, 3))
})

test_that("grid_canopy works 2", {

  dt = data.table::data.table(X = seq(0,10, 0.5), Y = seq(0,10, 0.5), Z = runif(21))
  las = suppressWarnings(LAS(dt))

  chm = grid_canopy(las, 2)

  expect_equal(min(chm$X), 1)
  expect_equal(min(chm$Y), 1)
  expect_equal(max(chm$X), 11)
  expect_equal(max(chm$Y), 11)
  expect_equal(nrow(chm), 5+1)

  chm = grid_canopy(las, 1)

  expect_equal(min(chm$X), 0.5)
  expect_equal(min(chm$Y), 0.5)
  expect_equal(max(chm$X), 10.5)
  expect_equal(max(chm$Y), 10.5)
  expect_equal(nrow(chm), 10+1)
})

test_that("grid_canopy works with subcircle", {

  dt = data.table::data.table(X = 0, Y = 0, Z = 0)
  las = suppressWarnings(LAS(dt))

  chm = grid_canopy(las, 0.5, 10)
  expect_equal(dim(chm), c(8, 3))

  dt = data.table::data.table(X = c(0,10), Y = c(0,20), Z = c(0, 5))
  las = suppressWarnings(LAS(dt))

  chm = grid_canopy(las, 0.5, 10)
  expect_equal(dim(chm), c(16, 3))
})

test_that("grid_canopy create the same pixels independently of the starting point", {

  las  = lidR:::dummy_las(100)
  las2 = las
  las3 = las

  las2@data = rbind(las@data, list(-11.5,-30.25,20L,1L,1L,1L,1L))
  las3@data = rbind(las@data, list(-21.10,-18.75,20L,1L,1L,1L,1L))

  las2 = LAS(las2@data, las@header)
  las3 = LAS(las3@data, las@header)

  chm1 = grid_canopy(las2, 4)
  chm2 = grid_canopy(las3, 4)

  data.table::setorder(chm1, X, Y)
  data.table::setorder(chm2, X, Y)

  chm1 = chm1[-1]
  chm2 = chm2[-1]

  expect_equal(chm1, chm2)
})

