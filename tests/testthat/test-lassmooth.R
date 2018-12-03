context("lassmooth")

data = data.frame(X = c(0,10,20), Y = c(0,0,0), Z = c(0,1,2))
las = LAS(data)

test_that("lassmooth works with average", {

  las <- lassmooth(las, 1, "average")
  expect_equal(las@data$Z, las@data$Zraw)

  las <- lasunsmooth(las)

  las <- lassmooth(las, 20, "average")
  expect_equal(las@data$Z, c(0.5, 1, 1.5))

  las <- lasunsmooth(las)

  las <- lassmooth(las, 50, "average")
  expect_equal(las@data$Z, c(1, 1, 1))

  lasunsmooth(las)
})

test_that("lassmooth works with gaussian", {

  las <- lassmooth(las, 1, "gaussian", sigma = 2)
  expect_equal(las@data$Z, las@data$Zraw)

  las <- lasunsmooth(las)

  las <- lassmooth(las, 20, "gaussian", sigma = 2)
  expect_equal(las@data$Z, las@data$Zraw, tolerance = 0.001)

  las <- lasunsmooth(las)

  las <- lassmooth(las, 20, "gaussian", sigma = 10)

  w1 = 1/(20*pi)*exp(-100/200)
  w2 = 1/(20*pi)
  z1 = (w2*data$Z[1] + w1*data$Z[2])/(w1+w2)
  z3 = (w1*data$Z[2] + w2*data$Z[3])/(w1+w2)
  expect_equal(las@data$Z, c(z1, 1, z3))

  las <- lasunsmooth(las)
})

