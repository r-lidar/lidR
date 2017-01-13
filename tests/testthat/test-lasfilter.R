library(data.table)

context("lasfilter")

dt = data.table(X = runif(1000, 0, 100),
                Y = runif(1000, 0, 100),
                Z = runif(1000, 0, 25),
                Intensity = rnorm(1000, 50, 10),
                ReturnNumber = rep(c(1,1,1,2,3,1,2,1,2,1), 10))

las = LAS(dt)

test_that("filter first return works", {
  expect_equal(dim(lasfilterfirst(las)@data)[1], 600)
})
