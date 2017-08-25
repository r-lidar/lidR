context("fasttools")

xmin = 0
ymin = 0
xmax = 20
ymax = 10
xc = (xmax-xmin)/2
yc = (ymax-ymin)/2

m = matrix(runif(200), 10, 20)
r <- raster::raster(m, xmn = xmin, xmx = xmax, ymn = ymin, ymx = ymax)

xres = res(r)[1]

test_that("fast_extract returns the same as raster::extract", {

  x = runif(1000, xmin, xmax)
  y = runif(1000, ymin, ymax)

  m = raster::as.matrix(r)

  z1 = raster::extract(r, cbind(x, y))
  z2 = lidR:::fast_extract(m, x, y, xmin, ymin, xres)

  expect_equal(z1, z2)

  x = c(xmin, xmin, xmin, xc, xc, xmax, xmax, xmax)
  y = c(ymin, yc, ymax, ymin, ymax, ymin, yc, ymax)

  z1 = raster::extract(r, cbind(x, y))
  z2 = lidR:::fast_extract(m, x, y, xmin, ymin, xres)

  expect_equal(z1, z2)

  x = round(runif(10, xmin, ymax))
  y = round(runif(10, ymin, ymax))

  z1 = raster::extract(r, cbind(x, y))
  z2 = lidR:::fast_extract(m, x, y, xmin, ymin, xres)

  expect_equal(z1, z2)

  x = round(runif(10, xmin, ymax))
  y = runif(10, ymin, ymax)

  z1 = raster::extract(r, cbind(x, y))
  z2 = lidR:::fast_extract(m, x, y, xmin, ymin, xres)

  expect_equal(z1, z2)

  x = runif(10, xmin, xmax)
  y = round(runif(10, ymin, ymax))

  z1 = raster::extract(r, cbind(x, y))
  z2 = lidR:::fast_extract(m, x, y, xmin, ymin, xres)

  expect_equal(z1, z2)
})


test_that("fast_extract returns NA outside extent", {

  m = raster::as.matrix(r)

  x = runif(1000, xmin, xmax)
  y = runif(1000, ymin, ymax)

  m = raster::as.matrix(r)

  x = c(5, -2, 22, 5, 7)
  y = c(5, 1, 5, 11, -4)

  z1 = raster::extract(r, cbind(x,y))
  z2 = lidR:::fast_extract(m, x, y, xmin, ymin, xres)

  expect_equal(z1, z2)

})

test_that("fast_extract returns NA inside extent", {

  r[1:4, 4:8] <- NA
  m = raster::as.matrix(r)

  x = runif(1000, xmin, xmax)
  y = runif(1000, ymin, ymax)

  m = raster::as.matrix(r)

  z1 = raster::extract(r, cbind(x, y))
  z2 = lidR:::fast_extract(m, x, y, xmin, ymin, xres)

  expect_equal(z1, z2)
})

