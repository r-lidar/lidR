context("spatial index")

LASfile <- example_las_path

test_that("read*LAS work", {

  las = readLAS(LASfile)

  expect_equal(las@index$sensor, lidR:::ALSLAS)
  expect_equal(las@index$index, 0L)

  las = readALSLAS(LASfile)

  expect_equal(las@index$sensor, lidR:::ALSLAS)
  expect_equal(las@index$index, 0L)

  las = readTLSLAS(LASfile)

  expect_equal(las@index$sensor, lidR:::TLSLAS)
  expect_equal(las@index$index, 0L)
})

test_that("read*LAScatalog work", {

  las = readLAScatalog(LASfile)

  expect_equal(las@index$sensor, lidR:::ALSLAS)
  expect_equal(las@index$index, 0L)

  las = readALSLAScatalog(LASfile)

  expect_equal(las@index$sensor, lidR:::ALSLAS)
  expect_equal(las@index$index, 0L)

  las = readTLSLAScatalog(LASfile)

  expect_equal(las@index$sensor, lidR:::TLSLAS)
  expect_equal(las@index$index, 0L)
})

test_that("index works", {

  las = readLAS(LASfile)

  expect_equal(index(las), 0L)

  index(las) <- 2

  expect_equal(index(las), 2L)

  index(las) <- "quadtree"

  expect_equal(index(las), 3L)
  expect_equal(index(las, h = TRUE), "quadtree")

  expect_error(index(las) <- "plop")
  expect_error(index(las) <- 8)
})

test_that("sensor works", {

  las = readLAS(LASfile)

  expect_equal(sensor(las),lidR:::ALSLAS)

  sensor(las) <- lidR:::TLSLAS

  expect_equal(sensor(las), lidR:::TLSLAS)

  sensor(las) <- "tls"

  expect_equal(sensor(las), lidR:::TLSLAS)
  expect_equal(sensor(las, h = TRUE), "TLS")

  expect_error(sensor(las) <- "plop")
  expect_error(sensor(las) <- 8)
})

test_that("knn works in 2D", {

  X = c(0, 1, 1, 0, 0.5)
  Y = c(0, 0, 1, 1, 0.5)
  Z = c(0, 1, 1, 0, 0.5)

  x = 0.1
  y = 0.1

  nn = lidR:::C_knn(X,Y, x, y, 1, 1)

  expect_equal(nn$nn.idx, matrix(1))
  expect_equal(nn$nn.dist, matrix(sqrt(2)/10))

  nn = lidR:::C_knn(X,Y, x, y, 2, 1)

  expect_equal(nn$nn.idx, matrix(c(1,5), ncol = 2))
  expect_equal(nn$nn.dist, matrix(c(matrix(sqrt(2)/10), 4*sqrt(2)/10), ncol = 2))
})

test_that("Spatial index knn works even with npoints < k", {

  X = c(0, 1, 1, 0, 0.5)
  Y = c(0, 0, 1, 1, 0.5)
  Z = c(0, 1, 1, 0, 0.5)

  nn = lidR:::C_knn(X,Y, c(0,2), c(0.8,2), 8, 1)

  expect <- matrix(c(4,5,1,3,2,0,0,0,3,5,4,2,1,0,0,0), ncol = 8, byrow = T)

  expect_equal(nn$nn.idx, expect)
})

test_that("knnidw works", {

  X = c(0, 1, 1, 0, 0.5)
  Y = c(0, 0, 1, 1, 0.5)
  Z = c(0, 1, 1, 0, 0.5)
  D = data.frame(X,Y,Z)
  las = LAS(D)

  val1 = lidR:::C_knnidw(las, 0.1, 0.1, 1, 1, 50, 1)
  val2 = lidR:::C_knnidw(las, 0.2, 0.2, 1, 1, 50, 1)
  val3 = lidR:::C_knnidw(las, 0.3, 0.3, 1, 1, 50, 1)
  val4 = lidR:::C_knnidw(las, 0.4, 0.5, 1, 1, 50, 1)

  expect_equal(val1, 0)
  expect_equal(val2, 0)
  expect_equal(val3, 0.5)
  expect_equal(val4, 0.5)

  val1 = lidR:::C_knnidw(las, 0.25, 0.25, 2, 1, 50, 1)
  val2 = lidR:::C_knnidw(las, 0.2, 0.2, 2, 1, 50, 1)

  expect_equal(val1, 0.25)
  expect_equal(val2, 0.2)
})

X = c(1, 2, 3, 10, 11, 12, 1, 2, 3)
Y = c(3, 2, 1, 5, 7, 6, 8, 9, 7)
Z = c(20, 1, 1, 5, 7, 6, 1, 1, 4)
D <- data.frame(X,Y,Z)
las = LAS(D)

test_that("circle lookup works for each spatial index", {

  for(index in 0:3)
  {
    index(las) <- index

    id = lidR:::C_circle_lookup(las, 2,2,1)
    expect_equal(id, 2)

    id = lidR:::C_circle_lookup(las, 1.9,3,1)
    expect_equal(id, 1)

    id = lidR:::C_circle_lookup(las, 2,2,2)
    id = sort(id)
    expect_equal(id, 1:3)

    id = lidR:::C_circle_lookup(las, 6,6,2)
    expect_equal(length(id), 0)
  }
})

test_that("oriented rectangle lookup works for each spatial index", {

  for(index in 0:3)
  {
    index(las) <- index

    id = lidR:::C_orectangle_lookup(las, 2,2,3,3, -pi/4)
    id = sort(id)
    expect_equal(id, 1:3)

    id = lidR:::C_orectangle_lookup(las, 10, 6, 5, 2, 0)
    expect_equal(id, 6)

    id = lidR:::C_orectangle_lookup(las, 10, 6, 5, 2, pi/8)
    id = sort(id)
    expect_equal(id, 4:6)
  }
})

test_that("knn in 2d works for each spatial indexes", {

  for(index in 0:3)
  {
    index(las) <- index

    id = lidR:::C_knn2d_lookup(las, 2,2,1)
    expect_equal(id, 2)

    id = lidR:::C_knn2d_lookup(las, 2,2.1,4)
    expect_equal(id, c(2,1,3,9))

    id = lidR:::C_knn2d_lookup(las, 6,6,2)
    expect_equal(id, c(9, 4))
  }
})

test_that("knn in 3d works for each spatial indexes", {

  for(index in 0:3)
  {
    index(las) <- index

    id = lidR:::C_knn3d_lookup(las, 2,2,0,1)
    expect_equal(id, 2)

    id = lidR:::C_knn3d_lookup(las, 2,2,2,4)
    expect_equal(id, c(2,3,9,7))

    id = lidR:::C_knn3d_lookup(las, 6,6,50,2)
    expect_equal(id, c(1, 5))
  }
})

test_that("Spatial index fails with inccorect code", {

    las@index$index <- 12
    expect_error(lidR:::C_circle_lookup(las, 2,2,1), "index code inccorect")
})

test_that("Spatial indexes work with more points (coverage)", {

  las = lidR:::generate_las(2000)

  x = runif(1, 10, 900)
  y = runif(1, 10, 900)
  z = runif(1, 1, 19)

  u = vector("list", 3)
  for(index in 0:3)
  {
    index(las) <- index
    id = lidR:::C_knn3d_lookup(las, x,y,z,10)
    u[[index+1]] = id
  }

  expect_true(all(sapply(u, identical, u[[1]])))

  u = vector("list", 3)
  for(index in 0:3)
  {
    index(las) <- index
    id = lidR:::C_circle_lookup(las, x, y, 5)
    id = sort(id)
    u[[index+1]] = id
  }

  expect_true(all(sapply(u, identical, u[[1]])))
})

test_that("Spatial indexes work with 0 point", {

  las = lidR:::generate_las(0)

  x = runif(1, 10, 900)
  y = runif(1, 10, 900)
  z = runif(1, 1, 19)

  u = vector("list", 3)
  for(index in 0:3)
  {
    index(las) <- index
    id = lidR:::C_knn3d_lookup(las, x, y, z,10)
    u[[index+1]] = id
  }

  expect_true(all(sapply(u, identical, integer(0))))

  u = vector("list", 3)
  for(index in 0:3)
  {
    index(las) <- index
    id = lidR:::C_circle_lookup(las, x, y, 5)
    id = sort(id)
    u[[index+1]] = id
  }

  expect_true(all(sapply(u, identical, integer(0))))
})

test_that("Spatial indexes work with 1 point", {

  las = LAS(data.frame(X = 10, Y = 10, Z = 10))

  x = runif(1, 10, 11)
  y = runif(1, 10, 11)
  z = runif(1, 1, 19)

  u = vector("list", 3)
  for(index in 0:3)
  {
    index(las) <- index
    id = lidR:::C_knn3d_lookup(las, x, y, z, 10)
    u[[index+1]] = id
  }

  expect_true(all(sapply(u, identical, 1L)))

  u = vector("list", 3)
  for(index in 0:3)
  {
    index(las) <- index
    id = lidR:::C_circle_lookup(las, x, y, 5)
    id = sort(id)
    u[[index+1]] = id
  }

  expect_true(all(sapply(u, identical, 1L)))
})

test_that("Spatial indexes work no point to find", {

  las = lidR:::generate_las(10)

  x = -100
  y = -100
  z = 0

  u = vector("list", 3)
  for(index in 0:3)
  {
    index(las) <- index
    id = lidR:::C_circle_lookup(las, x, y, 5)
    id = sort(id)
    u[[index+1]] = id
  }

  expect_true(all(sapply(u, identical, integer(0))))
})

test_that("Forcing autoindex work", {

  las = readLAS(LASfile)

  lidR:::force_autoindex(las) <- lidR:::LIDRGRIDPARTITION

  expect_equal(index(las), 1L)

  lidR:::force_autoindex(las) <- lidR:::LIDRQUADTREE

  expect_equal(index(las), 1L)

  index(las) <- lidR:::LIDRAUTOINDEX

  lidR:::force_autoindex(las) <- lidR:::LIDRQUADTREE

  expect_equal(index(las), 3L)
})
