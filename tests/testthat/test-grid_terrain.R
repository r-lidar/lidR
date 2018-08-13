context("grid_terrain")

las = lidR:::dummy_las(5000)
las@crs = sp::CRS("+init=epsg:4326")
las@data[, Z := Z + 0.1*X+0.1*Y+sin(0.01*X)-sin(0.1*Y)+sin(0.003*X*Y)]

truedtm = lidR:::make_overlay_raster(las, 1)
xy = raster::xyFromCell(truedtm, 1:raster::ncell(truedtm))
X = xy[,1]
Y = xy[,2]
truedtm[] = 0.1*X+0.1*Y+sin(0.01*X)-sin(0.1*Y)+sin(0.003*X*Y)

test_that("grid_terrain works with knnidw", {
  dtm = grid_terrain(las, 1, method = "knnidw", k = 10L)

  expect_true(is(dtm, "RasterLayer"))
  expect_equal(raster::res(dtm), c(1,1))
  expect_equal(dim(dtm), dim(truedtm))
  expect_equal(raster::extent(dtm), raster::extent(truedtm))
  expect_equal(dtm@crs, las@crs)
  expect_equal(names(dtm), "Z")

  error = abs(dtm-truedtm)
  expect_lt(mean(error[], na.rm = TRUE), 0.21)

  z = raster::extract(dtm, las@data[, .(X,Y)])
  expect_true(!anyNA(z))
})

test_that("grid_terrain works with delaunay", {
  dtm = suppressWarnings(grid_terrain(las, 1, method = "delaunay"))

  expect_true(is(dtm, "RasterLayer"))
  expect_equal(raster::res(dtm), c(1,1))
  expect_equal(dim(dtm), dim(truedtm))
  expect_equal(raster::extent(dtm), raster::extent(truedtm))
  expect_equal(dtm@crs, las@crs)
  expect_equal(names(dtm), "Z")

  error = abs(dtm-truedtm)
  expect_lt(mean(error[], na.rm = TRUE), 0.095)

  z = raster::extract(dtm, las@data[, .(X,Y)])
  expect_true(!anyNA(z))
})

test_that("gridterrain works with kriging", {
  dtm = grid_terrain(las, 1, method = "kriging", k = 10L)

  expect_true(is(dtm, "RasterLayer"))
  expect_equal(raster::res(dtm), c(1,1))
  expect_equal(dim(dtm), dim(truedtm))
  expect_equal(raster::extent(dtm), raster::extent(truedtm))
  expect_equal(dtm@crs, las@crs)
  expect_equal(names(dtm), "Z")

  error = abs(dtm-truedtm)
  expect_lt(mean(error[], na.rm = TRUE), 0.07)

  z = raster::extract(dtm, las@data[, .(X,Y)])
  expect_true(!anyNA(z))
})

las2 = lasclipCircle(las, 50,50,40)

test_that("grid_terrain returns a circular dtm ", {

  dtm = grid_terrain(las2, 1, method = "delaunay")

  expect_true(is(dtm, "RasterLayer"))
  expect_equal(raster::res(dtm), c(1,1))
  expect_equal(dtm@crs, las@crs)
  expect_equal(names(dtm), "Z")

  error = suppressWarnings(abs(dtm-truedtm))
  expect_lt(mean(error[], na.rm = TRUE), 0.095)

  z = raster::extract(dtm, las2@data[, .(X,Y)])
  expect_true(!anyNA(z))
})

file <- system.file("extdata", "Topography.laz", package="lidR")
ctg = catalog(file)
las = readLAS(file)
cores(ctg) <- 1
tiling_size(ctg) <- 180
buffer(ctg) <- 30
ctg@clustering_options$alignment = c(332400, 5238400)
progress(ctg) <- FALSE

test_that("grid_terrain return the same both with LAScatalog and LAS", {
  dtm1 = grid_terrain(las, 1, method = "delaunay")
  dtm2 = grid_terrain(ctg, 1, method = "delaunay")

  bbox = raster::extent(dtm1)-2*3

  cdtm1 = raster::crop(dtm1, bbox)
  cdtm2 = raster::crop(dtm2, bbox)

  error = abs(cdtm1 - cdtm2)
  error = error[error > 0.01]

  expect_lt(length(error), raster::ncell(cdtm1)*0.002)
  expect_lt(mean(error), 0.05)

  z = raster::extract(dtm2, las@data[, .(X,Y)])
  expect_true(!anyNA(z))
})

