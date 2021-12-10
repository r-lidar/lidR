las <- lidR:::generate_las(5000)
st_crs(las) <- 2949
las@data[, Z := round(Z + 0.1*X + 0.1*Y + sin(0.01*X) - sin(0.1*Y) + sin(0.003*X*Y),3)]

tdtm <- lidR:::raster_layout(las, 1, format = getOption("lidR.raster.default"))
xy   <- lidR:::raster_as_dataframe(tdtm, na.rm = FALSE, xy = TRUE)
X    <- xy$X
Y    <- xy$Y
cell <- lidR:::raster_cell_from_xy(tdtm, X, Y)
gnd  <- round(0.1*X + 0.1*Y + sin(0.01*X) - sin(0.1*Y) + sin(0.003*X*Y),3)
tdtm <- lidR:::raster_set_values(tdtm, gnd, cell)

slr = lidR:::raster_class()

test_that("rasterize_terrain works with knnidw", {

  dtm <- rasterize_terrain(las, 1, knnidw(k = 10L))

  expect_true(is(dtm, slr))
  expect_equal(lidR:::raster_res(dtm), c(1,1))
  expect_equal(dim(dtm), dim(tdtm))
  expect_equivalent(sf::st_bbox(dtm), sf::st_bbox(tdtm))
  expect_equal(lidR:::raster_names(dtm), "Z")
  expect_equal(sum(is.na(lidR:::raster_values(dtm))), 1L)

  error <- abs(dtm - tdtm)
  expect_equal(mean(lidR:::raster_values(error), na.rm = TRUE), 0.1558, tolerance = 0.0001)

  z <- lidR:::raster_value_from_xy(dtm, las$X, las$Y)
  expect_true(!anyNA(z))
})

test_that("grid_terrain is backward compatible", {

  dtm <- grid_terrain(las, 1, knnidw(k = 10L))

  expect_true(is(dtm, "RasterLayer"))
  expect_equal(lidR:::raster_res(dtm), c(1,1))
  expect_equivalent(lidR:::raster_size(dtm), c(100, 100, 1))
  expect_equivalent(sf::st_bbox(dtm), sf::st_bbox(tdtm))
  expect_equal(lidR:::raster_names(dtm), "Z")
  expect_equal(sum(is.na(lidR:::raster_values(dtm))), 1L)
})

test_that("rasterize_terrain works with delaunay", {

  dtm <- suppressWarnings(rasterize_terrain(las, 1, tin()))

  expect_true(is(dtm, slr))
  expect_equal(lidR:::raster_res(dtm), c(1,1))
  expect_equal(dim(dtm), dim(tdtm))
  expect_equivalent(sf::st_bbox(dtm), sf::st_bbox(tdtm))
  expect_equal(lidR:::raster_names(dtm), "Z")
  expect_equal(sum(is.na(lidR:::raster_values(dtm))), 1L)

  error <- abs(dtm - tdtm)
  expect_equal(mean(lidR:::raster_values(error), na.rm = TRUE), 0.0739, tolerance = 0.0001)

  z <- lidR:::raster_value_from_xy(dtm, las$X, las$Y)
  expect_true(!anyNA(z))
})

test_that("rasterize_terrain works with kriging", {

  skip_if_not_installed("gstat")

  dtm <- rasterize_terrain(las, 1, kriging(k = 10L))

  expect_true(is(dtm, slr))
  expect_equal(lidR:::raster_res(dtm), c(1,1))
  expect_equal(dim(dtm), dim(tdtm))
  expect_equivalent(sf::st_bbox(dtm), sf::st_bbox(tdtm))
  expect_equal(lidR:::raster_names(dtm), "Z")
  expect_equal(sum(is.na(lidR:::raster_values(dtm))), 1L)

  error <- abs(dtm - tdtm)
  expect_equal(mean(lidR:::raster_values(error), na.rm = TRUE), 0.0604, tolerance = 0.0001)

  z <- lidR:::raster_value_from_xy(dtm, las$X, las$Y)
  expect_true(!anyNA(z))
})

test_that("rasterize_terrain option keep_lowest works", {

  dtm <- rasterize_terrain(las, 1, tin(), keep_lowest = TRUE)

  expect_true(is(dtm, slr))
  expect_equal(lidR:::raster_res(dtm), c(1,1))
  expect_equal(dim(dtm), dim(tdtm))
  expect_equivalent(sf::st_bbox(dtm), sf::st_bbox(tdtm))
  expect_equal(lidR:::raster_names(dtm), "Z")
  expect_equal(sum(is.na(lidR:::raster_values(dtm))), 1L)

  error <- abs(dtm - tdtm)
  expect_equal(mean(lidR:::raster_values(error), na.rm = TRUE), 0.0754, tolerance = 0.0001)

  z <- lidR:::raster_value_from_xy(dtm, las$X, las$Y)
  expect_true(!anyNA(z))
})

las2 <- clip_circle(las, 50, 50, 40)

test_that("rasterize_terrain returns a circular dtm ", {

  dtm <- rasterize_terrain(las2, 1, tin())

  expected_bbox = sf::st_bbox(c(xmin = 10, xmax = 90, ymin = 10, ymax = 90), crs = st_crs(las2))

  expect_true(is(dtm,slr))
  expect_equal(lidR:::raster_res(dtm), c(1,1))
  expect_equal(lidR:::raster_names(dtm), "Z")
  expect_equivalent(sf::st_bbox(dtm), expected_bbox)

  z <- lidR:::raster_value_from_xy(dtm, las2$X, las2$Y)
  expect_true(!anyNA(z))
})

ctg  <- topography_ctg
las  <- topography

opt_chunk_size(ctg)      <- 300
opt_chunk_buffer(ctg)    <- 30
opt_chunk_alignment(ctg) <- c(50, 200)
opt_progress(ctg)        <- FALSE

test_that("rasterize_terrain returns the same both with LAScatalog and LAS", {

  dtm1 <- rasterize_terrain(las, 1, tin())
  dtm2 <- rasterize_terrain(ctg, 1, tin())
  bbox <- sf::st_bbox(sf::st_buffer(sf::st_as_sfc(st_bbox(las), crs = st_crs(las)), -4))

  cdtm1 <- lidR:::raster_crop(dtm1, bbox)
  cdtm2 <- lidR:::raster_crop(dtm2, bbox)

  error <- abs(cdtm1 - cdtm2)

  expect_equal(mean(lidR:::raster_values(error)), 0, tolerance = 2e-5)

  z <- lidR:::raster_value_from_xy(dtm1, las$X, las$Y)
  expect_true(!anyNA(z))
})

test_that("rasterize_terrain with LAScatalog and file", {

  skip_on_cran()

  opt_output_files(ctg) <- "{tempdir()}/{ID}"
  dtm <- rasterize_terrain(ctg, 1, tin())

  expect_is(dtm, "SpatRaster")
})

test_that("rasterize_terrain fails in some specific case", {

  skip_if_not_installed("geometry")

  las@header@PHB$`X scale factor` <- 0.002
  las@header@PHB$`Y scale factor` <- 0.002

  expect_message(rasterize_terrain(las, 1, tin()), "reverted to the old slow method")

  las = data.frame(
    X = runif(10, 0,10),
    Y = runif(10, 0,10),
    Z = 1,
    Classification = 2L)

  las = LAS(las)

  expect_error(rasterize_terrain(las, 1, tin()), NA)

  las@data$Classification <- 1L
  expect_error(rasterize_terrain(las, 1, tin()), "No ground points found")

  expect_error(rasterize_terrain(las, 1, tin(), use_class = 1), NA)

  las@data$Classification <- NULL
  expect_error(rasterize_terrain(las, 1, tin()), "LAS object does not contain 'Classification' attribute")
})

