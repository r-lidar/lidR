context("lasmergespatial")

LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
shapefile_dir <- system.file("extdata", package = "lidR")

lidar <- readLAS(LASfile, select = "xyz", filter = "-thin_with_grid 2")
lakes <- rgdal::readOGR(shapefile_dir, "lake_polygons_UTM17", verbose = FALSE)

test_that("lasmergespatial works with SpatialPolygonsDataFrame", {
  lidar <- lasmergespatial(lidar, lakes, "inlakes")
  cn <- names(lidar@data)

  expect_true("inlakes" %in% cn)
  expect_true(is.logical(lidar@data$inlakes))
  expect_equivalent(as.numeric(table(lidar$inlakes)), c(11051, 1843))

  lidar <- lasmergespatial(lidar, lakes, "LAKENAME_1")
  cn <- names(lidar@data)

  expect_true("LAKENAME_1" %in% cn)
  expect_true(is.factor(lidar@data$LAKENAME_1))
  expect_equivalent(as.numeric(table(lidar$LAKENAME_1)), c(1843))

  lidar <- lasmergespatial(lidar, lakes)
  cn <- names(lidar@data)

  expect_true("id" %in% cn)
  expect_true(is.integer(lidar@data$id))
  expect_equivalent(as.numeric(table(lidar$id)), c(1843))
})

test_that("lasmergespatial never fails", {
  lakes <- as(lakes, "SpatialPolygons")
  lakes@polygons[[1]]@Polygons[[1]]@coords <- lakes@polygons[[1]]@Polygons[[1]]@coords + 2000

  lidar <- lasmergespatial(lidar, lakes)
  cn <- names(lidar@data)

  expect_true("id" %in% cn)
  expect_true(is.integer(lidar@data$id))
  expect_true(all(is.na(lidar$id)))
})

test_that("lasmergespatial works with SpatialPolygons", {
  lakes <- as(lakes, "SpatialPolygons")

  lidar <- lasmergespatial(lidar, lakes)
  cn <- names(lidar@data)

  expect_true("id" %in% cn)
  expect_true(is.integer(lidar@data$id))
  expect_equivalent(as.numeric(table(lidar$id)), c(1843))
})

test_that("las classifies works with raster", {
  r = grid_metrics(lidar, mean(Z))

  lidar <- lasmergespatial(lidar, r, "Zmean")

  cn <- names(lidar@data)

  expect_true("Zmean" %in% cn)
  expect_true(is.numeric(lidar@data$Zmean))
  expect_equal(mean(lidar@data$Zmean), 14.51, tol = 0.01)
})
