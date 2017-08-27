context("lasclassify")

LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
shapefile_dir <- system.file("extdata", package = "lidR")

lidar = readLAS(LASfile, select = "xyz")
lakes = rgdal::readOGR(shapefile_dir, "lake_polygons_UTM17")

test_that("las classifies works with shapefiles", {
  lasclassify(lidar, lakes, "inlakes")

  cn = names(lidar@data)

  expect_true("inlakes" %in% cn)
  expect_true(is.logical(lidar@data$inlakes))

  lasclassify(lidar, lakes, "LAKENAME_1")

  cn = names(lidar@data)

  expect_true("LAKENAME_1" %in% cn)
  expect_true(is.factor(lidar@data$LAKENAME_1))
})


test_that("las classifies works with raster", {
  r = as.raster(grid_metrics(lidar, mean(Z)))

  lasclassify(lidar, r, "Zmean")

  cn = names(lidar@data)

  expect_true("Zmean" %in% cn)
  expect_true(is.numeric(lidar@data$Zmean))
})
