context("lasmergespatial")

LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
shapefile_dir <- system.file("extdata", package = "lidR")

sink(tempfile())

lidar = readLAS(LASfile, select = "xyz")
lakes = rgdal::readOGR(shapefile_dir, "lake_polygons_UTM17")

sink(NULL)

test_that("las classifies works with shapefiles", {
  lidar <- lasmergespatial(lidar, lakes, "inlakes")

  cn = names(lidar@data)

  expect_true("inlakes" %in% cn)
  expect_true(is.logical(lidar@data$inlakes))

  lidar <- lasmergespatial(lidar, lakes, "LAKENAME_1")

  cn = names(lidar@data)

  expect_true("LAKENAME_1" %in% cn)
  expect_true(is.factor(lidar@data$LAKENAME_1))
})


test_that("las classifies works with raster", {
  r = grid_metrics(lidar, mean(Z))

  lidar <- lasmergespatial(lidar, r, "Zmean")

  cn = names(lidar@data)

  expect_true("Zmean" %in% cn)
  expect_true(is.numeric(lidar@data$Zmean))
})
