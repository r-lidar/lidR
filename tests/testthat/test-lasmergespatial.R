context("merge_spatial")

LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
shapefile_dir <- system.file("extdata", package = "lidR")

lidar <- readLAS(LASfile, select = "xyzi", filter = "-thin_with_grid 2")
lakes <- rgdal::readOGR(shapefile_dir, "lake_polygons_UTM17", verbose = FALSE)

test_that("merge_spatial works with SpatialPolygonsDataFrame", {

  lidar <- merge_spatial(lidar, lakes, "inlakes")
  cn <- names(lidar@data)

  expect_true("inlakes" %in% cn)
  expect_true(is.logical(lidar@data$inlakes))
  expect_equivalent(as.numeric(table(lidar$inlakes)), c(11051, 1843))

  lidar <- merge_spatial(lidar, lakes, "LAKENAME_1")
  cn <- names(lidar@data)

  expect_true("LAKENAME_1" %in% cn)
  expect_equal(typeof(lidar@data$LAKENAME_1), typeof(lakes$LAKENAME_1))
  expect_equivalent(as.numeric(table(lidar$LAKENAME_1)), c(1843))

  lidar <- merge_spatial(lidar, lakes)
  cn <- names(lidar@data)

  expect_true("id" %in% cn)
  expect_true(is.integer(lidar@data$id))
  expect_equivalent(as.numeric(table(lidar$id)), c(1843))
})

test_that("merge_spatial works with sf", {

  lakes <- sf::st_as_sf(lakes)

  lidar <- merge_spatial(lidar, lakes, "inlakes")
  cn <- names(lidar@data)

  expect_true("inlakes" %in% cn)
  expect_true(is.logical(lidar@data$inlakes))
  expect_equivalent(as.numeric(table(lidar$inlakes)), c(11051, 1843))

  lidar <- merge_spatial(lidar, lakes, "LAKENAME_1")
  cn <- names(lidar@data)

  expect_true("LAKENAME_1" %in% cn)
  expect_equal(typeof(lidar@data$LAKENAME_1), typeof(lakes$LAKENAME_1))
  expect_equivalent(as.numeric(table(lidar$LAKENAME_1)), c(1843))

  lidar <- merge_spatial(lidar, lakes)
  cn <- names(lidar@data)

  expect_true("id" %in% cn)
  expect_true(is.integer(lidar@data$id))
  expect_equivalent(as.numeric(table(lidar$id)), c(1843))
})

test_that("merge_spatial never fails (sp)", {

  # sp
  lakes <- as(lakes, "SpatialPolygons")
  lakes@polygons[[1]]@Polygons[[1]]@coords <- lakes@polygons[[1]]@Polygons[[1]]@coords + 2000

  lidar <- merge_spatial(lidar, lakes)
  cn <- names(lidar@data)

  expect_true("id" %in% cn)
  expect_true(is.integer(lidar@data$id))
  expect_true(all(is.na(lidar$id)))
})

test_that("merge_spatial never fails (sf)", {

  lakes <- as(lakes, "SpatialPolygons")
  lakes@polygons[[1]]@Polygons[[1]]@coords <- lakes@polygons[[1]]@Polygons[[1]]@coords + 2000
  lakes <- sf::st_as_sf(lakes)

  lidar <- merge_spatial(lidar, lakes)
  cn <- names(lidar@data)

  expect_true("id" %in% cn)
  expect_true(is.integer(lidar@data$id))
  expect_true(all(is.na(lidar$id)))
})

test_that("merge_spatial do not fail with 1 point (#347)", {

  one_out <- filter_poi(lidar, Z >= 29.97)
  one_in <- filter_poi(lidar, Intensity == 320)

  one_out <- merge_spatial(one_out, lakes)
  one_in <- merge_spatial(one_in, lakes)

  expect_equal(one_out$id, NA_integer_)
  expect_equal(one_in$id, 1L)
})

test_that("merge_spatial works with SpatialPolygons", {

  lakes <- as(lakes, "SpatialPolygons")

  lidar <- merge_spatial(lidar, lakes)
  cn <- names(lidar@data)

  expect_true("id" %in% cn)
  expect_true(is.integer(lidar@data$id))
  expect_equivalent(as.numeric(table(lidar$id)), c(1843))
})

test_that("merge_spatial works with raster", {

  r = grid_metrics(lidar, mean(Z))

  lidar <- merge_spatial(lidar, r, "Zmean")

  cn <- names(lidar@data)

  expect_true("Zmean" %in% cn)
  expect_true(is.numeric(lidar@data$Zmean))
  expect_equal(mean(lidar@data$Zmean), 14.51, tol = 0.01)
})


test_that("merge_spatial works a RGB RasterBrick", {

  layout = lidR:::rOverlay(lidar, 5)
  R = sample(0:(2^16-1), raster::ncell(layout))
  G = sample(0:(2^16-1), raster::ncell(layout))
  B = sample(0:(2^16-1), raster::ncell(layout))

  r = layout
  r[] <- R
  layout[] <- G
  r = addLayer(r, layout)
  layout[] <- B
  r = addLayer(r, layout)

  lidar <- merge_spatial(lidar, r)

  cn <- names(lidar@data)

  expect_true(all(c("R", "G", "B") %in% cn))
  expect_true(is.integer(lidar@data$R))
  expect_equal(lidar@header@PHB$`Point Data Format ID`, 2L)
})
