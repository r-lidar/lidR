context("lasclip")

LASfile <- system.file("extdata", "Megaplot.laz", package = "lidR")
las <- readLAS(LASfile, select = "xyz", filter = "-keep_class 2")
ctg <- catalog(LASfile)

opt_progress(ctg) <- FALSE
opt_select(ctg)   <- "xyz"
opt_filter(ctg)   <- "-keep_class 2"

shapefile_dir <- system.file("extdata", package = "lidR")
lakes <- rgdal::readOGR(shapefile_dir, "lake_polygons_UTM17", verbose = F)

test_that("lasclip clips a rectangle both on a LAS and a LAScatalog", {

  rect1 <- lasclipRectangle(las, 684850, 5017850, 684900, 5017900)
  rect2 <- lasclipRectangle(ctg, 684850, 5017850, 684900, 5017900)

  expect_true(extent(rect1) <= raster::extent(684850, 5017850, 684900, 5017900))
  expect_equal(rect1@proj4string, las@proj4string)
  expect_equal(nrow(rect1@data), 168L)
  expect_true(extent(rect2) <= raster::extent(684850, 5017850, 684900, 5017900))
  expect_equal(rect2@proj4string, ctg@proj4string)
  expect_equal(rect1, rect2)
})

test_that("lasclip clips a circle both on a LAS and a LAScatalog", {

  circ1 <- lasclipCircle(las, 684850, 5017850, 10)
  circ2 <- lasclipCircle(ctg, 684850, 5017850, 10)

  expect_true(extent(circ1) <= raster::extent(684850 - 10, 5017850 - 10,684850 + 10, 5017850 + 10))
  expect_equal(circ1@proj4string, las@proj4string)
  expect_equal(nrow(circ1@data), 7L)
  expect_true(extent(circ2) <= raster::extent(684850 - 10, 5017850 - 10, 684850 + 10, 5017850 + 10))
  expect_equal(circ2@proj4string, ctg@proj4string)
  expect_equal(circ1, circ2)
})

test_that("lasclip clips a polygon both on a LAS and a LAScatalog", {

  tri1 <- lasclipPolygon(las, c(684850, 684900, 684975, 684850), c(5017850, 5017900, 5017800, 5017850))
  tri2 <- lasclipPolygon(ctg, c(684850, 684900, 684975, 684850), c(5017850, 5017900, 5017800, 5017850))

  expect_true(extent(tri1) <= raster::extent(684850, 5017800, 684975, 5017900))
  expect_equal(tri1@proj4string, las@proj4string)
  expect_equal(nrow(tri1@data), 268L)
  expect_true(extent(tri2) <= raster::extent(684850, 5017800, 684975, 5017900))
  expect_equal(tri2@proj4string, ctg@proj4string)
  expect_equal(tri1, tri2)
})

test_that("lasclip memory optimization works", {

  las2 <- lasclipPolygon(las, c(0, 8e6, 8e6, 0), c(0, 5e8, 0, 0))
  las3 <- lasclipCircle(las, 684850, 5017850, 1000)

  expect_reference(las@data, las2@data)
  expect_reference(las@data, las3@data)
})

test_that("lasclip clips polygon works from WTK both on a LAS and LAScatalog", {

  wkt1 <- "MULTIPOLYGON (((684950.8 5017989, 685003.3 5017962, 684938.5 5017905, 684950.8 5017989)), ((684796.2 5017963, 684921.6 5017977, 684899.2 5017806, 684780.7 5017795, 684796.2 5017963), (684899.4 5017924, 684851.7 5017945, 684863.7 5017857, 684899.4 5017924)))"
  wkt2 <- "POLYGON ((684975.7 5017899, 685007.3 5017873, 684994.3 5017816, 684936.1 5017812, 684918.8 5017845, 684975.7 5017899))"

  mpoly1  <- lasclip(las, wkt1)
  poly1   <- lasclip(las, wkt2)
  mpoly2  <- lasclip(ctg, wkt1)
  poly2   <- lasclip(ctg, wkt2)

  expect_is(mpoly1, "LAS")
  expect_equal(nrow(mpoly1@data), 1564L)
  expect_equal(nrow(poly1@data), 230L)
  expect_equal(mpoly1, mpoly2)
  expect_equal(poly1, poly2)
  expect_equal(mpoly1@proj4string, las@proj4string)
  expect_equal(mpoly2@proj4string, ctg@proj4string)
})

test_that("lasclip clips polygon works from sp polygons both on a LAS and LAScatalog", {

  wkt1 <- "MULTIPOLYGON (((684950.8 5017989, 685003.3 5017962, 684938.5 5017905, 684950.8 5017989)), ((684796.2 5017963, 684921.6 5017977, 684899.2 5017806, 684780.7 5017795, 684796.2 5017963), (684899.4 5017924, 684851.7 5017945, 684863.7 5017857, 684899.4 5017924)))"
  wkt2 <- "POLYGON ((684975.7 5017899, 685007.3 5017873, 684994.3 5017816, 684936.1 5017812, 684918.8 5017845, 684975.7 5017899))"

  # Polygon
  spatialpolygons1 <- rgeos::readWKT(wkt1)
  spatialpolygons2 <- rgeos::readWKT(wkt2)

  polygon1 <- spatialpolygons1@polygons[[1]]@Polygons[[1]]
  poly1    <- lasclip(las, polygon1)
  poly2    <- lasclip(ctg, polygon1)

  expect_is(poly1, "LAS")
  expect_equal(nrow(poly1@data), 176L)
  expect_equal(poly1@proj4string, las@proj4string)
  expect_equal(poly2@proj4string, ctg@proj4string)
  expect_equal(poly1, poly2)

  # Polygons
  polygons1 <- spatialpolygons1@polygons[[1]]
  poly1     <- lasclip(las, polygons1)
  poly2     <- lasclip(ctg, polygons1)

  expect_is(poly1, "LAS")
  expect_equal(nrow(poly1@data), 1564L)
  expect_equal(poly1@proj4string, las@proj4string)
  expect_equal(poly2@proj4string, ctg@proj4string)
  expect_equal(poly1, poly2)

  # SpatialPolygonsDataFrame
  poly1 <- lasclip(las, lakes)
  poly2 <- lasclip(ctg, lakes)

  expect_is(poly1, "LAS")
  expect_equal(nrow(poly1@data), 4790L)
  expect_equal(poly1, poly2)
})

test_that("lasclip clips a rectangle from a bounding box both on a LAS and LAScatalog", {

  # Extent
  bbox <- extent(las)
  bbox <- bbox - 200

  rect1 <- lasclip(las, bbox)
  rect2 <- lasclip(ctg, bbox)

  expect_true(extent(rect1) <= bbox)
  expect_equal(rect1, rect2)

  # Matrix 2x2
  m <- raster::as.matrix(bbox)

  rect1 <- lasclip(las, m)
  rect2 <- lasclip(ctg, m)

  expect_true(extent(rect1) <= bbox)
  expect_equal(rect1, rect2)
})

test_that("lasclip returns an empty point cloud if no point found in the query", {

  circ1 <- suppressWarnings(lasclipCircle(las, 68480, 5017850, 10))
  circ2 <- suppressWarnings(lasclipCircle(ctg, 68480, 5017850, 10))

  expect_true(is.empty(circ1))
  expect_true(is.empty(circ2))
})


test_that("lasclip supports multiple queries", {

  # Multiple disc
  xc <- c(684800, 684850)
  yc <- c(5017850, 5017900)
  r  <- 10

  circ1 <- lasclipCircle(las, xc, yc, r)
  circ2 <- lasclipCircle(ctg, xc, yc, r)

  expect_is(circ1, "list")
  expect_equal(length(circ1), 2L)
  expect_equal(circ1, circ2)

  # Multiple rectangle
  xmin <- 684850 + c(0,1)
  ymin <- 5017850 + c(0,1)
  xmax <- 684900 + c(0,1)
  ymax <- 5017900 + c(0,1)

  rect1 <- lasclipRectangle(las, xmin, ymin, xmax, ymax)
  rect2 <- lasclipRectangle(ctg, xmin, ymin, xmax, ymax)

  expect_is(rect1, "list")
  expect_equal(length(circ1), 2L)
  expect_equal(circ1, circ2)

  # Multiple polygons

  wkt1 <- "MULTIPOLYGON (((684950.8 5017989, 685003.3 5017962, 684938.5 5017905, 684950.8 5017989)), ((684796.2 5017963, 684921.6 5017977, 684899.2 5017806, 684780.7 5017795, 684796.2 5017963), (684899.4 5017924, 684851.7 5017945, 684863.7 5017857, 684899.4 5017924)))"
  wkt2 <- "MULTIPOLYGON (((684975.7 5017899, 685007.3 5017873, 684994.3 5017816, 684936.1 5017812, 684918.8 5017845, 684975.7 5017899)))"
  wkt  <- glue::glue("GEOMETRYCOLLECTION({wkt1}, {wkt2})")
  spatialpolygons <- rgeos::readWKT(wkt)

  polys1 <- lasclip(las, spatialpolygons)
  polys2 <- lasclip(ctg, spatialpolygons)

  expect_is(polys1, "list")
  expect_equal(length(polys1), 2L)
  expect_equal(nrow(polys1[[1]]@data), 1564L)
  expect_equal(nrow(polys1[[2]]@data), 230L)
  expect_equal(polys1[[1]]@proj4string, las@proj4string)
  expect_equal(polys1, polys2)
})

test_that("lasclip returns an empty point cloud for empty multiple queries", {

  # Multiple disc
  xc <- c(684800, 68480)
  yc <- c(5017850, 5017900)
  r  <- 10

  circ1 <- suppressWarnings(lasclipCircle(las, xc, yc, r))
  circ2 <- suppressWarnings(lasclipCircle(ctg, xc, yc, r))

  expect_is(circ1, "list")
  expect_equal(length(circ1), 2L)
  expect_true(is.empty(circ1[[2]]))
  expect_equal(circ1, circ2)


  # Multiple rectangle
  xmin <- 684850 + c(0,-2000)
  ymin <- 5017850 + c(0,-2000)
  xmax <- 684900 + c(0,-2000)
  ymax <- 5017900 + c(0,-2000)

  rect1 <- suppressWarnings(lasclipRectangle(las, xmin, ymin, xmax, ymax))
  rect2 <- suppressWarnings(lasclipRectangle(ctg, xmin, ymin, xmax, ymax))

  expect_is(rect1, "list")
  expect_equal(length(rect1), 2L)
  expect_true(is.empty(rect1[[2]]))
  expect_equal(rect1, rect2)

  # Multiple polygons

  wkt1 <- "MULTIPOLYGON (((684950.8 5017989, 685003.3 5017962, 684938.5 5017905, 684950.8 5017989)), ((684796.2 5017963, 684921.6 5017977, 684899.2 5017806, 684780.7 5017795, 684796.2 5017963), (684899.4 5017924, 684851.7 5017945, 684863.7 5017857, 684899.4 5017924)))"
  wkt2 <- "MULTIPOLYGON (((68497 501789, 68500.3 501783, 68499 501786, 68496 501782, 68491 501784, 68497 501789)))"
  wkt  <- glue::glue("GEOMETRYCOLLECTION({wkt1}, {wkt2})")
  spatialpolygons <- rgeos::readWKT(wkt)

  polys1 <- suppressWarnings(lasclip(las, spatialpolygons))
  polys2 <- suppressWarnings(lasclip(ctg, spatialpolygons))

  expect_is(polys1, "list")
  expect_equal(length(polys1), 2L)
  expect_true(is.empty(polys1[[2]]))
  expect_equal(polys1, polys2)
})


test_that("lasclip throws errors with invalid queries", {

  # Invalid WKT
  wkt <- "POLGON ((684975.7 5«017899, 685007.3 5017873, 684994.3 5017816, 684936.1 5017812, 684918.8 5017845, 684975.7 5017899))"

  expect_error(lasclip(las, wkt), "Unable to parse")
  expect_error(lasclip(ctg, wkt), "Unable to parse")

  # Different number of coordinates
  xc <- c(684800)
  yc <- c(5017850, 5017900)
  r  <- 10

  expect_error(lasclipCircle(las, xc, yc, r), "different lengths")
  expect_error(lasclipCircle(ctg, xc, yc, r), "different lengths")

  xmin <- 684850
  ymin <- 5017850 + c(0,1)
  xmax <- 684900 + c(0,1)
  ymax <- 5017900 + c(0,1)

  expect_error(lasclipRectangle(las, xmin, ymin, xmax, ymax), "different lengths")
  expect_error(lasclipRectangle(ctg, xmin, ymin, xmax, ymax), "different lengths")

  # Wrong matrix dimension
  m <- matrix(0, 3, 2)

  expect_error(lasclip(las, m), "2 x 2")
  expect_error(lasclip(ctg, m), "2 x 2")

  # Non supported object
  geom <- 1
  class(geom) <- c("A",  "B")
  expect_error(lasclip(las, geom), "Geometry type A B not supported")
})

test_that("clip writes file following LAScatalog options", {

  tmp  <- tempdir()

  ctg2 <- ctg
  opt_output_files(ctg2)    <- paste0(tmp, "/file_{XLEFT}")
  opt_laz_compression(ctg2) <- TRUE

  ctg3 <- lasclipRectangle(ctg2, 684850, 5017850, 684900, 5017900)

  expect_true(is(ctg3, "LAScatalog"))
  expect_equal(normalizePath(ctg3@data$filename), normalizePath(paste0(tmp, "/file_684850.laz")))

  file.remove(paste0(tmp, "/file_684850.laz"))

  opt_output_files(ctg2)    <- paste0(tmp, "/file_{LAKENAME_1}")
  opt_laz_compression(ctg2) <- FALSE
  ctg3 <- lasclip(ctg2, lakes)

  expect_equal(normalizePath(ctg3@data$filename), normalizePath(paste0(tmp, "/file_Havelock Lake.las")))

  file.remove(paste0(tmp, "/file_Havelock Lake.las"))
})

