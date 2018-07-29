context("lasclip")

LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
LASfolder <- system.file("extdata", "", package="lidR")
las = readLAS(LASfile, select = "xyx", filter = "-keep_first")
las@crs = sp::CRS("+init=epsg:26917")
ctg = catalog(LASfolder)
ctg@crs = sp::CRS("+init=epsg:26917")
cores(ctg) <- 1
progress(ctg) <- FALSE

test_that("clip rectangle works on a LAS and a LAScatalog", {
  rect1 = lasclipRectangle(las, 684850, 5017850, 684900, 5017900)
  expect_true(extent(rect1) <= raster::extent(684850, 5017850, 684900, 5017900))
  expect_equal(rect1@crs, las@crs)
  expect_equal(nrow(rect1@data), 2789)

  rect2 = lasclipRectangle(ctg, 684850, 5017850, 684900, 5017900,  select = "xyx", filter = "-keep_first")
  expect_true(extent(rect2) <= raster::extent(684850, 5017850, 684900, 5017900))
  expect_equal(rect2@crs, ctg@crs)

  expect_equal(rect1, rect2)
})

test_that("clip circle works on a LAS and a LAScatalog", {
  circ1 = lasclipCircle(las, 684850, 5017850, 10)
  expect_true(extent(circ1) <= raster::extent(684850-10,5017850-10,684850+10,5017850+10))
  expect_equal(circ1@crs, las@crs)
  expect_equal(nrow(circ1@data), 361L)

  circ2 = lasclipCircle(ctg, 684850, 5017850, 10, select = "xyx", filter = "-keep_first")
  expect_true(extent(circ2) <= raster::extent(684850-10,5017850-10,684850+10,5017850+10))
  expect_equal(circ2@crs, ctg@crs)

  expect_equal(circ1, circ2)
})

test_that("clip polygon works on a LAS and a LAScatalog", {
  tri1 = lasclipPolygon(las, c(684850, 684900, 684975, 684850), c(5017850, 5017900, 5017800, 5017850))
  expect_true(extent(tri1) <= raster::extent(684850, 5017800, 684975, 5017900))
  expect_equal(tri1@crs, las@crs)
  expect_equal(nrow(tri1@data), 4784L)

  tri2 = lasclipPolygon(ctg, c(684850, 684900, 684975, 684850), c(5017850, 5017900, 5017800, 5017850), select = "xyx", filter = "-keep_first")
  expect_true(extent(tri2) <= raster::extent(684850, 5017800, 684975, 5017900))
  expect_equal(tri2@crs, ctg@crs)

  expect_equal(tri1, tri2)
})

test_that("clip polygon works with all supported geometries on a LAS and LAScatalog", {

  # WKT
  wkt1 = "MULTIPOLYGON (((684950.8 5017989, 685003.3 5017962, 684938.5 5017905, 684950.8 5017989)), ((684796.2 5017963, 684921.6 5017977, 684899.2 5017806, 684780.7 5017795, 684796.2 5017963), (684899.4 5017924, 684851.7 5017945, 684863.7 5017857, 684899.4 5017924)))"
  wkt2 = "POLYGON ((684975.7 5017899, 685007.3 5017873, 684994.3 5017816, 684936.1 5017812, 684918.8 5017845, 684975.7 5017899))"

  mpoly1  = lasclip(las, wkt1)
  poly1   = lasclip(las, wkt2)
  mpoly2  = lasclip(ctg, wkt1, select = "xyx", filter = "-keep_first")
  poly2   = lasclip(ctg, wkt2, select = "xyx", filter = "-keep_first")

  expect_is(mpoly1, "LAS")
  expect_equal(nrow(mpoly1@data), 22520L)
  expect_equal(nrow(poly1@data), 4473L)
  expect_equal(mpoly1, mpoly2)
  expect_equal(poly1, poly2)
  expect_equal(mpoly1@crs, las@crs)
  expect_equal(mpoly2@crs, ctg@crs)

  # Polygon
  spatialpolygons1 = rgeos::readWKT(wkt1)
  spatialpolygons2 = rgeos::readWKT(wkt2)

  polygon1 = spatialpolygons1@polygons[[1]]@Polygons[[1]]

  poly1  = lasclip(las, polygon1)
  poly2  = lasclip(ctg, polygon1, select = "xyx", filter = "-keep_first")

  expect_is(poly1, "LAS")
  expect_equal(nrow(poly1@data), 2117L)
  expect_equal(poly1@crs, las@crs)
  expect_equal(poly2@crs, ctg@crs)

  expect_equal(poly1, poly2)

  # Polygons
  polygons1 = spatialpolygons1@polygons[[1]]

  poly1  = lasclip(las, polygons1)
  poly2  = lasclip(ctg, polygons1, select = "xyx", filter = "-keep_first")

  expect_is(poly1, "LAS")
  expect_equal(nrow(poly1@data), 22520L)
  expect_equal(poly1@crs, las@crs)
  expect_equal(poly2@crs, ctg@crs)

  expect_equal(poly1, poly2)

  # SpatialPolygonsDataFrame
  LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
  shapefile_dir <- system.file("extdata", package = "lidR")
  lakes = rgdal::readOGR(shapefile_dir, "lake_polygons_UTM17", verbose = F)

  poly1 = lasclip(las, lakes)
  poly2 = lasclip(ctg, lakes, select = "xyx", filter = "-keep_first")

  expect_is(poly1, "LAS")
  expect_equal(nrow(poly1@data), 6898L)
  expect_equal(poly1, poly2)

  # Extent
  bbox = extent(las)
  bbox = bbox - 100

  rect1 = lasclip(las, bbox)
  rect2 = lasclip(ctg, bbox, select = "xyx", filter = "-keep_first")

  expect_true(extent(rect1) <= bbox)
  expect_equal(rect1, rect2)

  # RasterLayer
  r = raster::raster(bbox, res = 1)

  rect1 = lasclip(las, r)
  rect2 = lasclip(ctg, r, select = "xyx", filter = "-keep_first")

  expect_true(extent(rect1) <= raster::extent(r))
  expect_equal(rect1, rect2)

  # Matrix 2x2
  m = raster::as.matrix(bbox)

  rect1 = lasclip(las, m)
  rect2 = lasclip(ctg, m, select = "xyx", filter = "-keep_first")

  expect_true(extent(rect1) <= bbox)
  expect_equal(rect1, rect2)
})

test_that("clip returns NULL if no point found in the query", {
  circ1 = suppressWarnings(lasclipCircle(las, 68480, 5017850, 10))
  circ2 = suppressWarnings(lasclipCircle(ctg, 68480, 5017850, 10))

  expect_true(is.null(circ1))
  expect_true(is.null(circ2))
})

test_that("clip throw a warning if the query is outside the bounding box", {
  expect_warning(lasclipCircle(las, 68480, 5017850, 10), "No point found")
  #expect_warning(lasclipCircle(ctg, 68480, 5017850, 10), "outside the catalog")
})


test_that("clip supports multiple queries", {

  # Multiple disc
  xc = c(684800, 684850)
  yc = c(5017850, 5017900)
  r  = 10

  circ1 = lasclipCircle(las, xc, yc, r)
  circ2 = lasclipCircle(ctg, xc, yc, r, select = "xyx", filter = "-keep_first")

  expect_is(circ1, "list")
  expect_equal(length(circ1), 2L)
  expect_equal(circ1, circ2)


  # Multiple rectangle
  xmin = 684850 + c(0,1)
  ymin = 5017850 + c(0,1)
  xmax = 684900 + c(0,1)
  ymax = 5017900 + c(0,1)
  rect1 = lasclipRectangle(las, xmin, ymin, xmax, ymax)
  rect2 = lasclipRectangle(ctg, xmin, ymin, xmax, ymax, select = "xyx", filter = "-keep_first")

  expect_is(rect1, "list")
  expect_equal(length(circ1), 2L)
  expect_equal(circ1, circ2)

  # Multiple polygons

  wkt1 = "MULTIPOLYGON (((684950.8 5017989, 685003.3 5017962, 684938.5 5017905, 684950.8 5017989)), ((684796.2 5017963, 684921.6 5017977, 684899.2 5017806, 684780.7 5017795, 684796.2 5017963), (684899.4 5017924, 684851.7 5017945, 684863.7 5017857, 684899.4 5017924)))"
  wkt2 = "MULTIPOLYGON (((684975.7 5017899, 685007.3 5017873, 684994.3 5017816, 684936.1 5017812, 684918.8 5017845, 684975.7 5017899)))"
  wkt  = glue::glue("GEOMETRYCOLLECTION({wkt1}, {wkt2})")
  spatialpolygons = rgeos::readWKT(wkt)

  polys1 = lasclip(las, spatialpolygons)
  polys2 = lasclip(ctg, spatialpolygons, select = "xyx", filter = "-keep_first")

  expect_is(polys1, "list")
  expect_equal(length(polys1), 2L)
  expect_equal(nrow(polys1[[1]]@data), 22520L)
  expect_equal(nrow(polys1[[2]]@data), 4473L)
  expect_equal(polys1[[1]]@crs, las@crs)
  expect_equal(polys1, polys2)
})

test_that("clip returns NULL for empty multiple queries", {

  # Multiple disc
  xc = c(684800, 68480)
  yc = c(5017850, 5017900)
  r  = 10

  circ1 = suppressWarnings(lasclipCircle(las, xc, yc, r))
  circ2 = suppressWarnings(lasclipCircle(ctg, xc, yc, r, select = "xyx", filter = "-keep_first"))

  expect_is(circ1, "list")
  expect_equal(length(circ1), 2L)
  expect_true(is.null(circ1[[2]]))
  expect_equal(circ1, circ2)


  # Multiple rectangle
  xmin = 684850 + c(0,-2000)
  ymin = 5017850 + c(0,-2000)
  xmax = 684900 + c(0,-2000)
  ymax = 5017900 + c(0,-2000)
  rect1 = suppressWarnings(lasclipRectangle(las, xmin, ymin, xmax, ymax))
  rect2 = suppressWarnings(lasclipRectangle(ctg, xmin, ymin, xmax, ymax, select = "xyx", filter = "-keep_first"))

  expect_is(rect1, "list")
  expect_equal(length(rect1), 2L)
  expect_true(is.null(rect1[[2]]))
  expect_equal(rect1, rect2)

  # Multiple polygons

  wkt1 = "MULTIPOLYGON (((684950.8 5017989, 685003.3 5017962, 684938.5 5017905, 684950.8 5017989)), ((684796.2 5017963, 684921.6 5017977, 684899.2 5017806, 684780.7 5017795, 684796.2 5017963), (684899.4 5017924, 684851.7 5017945, 684863.7 5017857, 684899.4 5017924)))"
  wkt2 = "MULTIPOLYGON (((68497 501789, 68500.3 501783, 68499 501786, 68496 501782, 68491 501784, 68497 501789)))"
  wkt  = glue::glue("GEOMETRYCOLLECTION({wkt1}, {wkt2})")
  spatialpolygons = rgeos::readWKT(wkt)

  polys1 = suppressWarnings(lasclip(las, spatialpolygons))
  polys2 = suppressWarnings(lasclip(ctg, spatialpolygons, select = "xyx", filter = "-keep_first"))

  expect_is(polys1, "list")
  expect_equal(length(polys1), 2L)
  expect_true(is.null(polys1[[2]]))
  expect_equal(polys1, polys2)
})


test_that("clip throws errors with invalid queries", {

  # Invalid WKT
  wkt = "POLGON ((684975.7 5017899, 685007.3 5017873, 684994.3 5017816, 684936.1 5017812, 684918.8 5017845, 684975.7 5017899))"
  expect_error(lasclip(las, wkt), "WKT is not")
  expect_error(lasclip(ctg, wkt), "Unable to parse")

  # Non supported WKT
  wkt = "MULTIPOINT (684975.7 5017899, 685007.3 5017873, 684994.3 5017816, 684936.1 5017812, 684918.8 5017845, 684975.7 5017899)"
  expect_error(lasclip(las, wkt), "WKT is not")
  expect_error(lasclip(ctg, wkt), "WKT not supported")

  # Different number of coordinates
  xc = c(684800)
  yc = c(5017850, 5017900)
  r  = 10
  expect_error(lasclipCircle(las, xc, yc, r), "are_same_length")
  expect_error(lasclipCircle(ctg, xc, yc, r), "are_same_length")

  xmin = 684850
  ymin = 5017850 + c(0,1)
  xmax = 684900 + c(0,1)
  ymax = 5017900 + c(0,1)
  expect_error(lasclipRectangle(las, xmin, ymin, xmax, ymax), "are_same_length")
  expect_error(lasclipRectangle(ctg, xmin, ymin, xmax, ymax), "are_same_length")

  # Wrong matrix dimension
  m = matrix(0, 3, 2)
  expect_error(lasclip(las, m), "2 x 2")
  expect_error(lasclip(ctg, m), "2 x 2")

  # Non supported object
  geom = 1
  class(geom) <- c("A",  "B")
  expect_error(lasclip(las, geom), "Geometry type A B not supported")
})
