context("clip_roi")

las <- readLAS(example_las_path)
ctg <- example_ctg

opt_progress(ctg) = FALSE

test_that("clip_roi clips a rectangle both on a LAS and a LAScatalog", {

  rect1 <- clip_rectangle(las, 339006, 5248000, 339012, 5248002)
  rect2 <- clip_rectangle(ctg, 339006, 5248000, 339012, 5248002)

  expect_true(extent(rect1) <= raster::extent(339006, 5248000, 339012, 5248002))
  expect_equal(rect1@proj4string, las@proj4string)
  expect_equal(npoints(rect1), 17L)
  expect_equal(rect1, rect2)
})

test_that("clip_roi clips a circle both on a LAS and a LAScatalog", {

  circ1 <- clip_circle(las, 339008, 5248001, 2)
  circ2 <- clip_circle(ctg, 339008, 5248001, 2)

  expect_equal(circ1@proj4string, las@proj4string)
  expect_equal(npoints(circ1), 15L)
  expect_equal(circ2@proj4string, ctg@proj4string)
  expect_equal(circ1, circ2)
})

test_that("clip_roi clips a polygon both on a LAS and a LAScatalog", {

  tri1 <- clip_polygon(las, c(339008, 339010, 339010, 339008), c(5248000, 5248000, 5248002, 5248000))
  tri2 <- clip_polygon(ctg, c(339008, 339010, 339010, 339008), c(5248000, 5248000, 5248002, 5248000))

  expect_equal(tri1@proj4string, las@proj4string)
  expect_equal(npoints(tri1), 15L)
  expect_equal(tri2@proj4string, ctg@proj4string)
  expect_equal(tri1, tri2)
})

test_that("clip_roi memory optimization works", {

  las2 <- clip_polygon(las, c(0, 8e6, 8e6, 0), c(0, 5e8, 0, 0))
  las3 <- clip_circle(las, 339008, 5248001, 30)

  expect_reference(las@data, las2@data)
  expect_reference(las@data, las3@data)
})

test_that("clip_roi clips polygon works from WTK both on a LAS and LAScatalog", {

  wkt2 <- "POLYGON ((339008 5248000, 339010 5248000, 339010 5248002, 339008 5248000))"

  poly1   <- clip_roi(las, wkt2)
  poly2   <- clip_roi(ctg, wkt2)

  expect_is(poly1, "LAS")
  expect_equal(npoints(poly1), 15L)
  expect_equal(poly1, poly2)
  expect_equal(poly1@proj4string, las@proj4string)
})

test_that("clip_roi clips polygon works from sp polygons both on a LAS and LAScatalog", {

  wkt1 <- "MULTIPOLYGON (((339010.5 5248000, 339012 5248000, 339010.5 5248002, 339010.5 5248000)),
  ((339008 5248000, 339010 5248000, 339010 5248002, 339008 5248000),
  (339008.5 5248000.2, 339009.5 5248000.2, 339009.5 5248001, 339008.5 5248000.2)))"

  wkt2 <- "POLYGON ((339008 5248000, 339010 5248000, 339010 5248002, 339008 5248000))"

  # Polygon
  spatialpolygons1 <- rgeos::readWKT(wkt1)
  spatialpolygons2 <- rgeos::readWKT(wkt2)

  polygon1 <- spatialpolygons1@polygons[[1]]@Polygons[[1]]
  poly1    <- clip_roi(las, polygon1)
  poly2    <- clip_roi(ctg, polygon1)

  expect_is(poly1, "LAS")
  expect_equal(npoints(poly1), 2L)
  expect_equal(poly1@proj4string, las@proj4string)
  expect_equal(poly2@proj4string, ctg@proj4string)
  expect_equal(poly1, poly2)

  # Polygons
  polygons1 <- spatialpolygons1@polygons[[1]]
  poly1     <- clip_roi(las, polygons1)
  poly2     <- clip_roi(ctg, polygons1)

  expect_is(poly1, "LAS")
  expect_equal(npoints(poly1), 11L)
  expect_equal(poly1@proj4string, las@proj4string)
  expect_equal(poly2@proj4string, ctg@proj4string)
  expect_equal(poly1, poly2)
})

test_that("clip_roi clips point with SpatialPoints on LAS and LAScatalog", {

  xc <- c(339008, 339009)
  yc <- c(5248001, 5248001)
  r  <- 2

  p = sp::SpatialPoints(cbind(xc, yc))

  discs1 <- clip_roi(las, p, radius = r)
  discs2 <- clip_roi(ctg, p, radius = r)
  discs3 <- clip_roi(ctg, p, radius = c(r,2))

  expect_is(discs1, "list")
  expect_equal(length(discs1), 2L)
  expect_equal(discs1, discs2)
})

test_that("clip_roi clips a transect on LAS and LAScatalog", {

  p1 = bbox(las)[,1]
  p2 = bbox(las)[,2]
  tr1 = clip_transect(las, p1, p2, 2, xz = FALSE)
  tr2 = clip_transect(ctg, p1, p2, 2, xz = FALSE)

  expect_equal(npoints(tr1), 29L)
  expect_equal(tr1, tr2)
})

test_that("clip_roi clips reorients the point cloud on LAS and LAScatalog", {

  p1 = bbox(las)[,1]
  p2 = bbox(las)[,2]
  tr1 = clip_transect(las, p1, p2, 2, xz = TRUE)
  tr2 = clip_transect(ctg, p1, p2, 2, xz = TRUE)

  expect_equal(npoints(tr1), 29L)
  expect_equal(tr1, tr2)
  expect_equal(mean(tr1$Y), 0, tol = 0.5)

  opt_output_files(ctg) <- tempfile()
  expect_error(clip_transect(ctg, p1, p2, 2, xz = TRUE), " not available yet")
})

test_that("clip_roi throw error with points and no radius", {

  xc <- c(684800, 684850)
  yc <- c(5017850, 5017900)
  r  <- 10

  p = sp::SpatialPoints(cbind(xc, yc))

  expect_error(clip_roi(las, p), "requires addition of parameter 'radius'")
})

test_that("clip_roi throw error with lines", {

  l1 = cbind(c(1,2,3),c(3,2,2))
  l2 = cbind(c(1,2,3),c(1,1.5,1))
  Sl1 = Line(l1)
  Sl2 = Line(l2)
  S1 = Lines(list(Sl1), ID="a")
  S2 = Lines(list(Sl2), ID="b")
  Sl = SpatialLines(list(S1,S2))
  sfline = sf::st_as_sf(Sl)

  expect_error(clip_roi(las, S2), "Geometry type Lines not supported")
  expect_error(clip_roi(las, Sl), "Geometry type SpatialLines not supported")
  expect_error(clip_roi(las, sfline), "Incorrect geometry type")
})

test_that("clip_roi clips a rectangle from a bounding box both on a LAS and LAScatalog", {

  # Extent
  bbox <- extent(las)
  bbox <- bbox*0.5

  rect1 <- clip_roi(las, bbox)
  rect2 <- clip_roi(ctg, bbox)

  expect_true(extent(rect1) <= bbox)
  expect_equal(npoints(rect1), 6L)
  expect_equal(rect1, rect2)

  # Matrix 2x2
  m <- raster::as.matrix(bbox)

  rect1 <- clip_roi(las, m)
  rect2 <- clip_roi(ctg, m)

  expect_true(extent(rect1) <= bbox)
  expect_equal(npoints(rect1), 6L)
  expect_equal(rect1, rect2)
})

test_that("clip_roi returns an empty point cloud if no point found in the query", {

  circ1 <- suppressWarnings(clip_circle(las, 68480, 5017850, 10))
  circ2 <- suppressWarnings(clip_circle(ctg, 68480, 5017850, 10))

  # Unit test for #400
  opt_output_files(ctg) <- tempfile()
  circ3 <- suppressWarnings(clip_circle(ctg, 68480, 5017850, 10))

  expect_true(is.empty(circ1))
  expect_true(is.empty(circ2))
  expect_equal(dim(circ3), c(0,34))
})

test_that("clip_roi supports multiple queries", {

  # Multiple disc
  xc <- c(339008, 339009)
  yc <- c(5248001, 5248001)
  r  <- 2

  circ1 <- clip_circle(las, xc, yc, r)
  circ2 <- clip_circle(ctg, xc, yc, r)

  expect_is(circ1, "list")
  expect_equal(length(circ1), 2L)
  expect_equal(circ1, circ2)

  # Multiple rectangle
  xmin <- 339008 + c(0,0)
  ymin <- 5248000 + c(0,0)
  xmax <- 339008 + c(2,3)
  ymax <- 5248001 + c(2,3)

  rect1 <- clip_rectangle(las, xmin, ymin, xmax, ymax)
  rect2 <- clip_rectangle(ctg, xmin, ymin, xmax, ymax)

  expect_is(rect1, "list")
  expect_equal(length(rect1), 2L)
  expect_equal(rect1, rect2)
})

test_that("clip_roi throw error for invalid multiple queries", {

  # Multiple disc
  xc <- c(684800, 684850)
  yc <- c(5017850, 5017900)
  r  <- 10:13

  expect_error(clip_circle(las, xc, yc, r), "xc and r have different lengths")
})

test_that("clip_roi returns an empty point cloud for empty multiple queries", {

  # Multiple disc
  xc <- c(339008, 68480)
  yc <- c(5248001, 5017900)
  r  <- 10

  circ1 <- suppressWarnings(clip_circle(las, xc, yc, r))
  circ2 <- suppressWarnings(clip_circle(ctg, xc, yc, r))

  expect_is(circ1, "list")
  expect_equal(length(circ1), 2L)
  expect_equal(npoints(circ1[[1]]), 30)
  expect_true(is.empty(circ1[[2]]))
  expect_equal(circ1, circ2)

  # Multiple rectangle
  xmin <- 339008 + c(0,-2000)
  ymin <- 5248000 + c(0,-2000)
  xmax <- 3390010 + c(0,-2000)
  ymax <- 5248002 + c(0,-2000)

  rect1 <- suppressWarnings(clip_rectangle(las, xmin, ymin, xmax, ymax))
  rect2 <- suppressWarnings(clip_rectangle(ctg, xmin, ymin, xmax, ymax))

  expect_is(rect1, "list")
  expect_equal(length(rect1), 2L)
  expect_equal(npoints(rect1[[1]]), 20)
  expect_true(is.empty(rect1[[2]]))
  expect_equal(rect1, rect2)
})

test_that("clip_roi throws errors with invalid queries", {

  # Invalid WKT
  wkt <- "POLGON ((684975.7 5«017899, 685007.3 5017873, 684994.3 5017816, 684936.1 5017812, 684918.8 5017845, 684975.7 5017899))"

  expect_error(clip_roi(las, wkt), "Unable to parse")
  expect_error(clip_roi(ctg, wkt), "Unable to parse")

  # Different number of coordinates
  xc <- c(684800)
  yc <- c(5017850, 5017900)
  r  <- 10

  expect_error(clip_circle(las, xc, yc, r), "different lengths")
  expect_error(clip_circle(ctg, xc, yc, r), "different lengths")

  xmin <- 684850
  ymin <- 5017850 + c(0,1)
  xmax <- 684900 + c(0,1)
  ymax <- 5017900 + c(0,1)

  expect_error(clip_rectangle(las, xmin, ymin, xmax, ymax), "different lengths")
  expect_error(clip_rectangle(ctg, xmin, ymin, xmax, ymax), "different lengths")

  # Wrong matrix dimension
  m <- matrix(0, 3, 2)

  expect_error(clip_roi(las, m), "2 x 2")
  expect_error(clip_roi(ctg, m), "2 x 2")

  # Non supported object
  geom <- 1
  class(geom) <- c("A",  "B")
  expect_error(clip_roi(las, geom), "Geometry type A B not supported")
})

test_that("clip writes file following LAScatalog options", {

  tmp  <- tempdir()

  ctg2 <- ctg
  opt_output_files(ctg2)    <- paste0(tmp, "/file_{XLEFT}")
  opt_laz_compression(ctg2) <- TRUE

  ctg3 <- clip_rectangle(ctg2, 339006, 5248000, 339012, 5248002)

  expect_true(is(ctg3, "LAScatalog"))
  expect_equal(normalizePath(ctg3@data$filename), normalizePath(paste0(tmp, "/file_339006.laz")))

  file.remove(paste0(tmp, "/file_339006.laz"))

  #opt_output_files(ctg2)    <- paste0(tmp, "/file_{LAKENAME_1}")
  opt_laz_compression(ctg2) <- FALSE
  #ctg3 <- clip_roi(ctg2, lakes)

  #expect_equal(normalizePath(ctg3@data$filename), normalizePath(paste0(tmp, "/file_Havelock Lake.las")))

  #file.remove(paste0(tmp, "/file_Havelock Lake.las"))

  xc <- c(339008, 339009)
  yc <- c(5248001, 5248001)
  X = cbind(xc, yc)
  D = data.frame(PlotID = paste0("plot", 1:2))
  P = sp::SpatialPointsDataFrame(X, D)

  opt_output_files(ctg2) <- paste0(tmp, "/{PlotID}")
  ctg3 = clip_roi(ctg2, P, radius = 2)
  expect_equal(normalizePath(ctg3@data$filename), normalizePath(paste0(tmp, "/plot", 1:2, ".las")))
})

test_that("clip throw an error with invalid template", {

  tmp  <- tempdir()

  ctg2 <- ctg
  opt_output_files(ctg2)    <- paste0(tmp, "/file_{1:3}")
  opt_laz_compression(ctg2) <- TRUE

  expect_error(clip_rectangle(ctg2,  339006, 5248000, 339012, 5248002), "Ill-formed template string in the catalog")

  opt_output_files(ctg2)    <- paste0(tmp, "/*")

  expect_error(clip_rectangle(ctg2,  339006, 5248000, 339012, 5248002), "undefined in clip functions")
})

test_that("clip repects spatial index metadata in LAScatalog", {
  xc <- 339008
  yc <- 5248001
  r  <- 2
  sensor(ctg) <- "tls"
  index(ctg) <- "octree"
  las = clip_circle(ctg, xc, yc, r)
  expect_equal(index(las), index(ctg))
  expect_equal(sensor(las), sensor(ctg))
})

