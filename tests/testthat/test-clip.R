context("clip_roi")

las <- readLAS(example_las_path)
ctg <- example_ctg

opt_progress(ctg) = FALSE

test_that("clip_rectangle clips a rectangle both on a LAS and a LAScatalog", {

  rect1 <- clip_rectangle(las, 339006, 5248000, 339012, 5248002)
  rect2 <- clip_rectangle(ctg, 339006, 5248000, 339012, 5248002)

  #expect_true(extent(rect1) <= raster::extent(339006, 5248000, 339012, 5248002))
  expect_equal(rect1@crs, las@crs)
  expect_equal(npoints(rect1), 17L)
  expect_equal(rect1, rect2)
})

test_that("clip_circle clips a circle both on a LAS and a LAScatalog", {

  circ1 <- clip_circle(las, 339008, 5248001, 2)
  circ2 <- clip_circle(ctg, 339008, 5248001, 2)

  expect_equal(st_crs(circ1), st_crs(las))
  expect_equal(npoints(circ1), 15L)
  expect_equal(st_crs(circ2), st_crs(ctg))
  expect_equal(circ1, circ2)
})

test_that("clip_polygon clips a polygon both on a LAS and a LAScatalog", {

  tri1 <- clip_polygon(las, c(339008, 339010, 339010, 339008), c(5248000, 5248000, 5248002, 5248000))
  tri2 <- clip_polygon(ctg, c(339008, 339010, 339010, 339008), c(5248000, 5248000, 5248002, 5248000))

  expect_equal(st_crs(tri1), st_crs(las))
  expect_equal(npoints(tri1), 15L)
  expect_equal(st_crs(tri2), st_crs(ctg))
  expect_equal(tri1, tri2)
})

test_that("clip_roi memory optimization works", {

  las2 <- clip_polygon(las, c(0, 8e6, 8e6, 0), c(0, 5e8, 0, 0))
  las3 <- clip_circle(las, 339008, 5248001, 30)

  expect_reference(las@data, las2@data)
  expect_reference(las@data, las3@data)
})

test_that("clip_roi clips polygon works from WTK both on a LAS and LAScatalog", {

  wkt <- "POLYGON ((339008 5248000, 339010 5248000, 339010 5248002, 339008 5248000))"

  poly1   <- clip_roi(las, wkt)
  poly2   <- clip_roi(ctg, wkt)

  expect_is(poly1, "LAS")
  expect_equal(npoints(poly1), 15L)
  expect_equal(poly1, poly2)
  expect_equal(poly1@crs, las@crs)
})

test_that("clip_roi clips polygon works sfc", {

  wkt <- "POLYGON ((339008 5248000, 339010 5248000, 339010 5248002, 339008 5248000))"
  p <- sf::st_as_sfc(wkt)
  sf::st_crs(p) <- st_crs(las)

  poly1   <- las[p]
  poly2   <- clip_roi(las, p)

  expect_is(poly2, "LAS")
  expect_equal(npoints(poly2), 15L)
  expect_equal(poly1, poly2)
  expect_equal(poly1@crs, las@crs)
})

test_that("clip_roi clips multipolygon with hole", {

  wkt <- "POLYGON ((339008 5248000, 339010 5248000, 339010 5248002, 339008 5248000))"
  p1 <- sf::st_point(c(684850, 5017850))
  p2 <- sf::st_point(c(684900, 5017900))
  poly1e <- sf::st_buffer(p1, 20)
  poly1i <- sf::st_buffer(p1, 10)
  poly1 <- sf::st_difference(poly1e, poly1i)
  poly2 <- sf::st_buffer(p2, 20)
  poly <- c(poly1, poly2)

  donut <- clip_roi(megaplot, poly)

  expect_equal(npoints(donut), 3905)
})

test_that("clip_roi clips polygon works from sp polygons both on a LAS and LAScatalog", {

  skip_if_not_installed("sp")

  wkt1 <- "MULTIPOLYGON (((339010.5 5248000, 339012 5248000, 339010.5 5248002, 339010.5 5248000)), ((339008 5248000, 339010 5248000, 339010 5248002, 339008 5248000), (339008.5 5248000.2, 339009.5 5248000.2, 339009.5 5248001, 339008.5 5248000.2)))"
  wkt2 <- "POLYGON ((339008 5248000, 339010 5248000, 339010 5248002, 339008 5248000))"

  # SpatialPolygons
  spatialpolygons1 <- sf::as_Spatial(sf::st_as_sfc(wkt1))
  spatialpolygons2 <- sf::as_Spatial(sf::st_as_sfc(wkt2))

  polygon1 <- spatialpolygons1@polygons[[1]]@Polygons[[1]]
  poly1    <- clip_roi(las, polygon1)
  poly2    <- clip_roi(ctg, polygon1)

  expect_is(poly1, "LAS")
  expect_equal(npoints(poly1), 2L)
  expect_equal(st_crs(poly1), st_crs(las))
  expect_equal(st_crs(poly2), st_crs(ctg))
  expect_equal(poly1, poly2)
})

test_that("clip_roi clips point with SpatialPoints and sfc on LAS and LAScatalog", {

  xc <- c(339008, 339009)
  yc <- c(5248001, 5248001)
  xy <- data.frame(X = xc, Y = yc)
  r  <- 2

  p <- sf::st_as_sf(xy, coords = c("X", "Y"))
  p <- sf::st_geometry(p)

  #discs1 <- clip_roi(las, sf::as_Spatial(p), radius = r)
  discs2 <- clip_roi(las, p, radius = r)
  discs3 <- clip_roi(ctg, p, radius = r)
  discs4 <- clip_roi(ctg, p, radius = c(r,2))

  expect_is(discs2, "list")
  expect_equal(length(discs2), 2L)
  #expect_equal(discs1, discs2)
  expect_equal(discs2, discs3)
  expect_equal(discs2, discs4)
})

test_that("clip_transect clips a transect on LAS and LAScatalog", {

  p1  <- st_bbox(las)[1:2]
  p2  <- st_bbox(las)[3:4]
  tr1 <- clip_transect(las, p1, p2, 2, xz = FALSE)
  tr2 <- clip_transect(ctg, p1, p2, 2, xz = FALSE)

  expect_equal(npoints(tr1), 29L)
  #expect_equal(tr1, tr2)
})

test_that("clip_transect clips reorients the point-cloud on LAS and LAScatalog", {

  p1  <- st_bbox(las)[1:2]
  p2  <- st_bbox(las)[3:4]
  tr1 <- clip_transect(las, p1, p2, 2, xz = TRUE)
  tr2 <- clip_transect(ctg, p1, p2, 2, xz = TRUE)

  expect_equal(npoints(tr1), 29L)
  #expect_equal(tr1, tr2)
  expect_equal(mean(tr1$Y), 0, tol = 0.5)

  opt_output_files(ctg) <- tempfile()
  expect_error(clip_transect(ctg, p1, p2, 2, xz = TRUE), " not available yet")
})

test_that("clip_roi throw error with points and no radius", {

  xc <- c(684800, 684850)
  yc <- c(5017850, 5017900)
  xy <- data.frame(X = xc, Y = yc)
  r  <- 2
  p <- sf::st_as_sf(xy, coords = c("X", "Y"))
  p <- sf::st_geometry(p)

  expect_error(clip_roi(las, p), "requires addition of parameter 'radius'")
})

test_that("clip_roi throw error with lines", {

  l1 = cbind(c(1,2,3),c(3,2,2))
  l2 = cbind(c(1,2,3),c(1,1.5,1))
  Sl = sf::st_geometry(sf::st_linestring(l1))

  expect_error(clip_roi(las, Sl), "Incorrect geometry type")
})

test_that("clip_roi clips a rectangle from a bounding box both on a LAS and LAScatalog", {

  # Extent
  bbox <- sf::st_bbox(c(xmin = 339005.9, xmax = 339012.1, ymin = 5248000, ymax = 5248001))

  rect1 <- clip_roi(las, bbox)
  rect2 <- clip_roi(ctg, bbox)

  #expect_true(extent(rect1) <= bbox)
  expect_equal(npoints(rect1), 14L)
  expect_equal(rect1, rect2)
})


test_that("clip_* returns an empty point cloud if no point found in the query", {

  circ1 <- suppressWarnings(clip_circle(las, 68480, 5017850, 10))
  circ2 <- suppressWarnings(clip_circle(ctg, 68480, 5017850, 10))

  # Unit test for #400
  opt_output_files(ctg) <- tempfile()
  circ3 <- suppressWarnings(clip_circle(ctg, 68480, 5017850, 10))

  expect_true(is.empty(circ1))
  expect_true(is.empty(circ2))
  expect_equal(dim(circ3), c(0,35))
})

test_that("clip_* supports multiple queries", {

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

test_that("clip_* throw error for invalid multiple queries", {

  # Multiple disc
  xc <- c(684800, 684850)
  yc <- c(5017850, 5017900)
  r  <- 10:13

  expect_error(clip_circle(las, xc, yc, r), "xc and r have different lengths")
})

test_that("clip_* return an empty point-cloud for empty multiple queries", {

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

test_that("clip_* throw errors with invalid queries", {

  # Invalid WKT
  wkt <- "POLGON ((684975.7 5«017899, 685007.3 5017873, 684994.3 5017816, 684936.1 5017812, 684918.8 5017845, 684975.7 5017899))"

  expect_error(clip_roi(las, wkt), "OGR error")
  expect_error(clip_roi(ctg, wkt), "OGR error")

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

test_that("clip_* write file following LAScatalog options", {

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
  D = data.frame(X = xc, Y = yc, PlotID = paste0("plot", 1:2))
  P = sf::st_as_sf(D, coords = c("X", "Y"))

  opt_output_files(ctg2) <- paste0(tmp, "/{PlotID}")
  ctg3 = clip_roi(ctg2, P, radius = 2)
  expect_equal(normalizePath(ctg3@data$filename), normalizePath(paste0(tmp, "/plot", 1:2, ".las")))
})

test_that("clip_* throw an error with invalid template", {

  tmp  <- tempdir()

  ctg2 <- ctg
  opt_output_files(ctg2)    <- paste0(tmp, "/file_{1:3}")
  opt_laz_compression(ctg2) <- TRUE

  expect_error(clip_rectangle(ctg2,  339006, 5248000, 339012, 5248002), "Ill-formed template string in the catalog")

  opt_output_files(ctg2)    <- paste0(tmp, "/*")

  expect_error(clip_rectangle(ctg2,  339006, 5248000, 339012, 5248002), "undefined in clip functions")
})

test_that("clip_* respect spatial index metadata in LAScatalog", {
  xc <- 339008
  yc <- 5248001
  r  <- 2
  sensor(ctg) <- "tls"
  index(ctg) <- "octree"
  las = clip_circle(ctg, xc, yc, r)
  expect_equal(index(las), index(ctg))
  expect_equal(sensor(las), sensor(ctg))
})

