context("lasfilter")

las = lidR:::dummy_las(500)

test_that("filter first return works", {
  expect_equal(dim(lasfilterfirst(las)@data)[1], 0.6*500)
})

test_that("filter firstlast return works", {
  expect_equal(dim(lasfilterfirstlast(las)@data)[1], 0.9*500)
})

test_that("filter ground works", {
  expect_equal(dim(lasfilterground(las)@data)[1], 0.2*500)
})

test_that("filter firstlast works", {
  expect_equal(dim(lasfilterlast(las)@data)[1], 0.6*500)
})

test_that("filter on non conditionnal statement return an error", {
  expect_error(lasfilter(las, y = 2))
})

test_that("clip rectangle works", {
  rect = lasclipRectangle(las, 10, 10, 50, 50)
  expect_true(extent(rect) <= raster::extent(10,50,10,50))
})

test_that("clip circle works", {
  circ = lasclipCircle(las, 50, 50, 10)
  expect_true(extent(circ) <= raster::extent(40,60,40,60))
})

test_that("filter on non matching data return null", {
  expect_warning(lasfilter(las, X > 200))
  expect_equal(suppressWarnings(lasfilter(las, X > 200)), NULL)
})

LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
ctg = catalog(LASfile)

test_that("clip circle works with a catalog", {
  circ = lasclipCircle(ctg, 684850, 5017850, 10)
  expect_true(extent(circ) <= raster::extent(684850-10,5017850-10,684850+10,5017850+10))
})

test_that("clip rectangle works with a catalog", {
  rect = lasclipRectangle(ctg, 684850, 5017850, 684900, 5017900)
  expect_true(extent(rect) <= raster::extent(684850, 5017850, 684900, 5017900))
})

test_that("clip polygon works with a catalog", {
  tri = lasclipPolygon(ctg, c(684850, 684900, 684975, 684850), c(5017850, 5017900, 5017800, 5017850))
  expect_true(extent(tri) <= raster::extent(684850, 5017800, 684975, 5017900))
})

LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
shapefile_dir <- system.file("extdata", package = "lidR")
lakes = rgdal::readOGR(shapefile_dir, "lake_polygons_UTM17", verbose = F)

test_that("clip works with a geometry", {
  poly = lakes@polygons[[1]]@Polygons[[1]]

  roi = lasclip(ctg, poly)
  expect_equal(nrow(roi@data), 7065)
})
