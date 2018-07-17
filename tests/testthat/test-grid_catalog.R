context("grid_catalog")

file <- system.file("extdata", "Megaplot.laz", package="lidR")
ctg = catalog(file)
las = readLAS(file)
cores(ctg) <- 1
tiling_size(ctg) <- 160
buffer(ctg) <- 0
progress(ctg) <- FALSE

test_that("grid_canopy returns the same both with catalog and las", {
  chm1 = grid_canopy(ctg)
  chm2 = grid_canopy(las)
  data.table::setorder(chm1, X, Y )
  data.table::setorder(chm2, X, Y )
  expect_equal(chm1, chm2)
})

test_that("grid_density returns the same both with catalog and las", {
  d1 = grid_density(ctg)
  d2 = grid_density(las)
  data.table::setorder(d1, X, Y )
  data.table::setorder(d2, X, Y )
  expect_equal(d1, d2)
})

test_that("grid_metric return the same both with catalog and las", {
  m1 = grid_metrics(ctg, .stdmetrics_z, 20)
  m2 = grid_metrics(las, .stdmetrics_z, 20)
  data.table::setorder(m1, X, Y )
  data.table::setorder(m2, X, Y )
  expect_equal(m1, m2)
})

test_that("grid_metric return the same both with catalog and las + grid alignment", {
  m1 = grid_metrics(ctg, .stdmetrics_z, 20, start = c(10,10))
  m2 = grid_metrics(las, .stdmetrics_z, 20, start = c(10,10))
  data.table::setorder(m1, X, Y )
  data.table::setorder(m2, X, Y )
  expect_equal(m1, m2)
})


file <- system.file("extdata", "Topography.laz", package="lidR")
ctg = catalog(file)
las = readLAS(file)
cores(ctg) <- 1
tiling_size(ctg) <- 180
buffer(ctg) <- 35
progress(ctg) <- FALSE

test_that("grid_terrain returns the same both with catalog and las", {
  t1 = grid_terrain(ctg, 2, "knnidw", k = 5)
  t2 = grid_terrain(las, 2, "knnidw", k = 5)
  data.table::setorder(t1, X, Y )
  data.table::setorder(t2, X, Y )
  expect_equal(t1, t2, check.attributes = F)

  t1 = suppressMessages(grid_terrain(ctg, 2, "delaunay"))
  t2 = suppressMessages(grid_terrain(las, 2, "delaunay"))

  tr1 = as.raster(t1)
  tr2 = as.raster(t2)

  tr1 = raster::crop(tr1, raster::extent(tr1)-2)
  tr2 = raster::crop(tr2, tr1)

  diff = abs(raster::values(tr1-tr2))
  expect_lt(mean(diff, na.rm = TRUE), 0.01)
})


test_that("grid_canopy returns a VRT", {
    # Run only on a local machine, CRAN does not like it
    if (file.exists(".Rbuildignore"))
    {
      vrt(ctg) <- tempdir()
      buffer(ctg) <- 0
      tiling_size(ctg) <- 180

      chm1 = grid_canopy(ctg)
      chm2 = as.raster(grid_canopy(las))

      expect_true(is(chm1, "RasterStack"))
      expect_true(object.size(chm1) < object.size(chm2))
    }
})

test_that("grid_catalog generic function works", {

  f = function(x, res){ grid_metrics(x, mean(Z), res) }

  X = grid_catalog(ctg, f, res = 20, select = "xyz")
  Y = grid_metrics(las, mean(Z), 20)

  expect_equal(dim(X), dim(Y))

  f = function(x, res){ 2 }

  expect_error(grid_catalog(ctg, f, res = 20, select = "xyz"), "data type")
})

#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' shapefile_dir <- system.file("extdata", package = "lidR")
#'
#' ctg = catalog(LASfile)
#' tiling_size(ctg) <- 160
#'
#' lakes = rgdal::readOGR(shapefile_dir, "lake_polygons_UTM17")
#'
#' my_grid_metrics = function(x, res, spdf)
#' {
#'   lasclassify(x, spdf, "inpoly")
#'   x = lasfilter(x, !inpoly)
#'   grid_metrics(x, mean(Z), res)
#' }
#'
#' mean = grid_catalog(ctg, my_grid_metrics, 20,
#'                     select = "xyz", filter = "-drop_z_below 5",
#'                     spdf = lakes)


