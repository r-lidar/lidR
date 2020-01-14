context("catalog_apply automerge")

ctg <- lidR:::catalog_generator(250)
ctg@output_options$drivers$Raster$param$overwrite = TRUE
ctg@output_options$drivers$Spatial$param$overwrite = TRUE
ctg@output_options$drivers$SimpleFeature$param$delete_dsn = TRUE

rtest <- function(cluster, layers = 1L) {
  las <- readLAS(cluster)
  if (is.empty(las)) return(NULL)
  r = lidR:::rOverlay(las, 2)
  r[] <- 1L
  r[55:56] <- NA
  if (layers > 1) {
    r <- raster::brick(r,r)
    layers <- 2L
  }
  names(r) <- paste0("layername", 1:layers)
  return(r)
}

sptest <- function(cluster, DataFrame = FALSE) {
  las <- readLAS(cluster)
  if (is.empty(las)) return(NULL)

  if (!DataFrame )
    return(sp::SpatialPoints(head(lidR:::coordinates(las))))
  else
    return(sp::SpatialPointsDataFrame(head(lidR:::coordinates(las)), data.frame(u = runif(6))))
}

sftest <- function(cluster) {
  x = sptest(cluster, TRUE)
  if (is.null(x)) return(NULL)
  return(sf::st_as_sf(x))
}

lastest <- function(cluster) {
  las <- readLAS(cluster)
  if (is.empty(las)) return(NULL)
  LAS(head(lidR:::coordinates3D(las)), las@header)
}

dftest <- function(cluster) {
  las <- readLAS(cluster)
  if (is.empty(las)) return(NULL)
  head(lidR:::coordinates3D(las))
}



vrt_supported <- FALSE
if (requireNamespace("gdalUtils", quietly = TRUE)) {
  gdalUtils::gdal_setInstallation()
  if (!is.null(getOption("gdalUtils_gdalPath")))
    vrt_supported <- TRUE
}


test_that("catalog_apply automerge works with in memory RastersLayer", {
  # No automerge option
  req1 <- catalog_apply(ctg, rtest)
  req1 <- lidR:::rMergeList(req1)

  # automerge option
  option <- list(automerge = TRUE)
  req2 <- catalog_apply(ctg, rtest, .options = option)

  expect_true(raster::inMemory(req1))
  expect_equal(names(req1), "layername1")
  expect_equal(projection(req1), "+proj=tmerc +lat_0=0 +lon_0=-55.5 +k=0.9999 +x_0=304800 +y_0=0 +ellps=clrk66 +units=m +no_defs")
  expect_is(req1, "RasterLayer")
  expect_is(req2, "RasterLayer")
  expect_equal(raster::extent(req1), raster::extent(0,200,0,200))
  expect_equal(sum(is.na(req1[])), 8L)

  expect_equal(req1, req2)
})

test_that("catalog_apply automerge works with in memory RastersBrick", {
  # No automerge option
  req1 <- catalog_apply(ctg, rtest, layers = 2L)
  req1 <- lidR:::rMergeList(req1)

  # automerge option
  option <- list(automerge = TRUE)
  req2 <- catalog_apply(ctg, rtest, layers = 2, .options = option)

  expect_true(raster::inMemory(req1))
  expect_equal(names(req1), c("layername1", "layername2"))
  expect_equal(projection(req1), "+proj=tmerc +lat_0=0 +lon_0=-55.5 +k=0.9999 +x_0=304800 +y_0=0 +ellps=clrk66 +units=m +no_defs")
  expect_is(req1, "RasterBrick")
  expect_equal(raster::extent(req1), raster::extent(0,200,0,200))
  expect_equal(sum(is.na(req1[])), 16L)

  expect_equal(req1, req2)
})

test_that("catalog_apply automerge works with on disk RastersLayer (VRT)", {

  if (vrt_supported)
  {
    opt_output_files(ctg) <- paste0(tempdir(), "/{ORIGINALFILENAME}")

    # automerge option
    option <- list(automerge = TRUE)
    req1 <- catalog_apply(ctg, rtest, .options = option)

    expect_true(!raster::inMemory(req1))
    #expect_equal(names(req1), "layername1")
    expect_equal(projection(req1), "+proj=tmerc +lat_0=0 +lon_0=-55.5 +k=0.9999 +x_0=304800 +y_0=0 +ellps=clrk66 +units=m +no_defs")
    expect_is(req1, "RasterLayer")
    expect_equal(raster::extent(req1), raster::extent(0,200,0,200))
    expect_equal(sum(is.na(req1[])), 8L)
  }
})

test_that("catalog_apply automerge works with on disk RastersBrick (VRT)", {

  if (vrt_supported)
  {
    opt_output_files(ctg) <- paste0(tempdir(), "/{ORIGINALFILENAME}")

    # automerge option
    option <- list(automerge = TRUE)
    req1 <- catalog_apply(ctg, rtest, layers = 2, .options = option)

    expect_true(!raster::inMemory(req1))
    #expect_equal(names(req1), c("layername1", "layername2"))
    expect_equal(projection(req1), "+proj=tmerc +lat_0=0 +lon_0=-55.5 +k=0.9999 +x_0=304800 +y_0=0 +ellps=clrk66 +units=m +no_defs")
    expect_is(req1, "RasterBrick")
    expect_equal(raster::extent(req1), raster::extent(0,200,0,200))
    expect_equal(sum(is.na(req1[])), 16L)
  }
})

test_that("catalog_apply automerge works with in memory SpatialPoints*", {

  option <- list(automerge = TRUE)
  req2 <- catalog_apply(ctg, sptest, .options = option)

  expect_is(req2, "SpatialPoints")
  expect_equal(projection(req2), "+proj=tmerc +lat_0=0 +lon_0=-55.5 +k=0.9999 +x_0=304800 +y_0=0 +ellps=clrk66 +units=m +no_defs")
  expect_equal(length(req2), 24L)

  option <- list(automerge = TRUE)
  req2 <- catalog_apply(ctg, sptest, DataFrame = TRUE, .options = option)

  expect_is(req2, "SpatialPointsDataFrame")
  expect_equal(projection(req2), "+proj=tmerc +lat_0=0 +lon_0=-55.5 +k=0.9999 +x_0=304800 +y_0=0 +ellps=clrk66 +units=m +no_defs")
  expect_equal(dim(req2), c(24L,1L))
})

test_that("catalog_apply automerge works with on disk SpatialPoints*", {

  opt_output_files(ctg) <- paste0(tempdir(), "/{ORIGINALFILENAME}")

  option <- list(automerge = TRUE)
  req3 <- catalog_apply(ctg, sptest, .options = option)

  expect_true(is.character(unlist(req3)))
  expect_true(all(tools::file_ext(req3) == "shp"))
})

test_that("catalog_apply automerge works with in memory POINTS", {

  option <- list(automerge = TRUE)
  req2 <- catalog_apply(ctg, sftest, .options = option)

  expect_is(req2, "sf")
  expect_equal(projection(req2), "+proj=tmerc +lat_0=0 +lon_0=-55.5 +k=0.9999 +x_0=304800 +y_0=0 +ellps=clrk66 +units=m +no_defs")
  expect_equal(nrow(req2), 24L)
})

test_that("catalog_apply automerge works with on disk POINTS*", {

  opt_output_files(ctg) <- paste0(tempdir(), "/{ORIGINALFILENAME}")

  option <- list(automerge = TRUE)
  req3 <- catalog_apply(ctg, sftest, .options = option)

  expect_true(is.character(unlist(req3)))
  expect_true(all(tools::file_ext(req3) == "shp"))
})

test_that("catalog_apply automerge works with in memory LAS", {

  option <- list(automerge = TRUE)
  req2 <- catalog_apply(ctg, lastest, .options = option)

  expect_is(req2, "LAS")
  expect_equal(projection(req2), "+proj=tmerc +lat_0=0 +lon_0=-55.5 +k=0.9999 +x_0=304800 +y_0=0 +ellps=clrk66 +units=m +no_defs")
  expect_equal(npoints(req2), 24L)
})

test_that("catalog_apply automerge works with on disk LAS (LAScatalog)", {

  opt_output_files(ctg) <- paste0(tempdir(), "/{ORIGINALFILENAME}")

  option <- list(automerge = TRUE)
  req3 <- catalog_apply(ctg, lastest, .options = option)

  expect_is(req3, "LAScatalog")
  expect_equal(projection(req3), "+proj=tmerc +lat_0=0 +lon_0=-55.5 +k=0.9999 +x_0=304800 +y_0=0 +ellps=clrk66 +units=m +no_defs")
})

test_that("catalog_apply automerge works with in memory data.frame", {

  option <- list(automerge = TRUE)
  req2 <- catalog_apply(ctg, dftest, .options = option)

  expect_is(req2, "data.frame")
  expect_equal(nrow(req2), 24L)
})

test_that("catalog_apply automerge works with on disk data.frame", {

  opt_output_files(ctg) <- paste0(tempdir(), "/{ORIGINALFILENAME}")

  option <- list(automerge = TRUE)
  req3 <- catalog_apply(ctg, dftest, .options = option)

  expect_true(is.character(unlist(req3)))
  expect_true(all(tools::file_ext(req3) == "txt"))
})

test_that("catalog_apply automerge do not fails with heterogeneous outputs", {

  test <- function(cluster) {
    if (raster::extent(cluster)@ymin > 80) return(list(0))
    return(data.frame(X = 1:3))
  }

  opt_wall_to_wall(ctg) <- FALSE
  option <- list(automerge = FALSE)
  req <- suppressWarnings(catalog_sapply(ctg, test, .options = option))

  expect_warning(catalog_sapply(ctg, test, .options = option), "heterogeneous objects")
  expect_is(req, "list")
  expect_is(req[[1]], "data.frame")
  expect_is(req[[3]], "list")
})

test_that("catalog_apply automerge do not fails with unsupported objects outputs", {

  test <- function(cluster) {
    x = runif(5)
    y = runif(5)
    stats::lm(y~x)
  }

  opt_wall_to_wall(ctg) <- FALSE
  option <- list(automerge = FALSE)
  req <- suppressWarnings(catalog_sapply(ctg, test, .options = option))

  expect_warning(catalog_sapply(ctg, test, .options = option), "unsupported objects")
  expect_is(req, "list")
  expect_is(req[[1]], "lm")
})

test_that("catalog_sapply is the same than apply with automerge", {

  option <- list(automerge = FALSE)
  req1 <- catalog_sapply(ctg, rtest)
  req2 <- catalog_sapply(ctg, rtest, .options = option)

  expect_true(raster::inMemory(req1))
  expect_equal(names(req1), "layername1")
  expect_equal(req1, req2)
})
