context("catalog_apply automerge")

ctg <- lidR:::catalog_generator(2, 250)
ctg@output_options$drivers$Raster$param$overwrite = TRUE
ctg@output_options$drivers$Spatial$param$overwrite = TRUE
ctg@output_options$drivers$SimpleFeature$param$delete_dsn = TRUE

rtest <- function(cluster, layers = 1L) {
  las <- readLAS(cluster)
  if (is.empty(las)) return(NULL)
  r = lidR:::rOverlay(las, 4)
  r[] <- 1L
  r[55:56] <- NA
  if (layers > 1) {
    r <- raster::brick(r,r)
    crs(r) <- crs(las)
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
    return(sp::SpatialPointsDataFrame(head(lidR:::coordinates(las)), data.frame(u = 1:6)))
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

test_that("catalog_apply automerge works with in memory RastersLayer", {
  # No automerge option
  req1 <- catalog_apply(ctg, rtest)
  req1 <- lidR:::rMergeList(req1)

  # automerge option
  option <- list(automerge = TRUE)
  req2 <- catalog_apply(ctg, rtest, .options = option)

  expect_true(raster::inMemory(req1))
  expect_equal(names(req1), "layername1")
  expect_equal(crs(req1), crs(ctg))
  expect_is(req1, "RasterLayer")
  expect_is(req2, "RasterLayer")
  expect_equal(raster::extent(req1), raster::extent(0,100,0,200))
  expect_equal(sum(is.na(req1[])), 4L)

  expect_equal(req1, req2)
})

test_that("catalog_apply automerge works with in memory RastersBrick", {

  skip_on_cran()

  # No automerge option
  req1 <- catalog_apply(ctg, rtest, layers = 2L)
  req1 <- lidR:::rMergeList(req1)

  # automerge option
  option <- list(automerge = TRUE)
  req2 <- catalog_apply(ctg, rtest, layers = 2, .options = option)

  expect_true(raster::inMemory(req1))
  expect_equal(names(req1), c("layername1", "layername2"))
  expect_equal(crs(req1), crs(ctg))
  expect_is(req1, "RasterBrick")
  expect_equal(raster::extent(req1), raster::extent(0,100,0,200))
  expect_equal(sum(is.na(req1[])), 8L)

  expect_equal(req1, req2)
})

test_that("catalog_apply automerge works with on disk RastersLayer (VRT)", {

    opt_output_files(ctg) <- paste0(tempdir(), "/{ORIGINALFILENAME}")

    # automerge option
    option <- list(automerge = TRUE)
    req1 <- catalog_apply(ctg, rtest, .options = option)

    expect_true(!raster::inMemory(req1))
    #expect_equal(names(req1), "layername1")
    #expect_equal(crs(req1)@projargs, crs(ctg)@projargs) # patch for raster not updated with rgal 1.5-8
    expect_is(req1, "RasterLayer")
    expect_equal(raster::extent(req1), raster::extent(0,100,0,200))
    expect_equal(sum(is.na(req1[])), 4L)
})

test_that("catalog_apply automerge works with on disk RastersBrick (VRT)", {

    opt_output_files(ctg) <- paste0(tempdir(), "/{ORIGINALFILENAME}")

    # automerge option
    option <- list(automerge = TRUE)
    req1 <- catalog_apply(ctg, rtest, layers = 2, .options = option)

    expect_true(!raster::inMemory(req1))
    #expect_equal(names(req1), c("layername1", "layername2"))
    #expect_equal(crs(req1)@projargs, crs(ctg)@projargs) # patch for raster not updated with rgal 1.5-8
    expect_is(req1, "RasterBrick")
    expect_equal(raster::extent(req1), raster::extent(0,100,0,200))
    expect_equal(sum(is.na(req1[])), 8L)
})

test_that("catalog_apply automerge works with in memory SpatialPoints*", {

  skip_on_cran()

  option <- list(automerge = TRUE)
  req2 <- catalog_apply(ctg, sptest, .options = option)

  expect_is(req2, "SpatialPoints")
  expect_equal(req2@proj4string, crs(ctg))
  expect_equal(length(req2), 12L)

  option <- list(automerge = TRUE)
  req2 <- catalog_apply(ctg, sptest, DataFrame = TRUE, .options = option)

  expect_is(req2, "SpatialPointsDataFrame")
  expect_equal(req2@proj4string, crs(ctg))
  expect_equal(dim(req2), c(12L,1L))
})

test_that("catalog_apply automerge works with on disk SpatialPoints*", {

  skip_on_cran()

  opt_output_files(ctg) <- paste0(tempdir(), "/{ORIGINALFILENAME}")

  option <- list(automerge = TRUE)
  req3 <- catalog_apply(ctg, sptest, .options = option)

  expect_true(is.character(req3))
  expect_true(all(tools::file_ext(req3) == "shp"))
})

test_that("catalog_apply automerge works with in memory POINTS", {

  skip_on_cran()

  option <- list(automerge = TRUE)
  req2 <- catalog_apply(ctg, sftest, .options = option)

  expect_is(req2, "sf")
  #expect_equal(projection(req2), "+proj=tmerc +lat_0=0 +lon_0=-55.5 +k=0.9999 +x_0=304800 +y_0=0 +ellps=clrk66 +units=m +no_defs")
  expect_equal(nrow(req2), 12L)
})

test_that("catalog_apply automerge works with on disk POINTS*", {

  skip_on_cran()

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
  expect_equal(crs(req2), crs(ctg))
  expect_equal(npoints(req2), 12L)
})

test_that("catalog_apply automerge works with on disk LAS (LAScatalog)", {

  opt_output_files(ctg) <- paste0(tempdir(), "/{ORIGINALFILENAME}")

  option <- list(automerge = TRUE)
  req3 <- catalog_apply(ctg, lastest, .options = option)

  expect_is(req3, "LAScatalog")
  expect_equal(crs(req3), crs(ctg))
})

test_that("catalog_apply automerge works with in memory data.frame", {

  skip_on_cran()

  option <- list(automerge = TRUE)
  req2 <- catalog_apply(ctg, dftest, .options = option)

  expect_is(req2, "data.frame")
  expect_equal(nrow(req2), 12L)
})

test_that("catalog_apply automerge works with on disk data.frame", {

  skip_on_cran()

  opt_output_files(ctg) <- paste0(tempdir(), "/{ORIGINALFILENAME}")

  option <- list(automerge = TRUE)
  req3 <- catalog_apply(ctg, dftest, .options = option)

  expect_true(is.character(req3))
  expect_true(all(tools::file_ext(req3) == "txt"))
})

test_that("catalog_apply automerge does not fail with heterogeneous outputs", {

  test <- function(cluster) {
    if (raster::extent(cluster)@ymin > 80) return(list(0))
    return(data.frame(X = 1:3))
  }

  opt_wall_to_wall(ctg) <- FALSE
  option <- list(automerge = FALSE)

  expect_warning(req <- catalog_sapply(ctg, test, .options = option), "heterogeneous objects")
  expect_is(req, "list")
  expect_is(req[[1]], "data.frame")
  expect_is(req[[2]], "list")
})

test_that("catalog_apply automerge does not fail with unsupported objects outputs", {

  test <- function(cluster) {
    x = runif(5)
    y = runif(5)
    stats::lm(y~x)
  }

  opt_wall_to_wall(ctg) <- FALSE
  option <- list(automerge = FALSE)

  expect_warning(req <- catalog_sapply(ctg, test, .options = option), "unsupported objects")
  expect_is(req, "list")
  expect_is(req[[1]], "lm")
})

test_that("catalog_sapply is the same than apply with automerge", {

  skip_on_cran()

  option <- list(automerge = FALSE)
  req1 <- catalog_sapply(ctg, rtest)
  req2 <- catalog_sapply(ctg, rtest, .options = option)

  expect_true(raster::inMemory(req1))
  expect_equal(names(req1), "layername1")
  expect_equal(req1, req2)
})

test_that("catalog_apply automerge disabled with opt_merge = FALSE", {

    opt_merge(ctg) <- FALSE

    # automerge option
    option <- list(automerge = TRUE)
    req1 <- catalog_apply(ctg, rtest, .options = option)

    expect_true(is.list(req1))
    expect_true(is(req1[[1]], "RasterLayer"))

    opt_output_files(ctg) <- paste0(tempdir(), "/{ORIGINALFILENAME}")
    req2 <- catalog_apply(ctg, rtest, .options = option)

    expect_true(is.list(req2))
    expect_true(is.character(req2[[1]]))
})
