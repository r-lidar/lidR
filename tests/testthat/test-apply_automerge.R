ctg <- random_2files_250points
ctg@output_options$drivers$Raster$param$overwrite = TRUE
ctg@output_options$drivers$Spatial$param$overwrite = TRUE
ctg@output_options$drivers$SimpleFeature$param$delete_dsn = TRUE

rtest <- function(cluster, layers = 1L) {
  las <- readLAS(cluster)
  if (is.empty(las)) return(NULL)
  r = lidR:::raster_layout(las, 4, format = "raster")
  r[] <- 1
  r[55:56] <- NA
  if (layers > 1) {
    r <- raster::brick(r,r)
    raster::crs(r) <- as(st_crs(las), "CRS")
    layers <- 2L
  }
  lidR:::raster_names(r) <- paste0("layername", 1:layers)
  return(r)
}

starstest <- function(cluster, layers = 1L) {
  las <- readLAS(cluster)
  if (is.empty(las)) return(NULL)
  r = lidR:::raster_layout(las, 4, format = "stars")
  r[[1]][] <- 1
  r[[1]][55:56] <- NA
  if (layers > 1) {
    r <- c(r,r, along = 3)
    sf::st_crs(r) <- st_crs(las)
    layers <- 2L
  }

  lidR:::raster_names(r) <- paste0("layername", 1:layers)
  return(r)
}

sftest <- function(cluster, DataFrame = TRUE) {
  las <- readLAS(cluster)
  if (is.empty(las)) return(NULL)
  D <- head(lidR:::coordinates(las))
  D <- sf::st_as_sf(D, coords = c("X", "Y"), crs = cluster@crs)
  D$u <- 1:6

  if (!DataFrame )
    return(sf::st_geometry(D))
  else
    return(D)
}

sptest <- function(cluster, DataFrame = FALSE) {
  x = sftest(cluster, DataFrame)
  if (is.null(x)) return(NULL)
  return(sf::as_Spatial(x))
}

lastest <- function(cluster) {
  las <- readLAS(cluster)
  if (is.empty(las)) return(NULL)
  las[1:6]
}

dftest <- function(cluster) {
  las <- readLAS(cluster)
  if (is.empty(las)) return(NULL)
  head(lidR:::coordinates3D(las))
}

terratest <- function(cluster, DataFrame = FALSE) {
  x <- sftest(cluster, DataFrame)
  if (is.null(x)) return(NULL)
  v <- terra::vect(x)
  terra::crs(v) <- sf::st_crs(x)$wkt
  return(v)
}

expected_bbox <- sf::st_bbox(c(xmin = 0,xmax = 100, ymin =  0, ymax = 200))

test_that("catalog_apply automerge works with in memory RasterLayer", {
  # No automerge option
  req1 <- catalog_apply(ctg, rtest)

  # automerge option
  option <- list(automerge = TRUE)
  req2 <- catalog_apply(ctg, rtest, .options = option)

  expect_is(req1, "list")
  expect_is(req2, "RasterLayer")
  expect_true(raster::inMemory(req2))
  expect_equal(lidR:::raster_names(req2), "layername1")
  expect_equivalent(crs(req2), crs(ctg))
  expect_equivalent(sf::st_bbox(req2), expected_bbox)
  expect_equal(sum(is.na(req2[])), 4L)
})

test_that("catalog_apply automerge works with in memory RastersBrick", {

  skip_on_cran()

  # No automerge option
  req1 <- catalog_apply(ctg, rtest, layers = 2L)

  # automerge option
  option <- list(automerge = TRUE)
  req2 <- catalog_apply(ctg, rtest, layers = 2, .options = option)

  expect_is(req1, "list")
  expect_is(req2, "RasterBrick")
  expect_true(raster::inMemory(req2))
  expect_equal(lidR:::raster_names(req2), c("layername1", "layername2"))
  expect_equivalent(sf::st_bbox(req2), expected_bbox)
  expect_equal(sum(is.na(req2[])), 8L)
})

test_that("catalog_apply automerge works with on disk rasters as Raster* (VRT)", {

    opt_output_files(ctg) <- paste0(tempdir(), "/{ORIGINALFILENAME}_{lidR:::uuid()}")
    option <- list(automerge = T)

    req1 <- catalog_apply(ctg, rtest, .options = option)

    expect_is(req1, "RasterLayer")
    expect_true(!raster::inMemory(req1))
    expect_equivalent(sf::st_bbox(req1), expected_bbox)
    expect_equal(sum(is.na(req1[])), 4L)
    expect_equal(lidR:::raster_names(req1), "layername1")

    req1 <- catalog_apply(ctg, rtest, layers = 2, .options = option)

    expect_is(req1, "RasterBrick")
    expect_true(!raster::inMemory(req1))
    expect_equivalent(sf::st_bbox(req1), expected_bbox)
    expect_equal(sum(is.na(req1[])), 8L)
    expect_equal(lidR:::raster_names(req1), c("layername1", "layername2"))
})

test_that("catalog_apply automerge works with on disk rasters as stars (VRT)", {

  opt_output_files(ctg) <- paste0(tempdir(), "/{ORIGINALFILENAME}_{lidR:::uuid()}")
  option <- list(automerge = TRUE)

  req1 <- catalog_apply(ctg, starstest, .options = option)
  res <- stars::read_stars(req1[[1]])

  expect_is(req1, "stars_proxy")
  expect_equivalent(sf::st_bbox(req1), expected_bbox)
  expect_equal(sum(is.na(res[[1]])), 4L)
  expect_equal(lidR:::raster_names(req1), "layername1")

  req1 <- catalog_apply(ctg, starstest, layers = 2, .options = option)
  res  <- stars::read_stars(req1[[1]])

  expect_is(req1, "stars_proxy")
  expect_equivalent(sf::st_bbox(req1), expected_bbox)
  expect_equal(sum(is.na(res[[1]])), 8L)
  expect_equal(lidR:::raster_names(req1), c("layername1", "layername2"))
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

  opt_output_files(ctg) <- paste0(tempdir(), "/{ORIGINALFILENAME}_{lidR:::uuid()}")

  option <- list(automerge = TRUE)
  req3 <- catalog_apply(ctg, sptest, .options = option)

  expect_true(is.character(req3))
  expect_true(all(tools::file_ext(req3) == "shp"))
})

test_that("catalog_apply automerge works with on disk SpatVector", {

  skip_on_cran()

  opt_output_files(ctg) <- ""

  option <- list(automerge = T)
  req3 <- catalog_apply(ctg, terratest, .options = option)

  expect_is(req3, "SpatVector")
  expect_false(terra::crs(req3) == "")

  opt_output_files(ctg) <- paste0(tempdir(), "/{ORIGINALFILENAME}_{lidR:::uuid()}")

  option <- list(automerge = TRUE)
  req3 <- catalog_apply(ctg, terratest, .options = option)

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

  opt_output_files(ctg) <- paste0(tempdir(), "/{ORIGINALFILENAME}_{lidR:::uuid()}")

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

  opt_output_files(ctg) <- paste0(tempdir(), "/{ORIGINALFILENAME}_{lidR:::uuid()}")

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

  opt_output_files(ctg) <- paste0(tempdir(), "/{ORIGINALFILENAME}_{lidR:::uuid()}")

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

  expect_is(req1, "RasterLayer")
  expect_true(raster::inMemory(req1))
  expect_equal(lidR:::raster_names(req1), "layername1")
  expect_equal(req1, req2)
})

test_that("catalog_apply automerge disabled with opt_merge = FALSE", {

  skip_on_cran()

  opt_merge(ctg) <- FALSE
  option <- list(automerge = TRUE)
  req1 <- catalog_apply(ctg, rtest, .options = option)

  expect_true(is.list(req1))
  expect_true(is(req1[[1]], "RasterLayer"))

  opt_output_files(ctg) <- paste0(tempdir(), "/{ORIGINALFILENAME}_{lidR:::uuid()}")
  req2 <- catalog_apply(ctg, rtest, .options = option)

  expect_true(is.list(req2))
  expect_true(is.character(req2[[1]]))
})

