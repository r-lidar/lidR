las <- clip_rectangle(mixedconifer, 481270, 3812930, 481310, 3812970)
las <- filter_poi(las, Z >= 0)
ctg <- mixedconifer_ctg

n_expected <- sum(table(las$treeID) > 3)

opt_progress(ctg) <- FALSE
opt_chunk_size(ctg) <- 120
opt_chunk_alignment(ctg) <- c(50, -30)
opt_chunk_buffer(ctg) <- 20
opt_progress(ctg) <- FALSE

test_that("crown_metrics works", {
  hulls = crown_metrics(las, func = NULL)

  expect_is(hulls, "sf")
  expect_is(sf::st_geometry(hulls), "sfc_POINT")
  expect_equal(dim(hulls), c(n_expected,2))
})

test_that("crown_metrics works with bbox hulls", {
  hulls = crown_metrics(las, func = NULL, geom = "bbox")

  expect_is(hulls, "sf")
  expect_is(sf::st_geometry(hulls), "sfc_POLYGON")
  expect_equal(dim(hulls), c(n_expected,2))
})

test_that("crown_metrics works with concave hulls", {

  skip_on_cran()

  hulls = crown_metrics(las, func = NULL, geom = "concave")

  expect_is(hulls, "sf")
  expect_is(sf::st_geometry(hulls), "sfc_POLYGON")
  expect_equal(dim(hulls), c(n_expected,2))
})

test_that("crown_metrics returns NULL for no trees", {

  las@data$treeID <- NA_integer_

  expect_warning(res <- crown_metrics(las, func = NULL), "NULL returned")
  expect_true(is.null(res))
})

test_that("crown_metrics supports custom metrics", {

  hulls = crown_metrics(las, func = ~max(Z))

  expect_is(hulls, "sf")
  expect_equal(dim(hulls), c(n_expected,3))
  expect_equal(names(hulls)[1:2], c("treeID", "V1"))
  expect_true(!anyNA(hulls$V1))
})

test_that("crown_metrics works with a custom metrics", {

  hulls = crown_metrics(ctg, func = ~max(Z))

  expect_is(hulls, "sf")
  expect_is(sf::st_geometry(hulls), "sfc_POINT")
  expect_equal(dim(hulls), c(200, 3))

  hulls = crown_metrics(ctg, func = ~max(Z), geom = "bbox")

  expect_is(hulls, "sf")
  expect_is(sf::st_geometry(hulls), "sfc_POLYGON")
  expect_equal(dim(hulls), c(200,3))
})

test_that("crown_metrics throw an error if no treeID", {

  expect_error(tree_metrics(megaplot, list(`Mean Z` = mean(Z))), "not segmented")
})


test_that("delineate_crowns is backward compatible", {
  hulls <- delineate_crowns(las)

  expect_is(hulls, "SpatialPolygonsDataFrame")
  expect_equal(dim(hulls), c(n_expected,4))
})

test_that("tree_metrics is backward compatible", {

  metrics = tree_metrics(las, ~list(Z = max(Z), MeanZ = mean(Z), MaxI = max(Intensity)))

  expect_is(metrics, "SpatialPointsDataFrame")
  expect_equal(names(metrics), c("treeID", "Z", "MeanZ", "MaxI"))
  expect_equal(dim(metrics), c(n_expected,4))
  expect_true(sf::st_crs(metrics) == st_crs(las))

  metrics = tree_metrics(las, .stdtreemetrics)

  expect_is(metrics, "SpatialPointsDataFrame")
  expect_equal(nrow(metrics), n_expected)
  expect_true(sf::st_crs(metrics) == st_crs(las))
})

test_that("tree_metrics is backward compatible and works with a LAScatalog", {

  metrics = tree_metrics(ctg, ~list(Z = max(Z), MeanZ = mean(Z), MaxI = max(Intensity)))

  expect_is(metrics, "SpatialPointsDataFrame")
  expect_equal(names(metrics), c("treeID", "Z", "MeanZ", "MaxI"))
  expect_equal(dim(metrics), c(200,4))
  expect_true(sf::st_crs(metrics) == st_crs(las))

  skip_on_cran()

  metrics = tree_metrics(ctg, .stdtreemetrics)

  expect_is(metrics, "SpatialPointsDataFrame")
  expect_equal(nrow(metrics), 200)
  expect_true(sf::st_crs(metrics) == st_crs(las))
})


