context("catalog_intersect")

data <- data.table::data.table(
  Max.X   = c(885228.88, 886993.96, 885260.93, 887025.96, 885292.94, 887056.88,
              892199.94, 893265.54, 892229.99, 893295.15, 888759.96, 890524.95,
              892259.98, 894025.98, 892289.96, 894055.93, 888790.91, 890554.98,
              888820.95, 890585.99, 892319.96, 894084.97, 892349.89, 894114.29,
              895250.23, 895094.78, 895044.96, 895053.55, 885323.96, 887087.95),
  Min.X   = c(885022.37, 885204.73, 885027.52, 885229.03, 885040.86, 885261.03,
              891503.09, 892198.69, 891501.42, 892200.07, 886970.07, 888735.55,
              891499.96, 892230.05, 890521.99, 892260.01, 886994.05, 888760.09,
              887026.07, 888791.01, 890525.05, 892290.04, 890555.01, 892320.12,
              894002.98, 894026.02, 894056.02, 894085.03, 885051.45, 885293.03),
  Max.Y   = c(630219.48, 630214.96, 631609.95, 631604.97, 633001.65, 632995.99,
              625898.35, 625882.94, 627289.82, 627273.89, 630174.88, 630134.94,
              628681.66, 628664.99, 630094.95, 630057.95, 631564.98, 631524.94,
              632955.82, 632915.99, 631486.90, 631447.96, 632876.93, 632838.96,
              628627.89, 630019.93, 631410.97, 631740.88, 634393.05, 634386.96),
  Min.Y   = c(629157.18, 629099.31, 630215.04, 630175.05, 631605.02, 631565.05,
              625816.52, 625793.60, 625883.01, 625860.81, 629036.82, 629017.72,
              627274.01, 627251.36, 628665.04, 628628.01, 630135.08, 630095.02,
              631525.01, 631487.19, 630058.02, 630020.05, 631448.08, 631411.03,
              627506.32, 628612.41, 629999.84, 631390.38, 632996.06, 632956.04),
  X.scale.factor = 0.01,
  Y.scale.factor = 0.01,
  filename = paste0("abc", 1:30)
)

geom <- lapply(1:nrow(data), function(i)
{
  mtx <- matrix(c(data$Min.X[i], data$Max.X[i], data$Min.Y[i], data$Max.Y[i])[c(1, 1, 2, 2, 1, 3, 4, 4, 3, 3)], ncol = 2)
  sf::st_polygon(list(mtx))
})

geom <-sf::st_sfc(geom)
sf::st_crs(geom) <- 26917
data <- sf::st_set_geometry(data, geom)

ctg       <- new("LAScatalog")
ctg@data  <- data

opt_progress(ctg) <- FALSE

# Build a polygon

polygon <- structure(
  c(888653.5, 890731.2, 891261.4, 889667.4,
    887754.0, 888653.5, 633572.9,  633244.7,
    630634.9, 629678.5, 631162.7, 633572.9),
  .Dim = c(6L, 2L))

polygon <- sf::st_polygon(list(polygon))
polygon <- sf::st_geometry(polygon)
polygon <- sf::st_set_crs(polygon, st_crs(ctg))

# Build a Raster
r <- raster::raster(raster::extent(sf::st_bbox(polygon)))
projection(r) <- projection(ctg)

# Build a SpatialPoints
pts <- structure(
  c(888653.5, 890731.2, 891261.4, 889667.4,
    887754.0, 888653.5, 633572.9,  633244.7,
    630634.9, 629678.5, 631162.7, 633572.9),
  .Dim = c(6L, 2L))
pts <- as.data.frame(pts)
names(pts) <- c("X", "Y")

pts <- sf::st_as_sf(pts, coords = c("X", "Y"), crs = st_crs(ctg))

test_that("catalog_intersect extract the tiles that lie in a SpatialPolygons", {

  ctg2 <- catalog_intersect(ctg, polygon)
  ctg22 <- catalog_intersect(ctg, sf::st_as_sf(polygon))


  expect_equal(ctg2$filename, c("abc12", "abc17", "abc18", "abc19", "abc20", "abc21", "abc23"))
  expect_equal(ctg2, ctg22)
})

test_that("catalog_intersect extracts the tiles that lie in the bbox of a Raster", {

  ctg2 <- suppressWarnings(catalog_intersect(ctg, r))
  ctg22 <- suppressWarnings(catalog_intersect(ctg, raster::extent(r)))

  expect_equal(ctg2$filename, c("abc11", "abc12", "abc15", "abc17", "abc18", "abc19", "abc20", "abc21", "abc23"))
  expect_equal(ctg2, ctg22)
})

test_that("catalog_intersect extracts the tiles that contains the points of a SpatialPoints", {

  ctg2 <- catalog_intersect(ctg, pts)
  ctg22 <- catalog_intersect(ctg, sf::st_as_sf(pts))

  expect_equal(sort(ctg2$filename), sort(c("abc12", "abc17", "abc21")))
  expect_equal(ctg2, ctg22)
})

