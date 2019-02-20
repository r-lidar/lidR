context("catalog_makeclusters")

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
  filename = paste0("abc", 1:30)
)


pgeom <- lapply(1:nrow(data), function(i)
{
  mtx <- matrix(c(data$Min.X[i], data$Max.X[i], data$Min.Y[i], data$Max.Y[i])[c(1, 1, 2, 2, 1, 3, 4, 4, 3, 3)], ncol = 2)
  sp::Polygons(list(sp::Polygon(mtx)),as.character(i))
})

Sr <- sp::SpatialPolygons(pgeom, proj4string = sp::CRS("+init=epsg:26917"))

ctg             <- new("LAScatalog")
ctg@bbox        <- Sr@bbox
ctg@proj4string <- Sr@proj4string
ctg@plotOrder   <- Sr@plotOrder
ctg@data        <- data
ctg@polygons    <- Sr@polygons

opt_progress(ctg) <- FALSE

test_that("catalog_makecluster makes correct clusters", {

  opt_chunk_size(ctg)   <-  800
  opt_chunk_buffer(ctg) <- 0

  cl <- lidR:::catalog_makecluster(ctg)

  width   <- sapply(cl, function(x) x@width)
  buffer  <- sapply(cl, function(x) x@buffer)
  xwidth  <- sapply(cl, function(x) x@bbox[3] - x@bbox[1])
  ywidth  <- sapply(cl, function(x) x@bbox[4] - x@bbox[2])
  xbwidth <- sapply(cl, function(x) x@bbbox[3] - x@bbbox[1])
  ybwidth <- sapply(cl, function(x) x@bbbox[4] - x@bbbox[2])
  xbuffer <- sapply(cl, function(x) x@bbbox[3] - x@bbox[3])
  ybuffer <- sapply(cl, function(x) x@bbbox[4] - x@bbox[4])

  expect_equal(length(cl), 98)
  expect_true(all(width == 800))
  expect_true(all(xwidth == 800))
  expect_true(all(ywidth == 800))
  expect_true(all(xbwidth == 800))
  expect_true(all(ybwidth == 800))
  expect_true(all(buffer == 0))
  expect_true(all(xbuffer == 0))
  expect_true(all(ybuffer == 0))
})

test_that("catalog_makecluster makes correct clusters with buffer", {

  opt_chunk_size(ctg)   <- 800
  opt_chunk_buffer(ctg) <- 50

  cl <- lidR:::catalog_makecluster(ctg)

  width   <- sapply(cl, function(x) x@width)
  buffer  <- sapply(cl, function(x) x@buffer)
  xwidth  <- sapply(cl, function(x) x@bbox[3] - x@bbox[1])
  ywidth  <- sapply(cl, function(x) x@bbox[4] - x@bbox[2])
  xbwidth <- sapply(cl, function(x) x@bbbox[3] - x@bbbox[1])
  ybwidth <- sapply(cl, function(x) x@bbbox[4] - x@bbbox[2])
  xbuffer <- sapply(cl, function(x) x@bbbox[3] - x@bbox[3])
  ybuffer <- sapply(cl, function(x) x@bbbox[4] - x@bbox[4])

  expect_equal(length(cl), 98)
  expect_true(all(width == 900))
  expect_true(all(xwidth == 800))
  expect_true(all(ywidth == 800))
  expect_true(all(xbwidth == 900))
  expect_true(all(ybwidth == 900))
  expect_true(all(buffer == 50))
  expect_true(all(xbuffer == 50))
  expect_true(all(ybuffer == 50))
})

test_that("catalog_makecluster makes correct clusters with negative buffer", {

  opt_chunk_size(ctg)   <- 800
  opt_chunk_buffer(ctg) <- -100

  cl <- lidR:::catalog_makecluster(ctg)

  width   <- sapply(cl, function(x) x@width)
  buffer  <- sapply(cl, function(x) x@buffer)
  xwidth  <- sapply(cl, function(x) x@bbox[3] - x@bbox[1])
  ywidth  <- sapply(cl, function(x) x@bbox[4] - x@bbox[2])
  xbwidth <- sapply(cl, function(x) x@bbbox[3] - x@bbbox[1])
  ybwidth <- sapply(cl, function(x) x@bbbox[4] - x@bbbox[2])
  xbuffer <- sapply(cl, function(x) x@bbbox[3] - x@bbox[3])
  ybuffer <- sapply(cl, function(x) x@bbbox[4] - x@bbox[4])

  expect_equal(length(cl), 92)
  expect_true(all(width == 600))
})

test_that("catalog_makecluster makes correct clusters by file", {

  opt_chunk_size(ctg)   <- 0
  opt_chunk_buffer(ctg) <- 0

  cl <- lidR:::catalog_makecluster(ctg)

  width   <- sapply(cl, function(x) x@width)
  buffer  <- sapply(cl, function(x) x@buffer)
  xwidth  <- sapply(cl, function(x) x@bbox[3] - x@bbox[1])
  ywidth  <- sapply(cl, function(x) x@bbox[4] - x@bbox[2])
  xbwidth <- sapply(cl, function(x) x@bbbox[3] - x@bbbox[1])
  ybwidth <- sapply(cl, function(x) x@bbbox[4] - x@bbbox[2])
  xbuffer <- sapply(cl, function(x) x@bbbox[3] - x@bbox[3])
  ybuffer <- sapply(cl, function(x) x@bbbox[4] - x@bbox[4])
  nfiles  <- sapply(cl, function(x) length(x@files))

  expect_equal(length(cl), nrow(ctg@data))
  expect_equal(width, ctg@data$Max.X - ctg@data$Min.X)
  expect_equal(xwidth, ctg@data$Max.X - ctg@data$Min.X)
  expect_equal(ywidth, ctg@data$Max.Y - ctg@data$Min.Y)
  expect_equal(xbwidth, ctg@data$Max.X - ctg@data$Min.X)
  expect_equal(ybwidth, ctg@data$Max.Y - ctg@data$Min.Y)
  expect_true(all(buffer == 0))
  expect_true(all(xbuffer == 0))
  expect_true(all(ybuffer == 0))
  expect_true(all(nfiles == 1))
})

test_that("catalog_makecluster makes correct clusters by file with buffer", {

  opt_chunk_size(ctg)   <- 0
  opt_chunk_buffer(ctg) <- 30

  cl  <- lidR:::catalog_makecluster(ctg)

  width   <- sapply(cl, function(x) x@width)
  buffer  <- sapply(cl, function(x) x@buffer)
  xwidth  <- sapply(cl, function(x) x@bbox[3] - x@bbox[1])
  ywidth  <- sapply(cl, function(x) x@bbox[4] - x@bbox[2])
  xbwidth <- sapply(cl, function(x) x@bbbox[3] - x@bbbox[1])
  ybwidth <- sapply(cl, function(x) x@bbbox[4] - x@bbbox[2])
  xbuffer <- sapply(cl, function(x) x@bbbox[3] - x@bbox[3])
  ybuffer <- sapply(cl, function(x) x@bbbox[4] - x@bbox[4])
  nfiles  <- sapply(cl, function(x) length(x@files))

  expect_equal(length(cl), nrow(ctg@data))
  expect_equivalent(width, (ctg@data$Max.X - ctg@data$Min.X) + 60)
  expect_equivalent(xwidth, ctg@data$Max.X - ctg@data$Min.X)
  expect_equivalent(ywidth, ctg@data$Max.Y - ctg@data$Min.Y)
  expect_equivalent(xbwidth, (ctg@data$Max.X - ctg@data$Min.X) + 60)
  expect_equivalent(ybwidth, (ctg@data$Max.Y - ctg@data$Min.Y) + 60)
  expect_true(all(buffer == 30))
  expect_true(all(xbuffer == 30))
  expect_true(all(ybuffer == 30))
  expect_true(all(nfiles > 1))
})

test_that("catalog_makecluster makes correct clusters by file with negative buffer", {

  opt_chunk_size(ctg)   <- 0
  opt_chunk_buffer(ctg) <- -30

  cl <- lidR:::catalog_makecluster(ctg)

  width   <- sapply(cl, function(x) x@width)
  buffer  <- sapply(cl, function(x) x@buffer)
  xwidth  <- sapply(cl, function(x) x@bbox[3] - x@bbox[1])
  ywidth  <- sapply(cl, function(x) x@bbox[4] - x@bbox[2])
  xbwidth <- sapply(cl, function(x) x@bbbox[3] - x@bbbox[1])
  ybwidth <- sapply(cl, function(x) x@bbbox[4] - x@bbbox[2])
  xbuffer <- sapply(cl, function(x) x@bbbox[3] - x@bbox[3])
  ybuffer <- sapply(cl, function(x) x@bbbox[4] - x@bbox[4])
  nfiles  <- sapply(cl, function(x) length(x@files))

  expect_equal(length(cl), nrow(ctg@data))
  expect_equivalent(width, (ctg@data$Max.X - ctg@data$Min.X) - 60)
  expect_equivalent(xwidth, ctg@data$Max.X - ctg@data$Min.X)
  expect_equivalent(ywidth, ctg@data$Max.Y - ctg@data$Min.Y)
  expect_equivalent(xbwidth, (ctg@data$Max.X - ctg@data$Min.X) - 60)
  expect_equivalent(ybwidth, (ctg@data$Max.Y - ctg@data$Min.Y) - 60)
  expect_true(all(buffer == -30))
  expect_true(all(xbuffer == -30))
  expect_true(all(ybuffer == -30))
  expect_true(all(nfiles == 1))
})


test_that("catalog_makecluster makes not cluster that belong on a tile only with buffer", {

  opt_chunk_size(ctg)   <- 800
  opt_chunk_buffer(ctg) <- 150
  opt_chunk_alignment(ctg) <- c(-100, -150)

  cl <- lidR:::catalog_makecluster(ctg)

  expect_equal(length(cl), 100L)
})

LASfile <- system.file("extdata", "Megaplot.laz", package = "lidR")
project <- catalog(LASfile)

test_that("catalog_makecluster makes correct clusters that do not overlap", {

  opt_chunk_buffer(project) <- 15
  opt_chunk_size(project)   <- 120

  cluster <- lidR:::catalog_makecluster(project)

  x <- unlist(lapply(cluster, function(cl) {c(cl@bbox[1], cl@bbox[3])}))
  y <- unlist(lapply(cluster, function(cl) {c(cl@bbox[2], cl@bbox[4])}))

  expect_equal(length(unique(x)), 4L)
  expect_equal(length(unique(y)), 4L)
})

