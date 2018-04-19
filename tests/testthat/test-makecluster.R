context("makecluster")

LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
project = catalog(LASfile)


test_that("Cluster do not overlap", {
  catalog_options(buffer = 15, multicore = 1, tiling_size = 120)

  cluster = lidR:::catalog_makecluster(project, 1, 15, FALSE,size = 120)

  x = unlist(lapply(cluster, function(cl) {c(cl@bbox$xmin, cl@bbox$xmax)}))
  y = unlist(lapply(cluster, function(cl) {c(cl@bbox$ymin, cl@bbox$ymax)}))

  expect_equal(length(unique(x)), 3)
  expect_equal(length(unique(y)), 3)
})

test_that("No extra cluster are generated", {

  catalog_options(multicore = 1, tiling_size = 160, buffer = 0)

  cluster = lidR:::catalog_makecluster(project, 20, 0, FALSE, size = 160)

  x = unlist(lapply(cluster, function(cl) {c(cl@bbox$xmin, cl@bbox$xmax)}))
  y = unlist(lapply(cluster, function(cl) {c(cl@bbox$ymin, cl@bbox$ymax)}))

  expect_equal(length(unique(x)), 3)
  expect_equal(length(unique(y)), 3)
})
