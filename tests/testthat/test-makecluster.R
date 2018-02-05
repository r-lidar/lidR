context("makecluster")

LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
project = catalog(LASfile)


test_that("Cluster do not overlap", {
  buffer(project) <- 15
  tiling_size(project) <- 120

  cluster = lidR:::catalog_makecluster(project, 1)

  x = unlist(lapply(cluster, function(cl) {c(cl@bbox$xmin, cl@bbox$xmax)}))
  y = unlist(lapply(cluster, function(cl) {c(cl@bbox$ymin, cl@bbox$ymax)}))

  expect_equal(length(unique(x)), 3)
  expect_equal(length(unique(y)), 3)
})

test_that("No extra cluster are generated", {
  buffer(project) <- 0
  tiling_size(project) <- 160

  cluster = lidR:::catalog_makecluster(project, 20)

  x = unlist(lapply(cluster, function(cl) {c(cl@bbox$xmin, cl@bbox$xmax)}))
  y = unlist(lapply(cluster, function(cl) {c(cl@bbox$ymin, cl@bbox$ymax)}))

  expect_equal(length(unique(x)), 3)
  expect_equal(length(unique(y)), 3)
})
