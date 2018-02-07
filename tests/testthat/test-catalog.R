context("test-catalog")

sink(tempfile())

folder <- system.file("extdata", "", package="lidR")

catalog_options(multicore = 1, progress = FALSE)

test_that("build catalog works", {
  ctg = catalog(folder)
  expect_equal(dim(ctg@data)[1], 3)
  expect_equal(dim(ctg@data)[2], 34)
})

test_that("catalog queries works", {
  x = c(684850, 684880)
  y = c(5017850, 5017880)
  r = 20
  n = c("plot1", "pouik2")
  ctg = catalog(folder)
  req = catalog_queries(ctg, x, y, r, roinames = n)

  expect_equal(length(req), 2)
  expect_true(is(req$plot1, "LAS"))
  expect_true(is(req$pouik2, "LAS"))
  expect_equal(diff(range(req$plot1@data$X)), 2*r, tolerance = .5)
  expect_equal(diff(range(req$plot1@data$Y)), 2*r, tolerance = .5)
})

test_that("catalog queries works with buffer", {
  x = c(684850, 684880)
  y = c(5017850, 5017880)
  r = 20
  buffer = 5
  n = c("plot1", "pouik2")
  ctg = catalog(folder)
  req = catalog_queries(ctg, x, y, r, roinames = n, buffer = buffer)

  expect_equal(diff(range(req$plot1@data$X)), 2*r+2*buffer, tolerance = .5)
  expect_equal(diff(range(req$plot1@data$Y)), 2*r+2*buffer, tolerance = .5)
  expect_true("buffer" %in% names(req$plot1@data))
  expect_equal(sort(unique(req$plot1@data$buffer)), c(0,1))
})

test_that("catalog queries works when no data", {
  x = c(6848, 684880)
  y = c(5017850, 5017880)
  r = 20
  buffer = 5
  n = c("plot1", "pouik2")
  ctg = catalog(folder)

  req = suppressWarnings(catalog_queries(ctg, x, y, r, roinames = n, buffer = buffer))

  expect_equal(length(req), 1)

  expect_warning(catalog_queries(ctg, x, y, r, roinames = n, buffer = buffer),
                "plot1 is outside the catalog.")
})

test_that("catalog queries works with the two shapes", {
  x = c(684850)
  y = c(5017850)
  r = 20
  n = c("plot1")

  ctg = catalog(folder)
  req = catalog_queries(ctg, x, y, r, r, roinames = n)

  a = area(req$plot1)

  expect_equal(a, 4*r**2, tolerance = 0.01)

  req = catalog_queries(ctg, x, y, r, roinames = n)

  a = area(req$plot1)

  expect_equal(a, pi*r**2, tolerance = 0.02)
})

test_that("catalog queries support readLAS options", {
  x = c(684850)
  y = c(5017850)
  r = 20
  n = c("plot1")

  ctg = catalog(folder)
  req = catalog_queries(ctg, x, y, r, r, roinames = n, select = "xyz")

  cn = names(req$plot1@data)

  expect_true(!any(cn %in% c("Intensity", "ScanAngle", "ReturnNumber")))

  req = catalog_queries(ctg, x, y, r, r, roinames = n, select = "xyzia")

  cn = names(req$plot1@data)

  expect_true(any(cn %in% c("Intensity", "ScanAngle", "ReturnNumber")))
})

test_that("catalog reshape works", {
  catalog_options(multicore = 1)
  lidr_options(interactive = FALSE)

  ctg = catalog(folder)
  ctg@data = ctg@data[1]
  temp = tempfile()

  ctg2 = catalog_reshape(ctg, 80, temp, prefix = "test_")

  unlink(temp, recursive = T)

  expect_equal(sum(ctg@data$`Number of point records`), sum(ctg2@data$`Number of point records`))
  expect_equal(nrow(ctg2@data), 9)
})

test_that("catalog apply works", {
  catalog_options(multicore = 1, tiling_size = 100, buffer = 0)

  ctg = catalog(folder)
  ctg@data = ctg@data[1]

  test = function(las){ return(nrow(las@data)) }

  req = catalog_apply(ctg, test)

  s1 = do.call(sum, req)
  s2 = sum(ctg@data$`Number of point records`)

  expect_equal(s1,s2)

  test = function(las){ return(sum(las@data$ReturnNumber == 1)) }

  req = catalog_apply(ctg, test)

  s1 = do.call(sum, req)
  s2 = sum(ctg@data$`Number of 1st return`)

  expect_equal(s1,s2)

})

sink(NULL)
