context("LAScatalog")

ctg <- megaplot_ctg

opt_chunk_size(ctg)      <- 140
opt_chunk_alignment(ctg) <- c(684760, 5017760)
opt_chunk_buffer(ctg)    <- 3

test_that("LAScatalog redefined behavior of $, [, and [[", {

  expect_true(is.character(ctg$File.Signature))
  expect_equal(length(ctg$File.Signature), 1)

  expect_true(is.null(ctg$U))

  expect_error(ctg$File.Signature <- 1, "cannot be modified")

  expect_true(is.character(ctg[["File.Signature"]]))
  expect_equal(length(ctg[["File.Signature"]]), 1)

  expect_true(is.null(ctg[["U"]]))

  expect_error(ctg[1:2,1:2], "not allowed")
})

test_that("LAScatalog conversion to SpatialPolygonsDataFrame works", {

  spctg <- as.spatial(ctg)

  expect_true(is(spctg, "SpatialPolygonsDataFrame"))
  expect_reference(ctg@data$UserData, spctg@data$UserData)
})


test_that("LAScatalog subset works", {

  ctg <- random_2files_250points

  sf1 <- ctg[2]
  expect_is(sf1, "sf")
  expect_equal(dim(sf1), c(2,2))
  expect_equal(names(sf1)[1], "File.Source.ID")

  ctg1 <- ctg[2,]
  expect_is(ctg1, "LAScatalog")
  expect_equal(dim(ctg1), c(1,35))

  ctg1 <- ctg[c(FALSE,TRUE)]
  expect_is(ctg1, "LAScatalog")
  expect_equal(dim(ctg1), c(1,35))

  ctg1 <- ctg[c(FALSE,FALSE)]
  expect_is(ctg1, "LAScatalog")
  expect_equal(dim(ctg1), c(0,35))

  p <- sf::st_point(c(50,50))
  p <- sf::st_sfc(p)
  p <- sf::st_set_crs(p, st_crs(ctg))
  p <- sf::st_as_sf(p)
  ctg1 <- ctg[p]
  expect_is(ctg1, "LAScatalog")
  expect_equal(dim(ctg1), c(1,35))
})


