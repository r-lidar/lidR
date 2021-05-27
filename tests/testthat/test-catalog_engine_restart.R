context("catalog_apply restart")

ctg <- lidR:::catalog_generator(2, 250)

ctg@chunk_options$size = 50
ctg@chunk_options$buffer = 0
ctg@processing_options$progress <- FALSE

test_that("catalog drops some chunk", {
  ctg@chunk_options$drop = 1:3
  cls = catalog_makechunks(ctg)
  expect_equal(length(cls), 5)

  ctg@chunk_options$drop = c(1:3, 8)
  cls = catalog_makechunks(ctg)
  expect_equal(length(cls), 4)
})

test_that("catalog engine returns a valid output", {
  ctg@chunk_options$drop = 1:3
  m = grid_metrics(ctg, ~mean(Z), 20)

  expect_equal(extent(m), extent(0,100,60,200))
  expect_equal(sum(is.na(m[])), 9)
})
