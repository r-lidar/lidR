context("catalog_apply restart")

ctg <- random_2files_250points
ctg@chunk_options$size = 50
ctg@chunk_options$buffer = 0
ctg@processing_options$progress <- FALSE

test_that("catalog drops some chunk", {
  ctg@chunk_options$drop = 1:3
  cls = engine_chunks(ctg)
  expect_equal(length(cls), 5)

  ctg@chunk_options$drop = c(1:3, 8)
  cls = engine_chunks(ctg)
  expect_equal(length(cls), 4)
})

test_that("catalog engine returns a valid output", {

  ctg@chunk_options$drop = 1:3
  m = pixel_metrics(ctg, ~mean(Z), 20)

  expect_equivalent(st_bbox(m), st_bbox(c(xmin = 0, xmax = 100,ymin = 60,ymax = 200)))
  expect_equal(sum(is.na(m[])), 9)
})
