context("terrain")

las = lidR:::dummy_las(5000)
las@data[, Z := Z + 0.1*X+0.1*Y+sin(0.01*X)-sin(0.1*Y)+sin(0.003*X*Y)]

truedtm = lidR:::make_grid(0.5,99.5,0.5,99.5,1)
truedtm[, Z := 0.1*X+0.1*Y+sin(0.01*X)-sin(0.1*Y)+sin(0.003*X*Y)]
as.lasmetrics(truedtm,1)
data.table::setkey(truedtm, X, Y)

test_that("terrain works with knnidw", {
  dtm = grid_terrain(las, 1, method = "knnidw", k = 10L)
  data.table::setkey(dtm, X, Y)
  diff = dtm - truedtm
  expect_equal(dim(dtm)[1], 10000)
  expect_lt(mean(abs(diff$Z)), 0.2)
})

test_that("terrain works with delaunay", {
  dtm = grid_terrain(las, 1, method = "delaunay")
  data.table::setkey(dtm, X, Y)
  diff = truedtm[dtm][!is.na(i.Z)][, Z := Z - i.Z]
  expect_lt(mean(abs(diff$Z)), 0.09)
})

test_that("terrain works with kriging", {
  dtm = grid_terrain(las, 1, method = "kriging", k = 10L)
  data.table::setkey(dtm, X, Y)
  diff = dtm - truedtm
  expect_equal(dim(dtm)[1], 10000)
  expect_lt(mean(abs(diff$Z)), 0.07)
})
