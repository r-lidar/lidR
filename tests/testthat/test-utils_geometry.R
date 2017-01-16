context("utils_geometry")

x = c(0,1,1,0,0.5,0.2,0.3,0.4,0.2,0.8,0.9)
y = c(0,0,1,1,0.1,0.5,0.8,0.3,0.1,0.4,0.7)

vertx = c(0,1,0)
verty = c(0,0,1)

test_that("convex hull works", {
  expect_equal(lidR:::convex_hull(x,y), data.frame(x = c(1,0,0,1,1), y = c(0,0,1,1,0)))
})

test_that("area works", {
  expect_equal(lidR:::area(x,y), 1)
  expect_equal(lidR:::area(vertx,verty), 0.5)
})

test_that("points in polygon works", {
  ch = lidR:::convex_hull(x,y)

  expect_equal(lidR:::points_in_polygon(vertx, verty, x, y), c(T, F, F, F, T, T, F, T, T, F, F))
  expect_true(lidR:::point_in_polygon(ch$x, ch$y, 0.5,0.5))
  expect_true(!lidR:::point_in_polygon(ch$x, ch$y, -0.5,0.5))
})


