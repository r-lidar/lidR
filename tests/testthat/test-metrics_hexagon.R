las <- lidR:::generate_las(2000)
f1  <- ~list(Zmean = mean(Z))
f2  <- ~list(meanZ = mean(Z), maxZ = max(Z))

test_that("hexagon_metrics returns a named sf", {

  x <- hexagon_metrics(las, f1, area = 800)

  expect_true(is(x, "sf"))
  expect_equivalent(nrow(x), 22)
  expect_equivalent(names(x)[1], "Zmean")

  x <- hexagon_metrics(las, f2, area = 800)

  expect_true(is(x, "sf"))
  expect_equivalent(nrow(x), 22)
  expect_equivalent(names(x)[1:2], c("meanZ", "maxZ"))
})


test_that("hexagon_metrics work with by_echo and filter", {

  x <- hexagon_metrics(las, f2, area = 800, filter = ~Z > 2, by_echo = c("all", "lastofmany", "single"))

  expect_true(is(x, "sf"))
  expect_equivalent(dim(x), c(22, 7))
})
