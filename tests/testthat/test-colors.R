context("colors")

test_that("set.colors attribute the proper color", {
  x = 1:10
  c = lidR:::set.colors(x, height.colors(50))

  expect_equal(length(unique(c)), length(x))

  x = rep(1,10)
  c = lidR:::set.colors(x, height.colors(50))

  expect_equal(c, "#0000FF")
})

test_that("color palette work", {
  col = height.colors(5)
  ref = c("#0000FF", "#00EEEE", "#00CD00", "#FFFF00", "#FF0000")

  expect_equal(col, ref)

  col = forest.colors(5)
  ref = c("#006400", "#248624", "#48A948", "#6CCB6C", "#90EE90")

  expect_equal(col, ref)

  col = random.colors(5)

  expect_equal(length(col), 5)

  col = pastel.colors(5)

  expect_equal(length(col), 5)
})
