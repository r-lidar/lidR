LASfile <- system.file("extdata", "example.las", package="rlas")
las = readLAS(LASfile)
compression_enabled <- utils::packageVersion("rlas") >= "1.6.0"

test_that("readLAS reads compressed data", {
  skip_if_not(compression_enabled, "Compression not supported")
  expect_gte(sum(las_is_compressed(las)), 5)
})

test_that("las_check does not materialize", {
  skip_if_not(compression_enabled, "Compression not supported")
  las_check(las, print = FALSE)
  expect_gte(sum(las_is_compressed(las)), 5)
})

test_that("filer does not materialize", {
  skip_if_not(compression_enabled, "Compression not supported")
  filter_ground(las)
  expect_gte(sum(las_is_compressed(las)), 5)
})

test_that("serialization does not materialize", {
  skip_if_not(compression_enabled, "Compression not supported")
  x <- serialize(las, NULL)
  y <- unserialize(x)
  expect_gte(sum(las_is_compressed(y)), 5)
})

test_that("locate_tree does not materialize", {
  skip_if_not(compression_enabled, "Compression not supported")
  m <- locate_trees(las, lmf(3))
  expect_gte(sum(las_is_compressed(las)), 5)
})

test_that("writeLAS does not materialize", {
  skip_if_not(compression_enabled, "Compression not supported")
  f <- tempfile(fileext = ".las")
  writeLAS(las, f)
  expect_gte(sum(las_is_compressed(las)), 5)
})


test_that("aggregation does not materialize", {
  skip_if_not(compression_enabled, "Compression not supported")
  m <- pixel_metrics(las, ~max(Z), 5)
  expect_gte(sum(las_is_compressed(las)), 5)
})

test_that("aggregation does materializes only requiered columns", {
  skip_if_not(compression_enabled, "Compression not supported")
  m <- pixel_metrics(las, ~sum(UserData), 5)
  expect_gte(sum(las_is_compressed(las)), 4)
})
