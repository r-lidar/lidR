context("assertions")

test_that("assertions work", {
  x = c(-1, 1)
  expect_error(lidR:::assert_all_are_non_negative(x))
  expect_error(lidR:::assert_all_are_positive(x))
  expect_error(lidR:::assert_all_are_in_closed_range(x, 0, 1))
  expect_error(lidR:::assert_all_are_in_open_range(x, -1, 1))
  expect_error(lidR:::assert_is_a_number(x))
  expect_error(lidR:::assert_is_function(x))

  y = c(T, F, T)
  expect_error(lidR:::assert_all_are_true(y))
  expect_error(lidR:::assert_is_a_bool(y))
  expect_error(lidR:::assert_is_numeric(y))

  expect_error(lidR:::assert_are_same_length(x, y))

  f = tempfile()
  expect_error(lidR:::assert_all_are_existing_files(f))

  g = function(x) {x}
  expect_error(lidR:::assert_is_vector(g))
  expect_error(lidR:::assert_is_list(g))
  expect_error(lidR:::assert_is_character(g))
  expect_error(lidR:::assert_is_all_of(g, "numeric"))


  expect_error(lidR:::assert_is_algorithm(g))
  expect_error(lidR:::assert_is_algorithm_dsm(g))
  expect_error(lidR:::assert_is_algorithm_itd(g))
  expect_error(lidR:::assert_is_algorithm_its(g))
  expect_error(lidR:::assert_is_algorithm_spi(g))
  expect_error(lidR:::assert_is_algorithm_dec(g))
  expect_error(lidR:::assert_is_algorithm_gnd(g))
  expect_error(lidR:::assert_is_algorithm_sng(g))

  expect_error(lidR:::assert_is_valid_context(tin(), "test"))

  lazfile <- system.file("extdata", "example.laz", package="rlas")
  las <- readLAS(lazfile)
  las <- lasfilter(las, Z > 1000)
  expect_error(lidR:::stopifnotlas(g))
  expect_error(lidR:::assert_las_is_not_empty(las))
  expect_error(lidR:::stopif_forbidden_name("X"))
})
