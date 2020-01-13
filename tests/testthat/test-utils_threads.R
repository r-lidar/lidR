test_that("set_lidr_threads set max cores if set to 0", {
  set_lidr_threads(0L)
  expect_equivalent(get_lidr_threads(), future::availableCores())
  set_lidr_threads(1L)
})
