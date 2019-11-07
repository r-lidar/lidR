context("hexbin_metrics")

las <- lidR:::dummy_las(4000)

test_that("hexbin_metrics space cells properly", {

  x  <- hexbin_metrics(las, mean(Z), 20)
  xy <- hexbin::hcell2xy(x)
  y  <- xy$x %>% unique %>% sort %>%  diff %>% round(2) %>% unique
  r  <- sqrt((2*400)/(3*sqrt(3))) %>% round(2)

  expect_equal(length(y), 1)
  expect_equal(y, r)
  expect_true(0 %in% xy$x)
})

test_that("hexbin_metrics accepts both an expression and a formula", {

  expect_error(hexbin_metrics(las,  mean(Z), 20), NA)
  expect_error(hexbin_metrics(las, ~mean(Z), 20), NA)
})


