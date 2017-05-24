context("grid_hexametrics")

las = lidR:::dummy_las(4000)

test_that("grid_hexametrics space hexa properly", {
  x = grid_hexametrics(las, mean(Z), 20)
  y = x$X %>% unique %>%  sort %>%  diff %>% round(2) %>% unique
  r = sqrt((2*400)/(3*sqrt(3))) %>% round(2)
  expect_equal(length(y), 1)
  expect_equal(y, r)
  expect_true(0 %in% x$X)
})

test_that("grid_hexametrics return an error if splitline and no flightlineID", {
  expect_error(grid_hexametrics(las, mean(Z), splitlines = T))
})


