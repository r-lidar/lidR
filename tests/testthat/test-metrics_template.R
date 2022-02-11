las = random_10_points

VCI = function() { 0 }
colorize_points = function() { 0 }
f = function()
{
  entropy <- function(x) x
  cloud_metrics(las, ~entropy(Z))
}


test_that("Ambiguous definitions are handled", {

  expect_error(cloud_metrics(las, ~VCI()), "exists in the package lidR but is also defined in")
  expect_error(cloud_metrics(las, ~colorize_points()), "exists in the package lidR but is also defined in")
  expect_error(f(), "exists in the package lidR but is also defined in")
  #expect_error(cloud_metrics(las, ~lidR::VCI(Z, 10)))
})
