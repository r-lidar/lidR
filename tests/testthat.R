Sys.setenv("R_TESTS" = "")

library(testthat)
library(lidR)

test_check("lidR")
