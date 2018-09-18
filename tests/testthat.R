Sys.setenv("R_TESTS" = "")

library(testthat)
library(lidR)

options(lidR.progress = FALSE)

test_check("lidR")
