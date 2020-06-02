Sys.setenv("R_TESTS" = "")

library(testthat)
library(lidR)

options(lidR.progress = FALSE)
rgdal::set_thin_PROJ6_warnings(TRUE)
test_check("lidR")
