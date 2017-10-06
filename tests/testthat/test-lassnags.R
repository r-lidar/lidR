context("lassnags")

# LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
# las = readLAS(LASfile, select = "xyzinr", filter="-drop_z_below 0 -keep_first")
#
# BBPRthrsh_mat <- matrix(c(0.80, 0.80, 0.70,
#                          0.85, 0.85, 0.60,
#                          0.80, 0.80, 0.60,
#                          0.90, 0.90, 0.55),
#                          nrow =3, ncol = 4)
#
# test_that("Wing's method works", {
#   lassnags(las, "wing2015")
#   expect_true("SnagCls" %in% names(las@data))
#   expect_true(all(!is.na(las@data$SnagCls)))
# })