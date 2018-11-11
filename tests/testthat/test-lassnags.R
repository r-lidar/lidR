context("lassnags")

LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
las = readLAS(LASfile, select = "xyzinr", filter="-drop_z_below 0 -keep_first -inside 481260 3812921 481300 3812961")

BBPRthrsh_mat <- matrix(c(0.80, 0.80, 0.70,
                         0.85, 0.85, 0.60,
                         0.80, 0.80, 0.60,
                         0.90, 0.90, 0.55),
                         nrow =3, ncol = 4)

test_that("Wing's method works", {
  las <- lassnags(las, wing2015(neigh_radii = c(1.5, 1, 2), BBPRthrsh_mat = BBPRthrsh_mat))
  expect_true("snagCls" %in% names(las@data))
  expect_equal(as.numeric(table(las@data$snagCls)), c(7223, 93, 55, 15))
})
