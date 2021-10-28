context("segment_snags")

las = clip_rectangle(mixedconifer, 481260, 3812921, 481300, 3812961)
las = filter_poi(las, Z >= 0)
ctg <- megaplot_ctg
opt_progress(ctg) <- FALSE
opt_chunk_size(ctg) <- 250
opt_chunk_alignment(ctg) <- c(10, 130)
opt_chunk_buffer(ctg) <- 10
opt_output_files(ctg) <- "{tempfile()}/{ID}"
opt_select(ctg) <- "2"

BBPRthrsh_mat <- matrix(c(0.80, 0.80, 0.70,
                         0.85, 0.85, 0.60,
                         0.80, 0.80, 0.60,
                         0.90, 0.90, 0.55),
                         nrow =3, ncol = 4)

test_that("Wing's method works", {
  las <- segment_snags(las, wing2015(neigh_radii = c(1.5, 1, 2), BBPRthrsh_mat = BBPRthrsh_mat))
  expect_true("snagCls" %in% names(las))
  expect_equal(as.numeric(table(las@data$snagCls)), c(7223, 93, 55, 15))
})

test_that("segment_snags works with a LAScatalog", {

 skip_on_cran()

 ctg2 <- segment_snags(ctg, wing2015(neigh_radii = c(1.5, 1, 2), BBPRthrsh_mat = BBPRthrsh_mat))
 las2 <- readLAS(ctg2)

 expect_is(ctg, "LAScatalog")
 expect_equal(sum(ctg2$Number.of.point.records), ctg$Number.of.point.records)
 expect_true("snagCls" %in% names(las2@data))
 expect_true(is.integer(las2$snagCls))
})


