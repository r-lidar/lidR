context("catalog_apply discontinuous files")

ctg = lidR:::catalog_generator(1000)

set.seed(34)
x = runif(4, 20, 180)
set.seed(42)
y = runif(4, 20, 180)

opt_output_files(ctg) <- "{tempfile()}"
opt_progress(ctg) <- FALSE
rois = clip_circle(ctg, x, y, 20)

test_that("opt_independent_files set adequate option for plot inventories", {
  opt_independent_files(rois) <- TRUE

  expect_equal(opt_chunk_buffer(rois), 0)
  expect_equal(opt_chunk_size(rois), 0)
  expect_equal(opt_wall_to_wall(rois), FALSE)
  expect_equal(opt_independent_files(rois), TRUE)
})

test_that("opt_independent_files built several DTMs without error", {
  opt_independent_files(rois) <- TRUE
  opt_merge(rois) <- FALSE
  opt_output_files(rois) <- ""

  dtms = grid_terrain(rois, 1, tin())

  expect_true(is.list(dtms))
  expect_true(is(dtms[[1]], "RasterLayer"))
})
