ctg = random_2files_250points

test_that("opt_independent_files set adequate option for plot inventories", {

  opt_independent_files(ctg) <- TRUE

  expect_equal(opt_chunk_buffer(ctg), 0)
  expect_equal(opt_chunk_size(ctg), 0)
  expect_equal(opt_wall_to_wall(ctg), FALSE)
  expect_equal(opt_independent_files(ctg), TRUE)
})

test_that("opt_independent_files built several DTMs without error", {

  opt_independent_files(ctg) <- TRUE
  opt_merge(ctg) <- FALSE
  opt_output_files(ctg) <- ""

  dtms = grid_terrain(ctg, 1, tin())

  expect_true(is.list(dtms))
  expect_true(is(dtms[[1]], "RasterLayer"))
})
