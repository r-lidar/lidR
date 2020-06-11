context("lastrees")

LASfile <- system.file("extdata", "MixedConifer.laz", package = "lidR")
las = readLAS(LASfile, select = "xyzrt", filter = "-drop_z_below 0 -keep_xy 481250 3812980 481300 3813030")
ctg = readLAScatalog(LASfile, progress = FALSE, chunk_size = 100, chunk_buffer = 20)
opt_chunk_alignment(ctg) <- c(0, 20)

chm = grid_canopy(las, 0.5, pitfree())
kernel = matrix(1,3,3)
chm = raster::focal(chm, w = kernel, fun = mean)


test_that("Dalponte's methods works", {

  ttops = suppressWarnings(tree_detection(chm, lmf(3, 2)))
  las <- lastrees(las, dalponte2016(chm, ttops))

  expect_true("treeID" %in% names(las@data))
  expect_equal(sort(unique(las@data$treeID)), 1:40L)
  expect_true(is.integer(las@data$treeID))

  # Test if a seed is not in the chm
  old = ttops@coords
  ttops@coords[1,1] <- 0

  # Test if seed IDs are propagated
  ttops@coords <- old
  ttops@data$treeID = 1:40*2L

  las <- lastrees(las, dalponte2016(chm, ttops))
  expect_equal(sort(unique(las@data$treeID)), 1:40L*2L)

  ttops$treeID[2:3] <- 1L

  expect_error(lastrees(las, dalponte2016(chm, ttops), "Duplicated tree IDs found"))
})

test_that("Dalponte's return NAs if no seed", {

  ttops <- suppressWarnings(tree_detection(chm, lmf(3, 2)))
  ttops@coords <- ttops@coords + 500

  expect_warning(lastrees(las, dalponte2016(chm, ttops)), "No tree can be used as seed")

  suppressWarnings(las <- lastrees(las, dalponte2016(chm, ttops)))

  expect_true("treeID" %in% names(las@data))
  expect_true(all(is.na(las$treeID)))
  expect_true(is.integer(las@data$treeID))

  # test is 0 seed
  ttops <- ttops[0,]
  expect_warning(lastrees(las, dalponte2016(chm, ttops)), "No tree found")
})

test_that("Dalponte's works standalone", {

  ttops <- suppressWarnings(tree_detection(chm, lmf(3, 2)))
  trees <- dalponte2016(chm, ttops)()

  expect_true(is(trees, "RasterLayer"))
  expect_equal(sort(unique(trees[])), 1:40L)


  ttops@coords <- ttops@coords + 500

  expect_warning(dalponte2016(chm, ttops)(), "No tree can be used as seed")

  suppressWarnings(trees <- dalponte2016(chm, ttops)())

  expect_true(is(trees, "RasterLayer"))
  expect_equal(sort(unique(trees[])), numeric(0))

  ttops@coords <- ttops@coords - 480

  expect_warning(dalponte2016(chm, ttops)(), "Some trees are outside the canopy height model")

  suppressWarnings(trees <- dalponte2016(chm, ttops)())

  expect_true(is(trees, "RasterLayer"))
  expect_equal(sort(unique(trees[])), 1:5L)
})

test_that("Li's method works", {
  las <- lastrees(las, li2012(speed_up = 5), attribute = "TID")

  expect_true("TID" %in% names(las@data))
  expect_equal(sort(unique(las@data$TID)), 1:48L)
  expect_true(is.integer(las@data$TID))

  expect_warning(lastrees(las, li2012(hmin =  100)), "No tree segmented")
})

test_that("Silvas's methods works", {
  ttops = suppressWarnings(tree_detection(chm, lmf(3, 2)))
  las <- lastrees(las, silva2016(chm, ttops))

  expect_true("treeID" %in% names(las@data))
  expect_true(is.integer(las@data$treeID))
})

test_that("Silva's return NAs if no seed", {

  ttops <- suppressWarnings(tree_detection(chm, lmf(3, 2)))
  ttops@coords <- ttops@coords + 500

  expect_warning(lastrees(las, silva2016(chm, ttops)), "No tree can be used as seed")

  suppressWarnings(las <- lastrees(las, silva2016(chm, ttops)))

  expect_true("treeID" %in% names(las@data))
  expect_true(all(is.na(las$treeID)))
  expect_true(is.integer(las@data$treeID))

  # test is 0 seed
  ttops <- ttops[0,]
  expect_warning(lastrees(las, dalponte2016(chm, ttops)), "No tree found")
})

test_that("Silva's works standalone", {
  ttops <- suppressWarnings(tree_detection(chm, lmf(3, 2)))
  trees <- silva2016(chm, ttops)()

  expect_true(is(trees, "RasterLayer"))
  expect_equal(sort(unique(trees[])), 1:40L)

  ttops@coords <- ttops@coords + 500

  expect_warning(silva2016(chm, ttops)(), "No tree can be used as seed")
})

test_that("Watershed's methods works", {

  if (require("EBImage", quietly = T, warn.conflicts = FALSE))
  {
    las <- lastrees(las, lidR::watershed(chm))

    expect_true("treeID" %in% names(las@data))
    expect_true(is.integer(las@data$treeID))
  }
})

# test_that("MC watershed methods works", {
#   ttops = suppressWarnings(tree_detection(chm, lmf(3, 2)))
#   las <- lastrees(las, mcwatershed(chm, ttops))
#
#   expect_true("treeID" %in% names(las@data))
#   expect_true(is.integer(las@data$treeID))
# })

test_that("lastrees can store in a user defined column", {

  las <- lastrees(las, li2012(speed_up = 5), attribute = "plop")
  expect_true("plop" %in% names(las@data))
})

test_that("lastrees supports different unicity srategies", {

  las <- lastrees(las, li2012(speed_up = 5), uniqueness = "gpstime")

  expect_equal(length(na.omit(unique(las@data$treeID))), 48L)

  las <- lastrees(las, li2012(speed_up = 5), uniqueness = "bitmerge")

  expect_equal(length(na.omit(unique(las@data$treeID))), 48L)
})


test_that("lastrees supports a LAScatalog", {

  opt_output_files(ctg) <- "{tempdir()}/{ID}"
  new_ctg <- lastrees(ctg, li2012(speed_up = 5), uniqueness = 'bitmerge')

  las = readLAS(new_ctg)
  las = lastrees(las, li2012(speed_up = 5), uniqueness = 'bitmerge', attribute = "treeID2")

  # Commenting this test temporarily to be able to have a build on MacOS. Then I will be
  # able to test that on somebody computer.
  #expect_equal(length(na.omit(unique(las@data$treeID))), 274) # 272 on Fedora and MacOS. Impossible to reproduce.
  #expect_equal(las$treeID, las$treeID2) # added this test but cannot test on mac os.
})

test_that("tree_metrics works", {

  las <- lastrees(las, li2012(speed_up = 5), attribute = "plop")

  expect_error(tree_metrics(las, max(Z)), "not segmented")

  Y <- tree_metrics(las, max(Z), attribute = "plop")

  expect_error(tree_metrics(las, max(Z), attribute = "abc"), "trees are not segmented")
})

# Commented because CRAN doesn't like to call Bioconductor package
# test_that("Watershed's methods works", {
#   las@data[, treeID := NULL]
#
#   lastrees_watershed(las, chm)
#   expect_true("treeID" %in% names(las@data))
# })


