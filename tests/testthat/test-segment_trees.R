context("segment_trees")

las = clip_rectangle(mixedconifer, 481250, 3812980, 481300, 3813030)
ctg = mixedconifer_ctg
opt_progress(ctg) = FALSE
opt_chunk_size(ctg) = 100
opt_chunk_buffer(ctg) = 20
opt_chunk_alignment(ctg) <- c(0, 20)

chm = grid_canopy(las, 0.5, pitfree())
kernel = matrix(1,3,3)
chm = raster::focal(chm, w = kernel, fun = mean)

test_that("Dalponte's methods works", {

  ttops = suppressWarnings(find_trees(chm, lmf(3, 2)))
  las <- segment_trees(las, dalponte2016(chm, ttops))

  expect_true("treeID" %in% names(las@data))
  expect_equal(sort(unique(las@data$treeID)), 1:40L)
  expect_true(is.integer(las@data$treeID))

  # Test if a seed is not in the chm
  old = ttops@coords
  ttops@coords[1,1] <- 0

  # Test if seed IDs are propagated
  ttops@coords <- old
  ttops@data$treeID = 1:40*2L

  las <- segment_trees(las, dalponte2016(chm, ttops))
  expect_equal(sort(unique(las@data$treeID)), 1:40L*2L)

  ttops$treeID[2:3] <- 1L

  expect_error(segment_trees(las, dalponte2016(chm, ttops), "Duplicated tree IDs found"))
})

test_that("Dalponte's return NAs if no seed", {

  ttops <- suppressWarnings(find_trees(chm, lmf(3, 2)))
  ttops@coords <- ttops@coords + 500

  expect_warning(segment_trees(las, dalponte2016(chm, ttops)), "No tree can be used as seed")

  suppressWarnings(las <- segment_trees(las, dalponte2016(chm, ttops)))

  expect_true("treeID" %in% names(las@data))
  expect_true(all(is.na(las$treeID)))
  expect_true(is.integer(las@data$treeID))

  # test is 0 seed
  ttops <- ttops[0,]
  expect_warning(segment_trees(las, dalponte2016(chm, ttops)), "No tree found")
})

test_that("Dalponte's works standalone", {

  ttops <- suppressWarnings(find_trees(chm, lmf(3, 2)))
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
  las <- segment_trees(las, li2012(speed_up = 5), attribute = "TID")

  expect_true("TID" %in% names(las@data))
  expect_equal(sort(unique(las@data$TID)), 1:48L)
  expect_true(is.integer(las@data$TID))

  expect_warning(segment_trees(las, li2012(hmin =  100)), "No tree segmented")
})

test_that("Silvas's methods works", {
  ttops = suppressWarnings(find_trees(chm, lmf(3, 2)))
  las <- segment_trees(las, silva2016(chm, ttops))

  expect_true("treeID" %in% names(las@data))
  expect_true(is.integer(las@data$treeID))
})

test_that("Silva's return NAs if no seed", {

  ttops <- suppressWarnings(find_trees(chm, lmf(3, 2)))
  ttops@coords <- ttops@coords + 500

  expect_warning(segment_trees(las, silva2016(chm, ttops)), "No tree can be used as seed")

  suppressWarnings(las <- segment_trees(las, silva2016(chm, ttops)))

  expect_true("treeID" %in% names(las@data))
  expect_true(all(is.na(las$treeID)))
  expect_true(is.integer(las@data$treeID))

  # test is 0 seed
  ttops <- ttops[0,]
  expect_warning(segment_trees(las, dalponte2016(chm, ttops)), "No tree found")
})

test_that("Silva's works standalone", {
  ttops <- suppressWarnings(find_trees(chm, lmf(3, 2)))
  trees <- silva2016(chm, ttops)()

  expect_true(is(trees, "RasterLayer"))
  expect_equal(sort(unique(trees[])), 1:40L)

  ttops@coords <- ttops@coords + 500

  expect_warning(silva2016(chm, ttops)(), "No tree can be used as seed")
})

test_that("Watershed's methods works", {

  skip_if_not_installed("EBImage")

  las <- segment_trees(las, lidR::watershed(chm))

  expect_true("treeID" %in% names(las@data))
  expect_true(is.integer(las@data$treeID))
})

# test_that("MC watershed methods works", {
#   ttops = suppressWarnings(find_trees(chm, lmf(3, 2)))
#   las <- segment_trees(las, mcwatershed(chm, ttops))
#
#   expect_true("treeID" %in% names(las@data))
#   expect_true(is.integer(las@data$treeID))
# })

test_that("segment_trees can store in a user defined column", {

  las <- segment_trees(las, li2012(speed_up = 5), attribute = "plop")
  expect_true("plop" %in% names(las@data))
})

test_that("segment_trees supports different unicity strategies", {

  las <- segment_trees(las, li2012(speed_up = 5), uniqueness = "incremental", attribute = "ID1")
  las <- segment_trees(las, li2012(speed_up = 5), uniqueness = "gpstime", attribute = "ID2")
  las <- segment_trees(las, li2012(speed_up = 5), uniqueness = "bitmerge", attribute = "ID3")

  expect_equal(length(na.omit(unique(las@data$ID1))), 48L)
  expect_equal(length(na.omit(unique(las@data$ID2))), 48L)
  expect_equal(length(na.omit(unique(las@data$ID3))), 48L)

  u = las@data[, if (!anyNA(.BY)) .I[which(Z == max(Z))], by = "ID1"]
  s = las@data[u$V1]

  expectedID2 = s$gpstime
  expectedID3 = lidR:::bitmerge(s$X*100, s$Y*100)

  id42 = s$ID1 == 42

  expect_equal(s$ID2[!id42], expectedID2[!id42])
  expect_equal(s$ID3[!id42], expectedID3[!id42])

  expect_equal(s$ID2[id42][1], min(s$gpstime[id42]))
  expect_equal(s$ID3[id42][1], expectedID3[id42][which.min(s$X[id42])])
})

test_that("segment_trees supports a LAScatalog", {

  # Skipping this test temporarily to be able to have a build on MacOS. Then I will be
  # able to test that on somebody' computer.
  skip_on_cran()

  opt_output_files(ctg) <- "{tempdir()}/{ID}"
  new_ctg <- segment_trees(ctg, li2012(speed_up = 5), uniqueness = 'gpstime')

  las = readLAS(new_ctg)
  las = segment_trees(las, li2012(speed_up = 5), uniqueness = 'gpstime', attribute = "treeID2")

  expect_equal(length(na.omit(unique(las@data$treeID))), 274) # 272 on Fedora and MacOS. Impossible to reproduce.
  expect_equal(length(na.omit(unique(las@data$treeID2))), 274) # 272 on Fedora and MacOS. Impossible to reproduce.

  skip("Need investigation")
  expect_equal(las$treeID, las$treeID2) # added this test but cannot test on mac os.
})

test_that("tree_metrics works", {

  las@data$plop = las$treeID
  las@data$treeID = NULL

  expect_error(tree_metrics(las, ~max(Z)), "not segmented")
  expect_error(tree_metrics(las, max(Z), attribute = "plop"), NA)
})

# Commented because CRAN doesn't like to call Bioconductor package
# test_that("Watershed's methods works", {
#   las@data[, treeID := NULL]
#
#   segment_trees_watershed(las, chm)
#   expect_true("treeID" %in% names(las@data))
# })


