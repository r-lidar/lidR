context("catalog_apply generic")

ctg <- lidR:::catalog_generator(500)
opt_chunk_size(ctg) <- 150

test_that("catalog_apply makes strict non-overlaping chunks", {

  # Count the points
  test <- function(cluster)
  {
    las <- readLAS(cluster)
    if (is.empty(las)) return(NULL)
    return(nrow(las@data))
  }

  req <- catalog_apply(ctg, test)
  s1  <- do.call(sum, req)
  s2  <- sum(ctg@data$Number.of.point.records)

  expect_equal(length(req), 4L)
  expect_equal(s1, s2)

  # Count the first return
  test <- function(cluster)
  {
    las <- readLAS(cluster)
    if (is.empty(las)) return(NULL)
    return(sum(las@data$ReturnNumber == 1))
  }

  req <- catalog_apply(ctg, test)
  s1  <- do.call(sum, req)
  s2  <- sum(ctg@data$Number.of.1st.return)

  expect_equal(length(req), 4L)
  expect_equal(s1, s2)
})

test_that("catalog_apply fixes chunk alignment", {

  test <- function(cluster, res)
  {
    las <- readLAS(cluster)
    if (is.empty(las)) return(NULL)
    r <- lidR:::rOverlay(las, res)
    return(r)
  }

  # Without option
  L      <- catalog_apply(ctg, test, res = 8)
  bboxes <- lapply(L, raster::extent)
  bbox   <- do.call(raster::merge, bboxes)
  area1  <- (bbox@xmax - bbox@xmin) * (bbox@ymax - bbox@ymin)
  areas1 <- sum(sapply(bboxes, function(x)  (x@xmax - x@xmin) * (x@ymax - x@ymin)))

  # With option
  option <- list(raster_alignment = 8)
  L      <- catalog_apply(ctg, test, res = 8, .options = option)
  bboxes <- lapply(L, raster::extent)
  bbox   <- do.call(raster::merge, bboxes)
  area2  <- (bbox@xmax - bbox@xmin) * (bbox@ymax - bbox@ymin)
  areas2 <- sum(sapply(bboxes, function(x)  (x@xmax - x@xmin) * (x@ymax - x@ymin)))

  expect_equal(area1, 40000)
  expect_equal(area1, area2)
  expect_gt(areas1, area1)
  expect_equal(area2, areas2)
  expect_message(catalog_apply(ctg, test, res = 8, .options = option), "Chunk size changed")
})

test_that("catalog_apply fixes chunk alignment even by file", {

  test <- function(cluster, res, align)
  {
    las <- readLAS(cluster)
    if (is.empty(las)) return(NULL)
    r <- lidR:::rOverlay(las, res, align)
    return(r)
  }

  opt_chunk_size(ctg) <- 0

  # Without option
  L      <- catalog_apply(ctg, test, res = 8, align = c(12, 12))
  bboxes <- lapply(L, raster::extent)
  bbox   <- do.call(raster::merge, bboxes)
  area1  <- (bbox@xmax - bbox@xmin) * (bbox@ymax - bbox@ymin)
  areas1 <- sum(sapply(bboxes, function(x)  (x@xmax - x@xmin) * (x@ymax - x@ymin)))

  # With option
  option <- list(raster_alignment = list(res = 8, start = c(12, 12)))
  L      <- catalog_apply(ctg, test, res = 8, align = c(12, 12), .options = option)
  bboxes <- lapply(L, raster::extent)
  bbox   <- do.call(raster::merge, bboxes)
  area2  <- (bbox@xmax - bbox@xmin) * (bbox@ymax - bbox@ymin)
  areas2 <- sum(sapply(bboxes, function(x)  (x@xmax - x@xmin) * (x@ymax - x@ymin)))

  expect_equal(area1, 43264)
  expect_equal(area1, area2)
  expect_equal(areas1, area1)
  expect_lt(area2, areas2)
  #expect_message(catalog_apply(ctg, test, res = 8, .options = option), "Chunk size changed")
})

test_that("catalog_apply generates errors if function does not return NULL for empty chunks", {

  test <- function(cluster)
  {
    las <- readLAS(cluster)
    return(0)
  }

  expect_error(catalog_apply(ctg, test), "not return NULL for empty chunks")
})

test_that("catalog_apply use alternative directories", {

  test <- function(cluster)
  {
    las <- readLAS(cluster)
    if (is.empty(las)) return(NULL)
    return(0)
  }

  realdir <- paste0(dirname(ctg$filename[1]), "/")
  falsedir <- "/home/user/data/"

  ctg@data$filename <- paste0(falsedir,basename(ctg$filename))

  expect_error(catalog_apply(ctg, test))

  ctg@input_options$alt_dir = c(falsedir,realdir)

  expect_error(catalog_apply(ctg, test), NA)
})

test_that("catalog_apply return a partial ouptut and generates logs", {

  test <- function(cluster) {
    if (raster::extent(cluster)@ymin > 80) stop("Test error")
    return(data.frame(X = 1:3))
  }

  opt_chunk_size(ctg) <- 0
  opt_wall_to_wall(ctg) <- FALSE

  option <- list(automerge = TRUE)
  req1 <- suppressMessages(catalog_apply(ctg, test))
  req2 <- suppressMessages(catalog_apply(ctg, test, .options = option))

  expect_is(req1, "list")
  expect_equal(length(req1), 2L)
  expect_message(catalog_apply(ctg, test), "chunk3.rds")
  expect_message(catalog_apply(ctg, test), "Test error")

  expect_is(req2, "data.frame")
  expect_equal(nrow(req2), 6L)
})

test_that("catalog_apply can bypass errors", {

  test <- function(cluster) {
    if (raster::extent(cluster)@ymin > 80) stop("Test error")
    return(data.frame(X = 1:3))
  }

  opt_chunk_size(ctg) <- 0
  opt_wall_to_wall(ctg) <- FALSE
  opt_stop_early(ctg) <- FALSE

  expect_message(catalog_apply(ctg, test), NA)
})

test_that("User get a warning/error when using ORIGINALFILENAME", {

  expect_message({opt_output_files(ctg) <- "{*}"}, "makes sense only when processing by file")
  expect_message({opt_output_files(ctg) <- "{ID}"}, NA)
})

