ctg <- random_2files_250points
opt_chunk_size(ctg) <- 151
opt_progress(ctg) <- FALSE

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

  expect_equal(length(req), 2L)
  expect_equal(s1, s2)

  skip_on_cran()

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

  expect_equal(length(req), 2L)
  expect_equal(s1, s2)
})

test_that("catalog_apply fixes chunk alignment", {

  test <- function(cluster, res)
  {
    las <- readLAS(cluster)
    if (is.empty(las)) return(NULL)
    return(npoints(las))
  }

  res = 8

  # Expected
  R0 <- list(381, 119)

  # Without option
  R1 <- catalog_apply(ctg, test, res = res)

  # With option
  option <- list(raster_alignment = res)
  expect_message(R2 <- catalog_apply(ctg, test, res = res, .options = option), "Chunk size changed to 152")

  expect_equal(R0, R2)
  expect_true(!identical(R0, R1))
})

test_that("catalog_apply fixes chunk alignment even by file", {

  test <- function(cluster, res, align)
  {
    las <- readLAS(cluster)
    if (is.empty(las)) return(NULL)
    r = grid_metrics(las, ~max(Z), res, align)
    return(r)
  }

  res = 8
  sta = c(8,8)

  opt_chunk_size(ctg) <- 0
  las = readLAS(ctg)

  # Reference
  R0 = grid_metrics(las, ~max(Z), res = res, start = sta)

  # Without option
  R1 <- catalog_sapply(ctg, test, res = res, align = sta)

  # With option
  option <- list(raster_alignment = list(res = res, start = sta))
  R2 <- catalog_sapply(ctg, test, res = res, align = sta, .options = option)

  expect_equal(R0, R2)
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

  skip_on_cran()

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
  expect_message(req1 <- catalog_apply(ctg, test), "chunk2.rds")
  expect_message(req2 <- catalog_apply(ctg, test, .options = option), "Test error")

  expect_is(req1, "list")
  expect_equal(length(req1), 1L)
  expect_is(req2, "data.frame")
  expect_equal(nrow(req2), 3L)
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

test_that("catalog_apply works with no future option", {

  options(lidR.no.future = TRUE)

  opt_chunk_size(ctg) <- 0
  opt_wall_to_wall(ctg) <- FALSE
  opt_stop_early(ctg) <- TRUE

  test <- function(cluster) {
    return(0)
  }

  expect_error(catalog_apply(ctg, test), NA)

  options(lidR.no.future = FALSE)
})

test_that("User get a warning/error when using ORIGINALFILENAME", {

  expect_message({opt_output_files(ctg) <- "{*}"}, "makes sense only when processing by file")
  expect_message({opt_output_files(ctg) <- "{ID}"}, NA)
})

test_that("User get throw error if function does not return NULL for empty chun", {

  test <- function(cluster) {
    las <- readLAS(cluster)
    return(nrow(las@data))
  }

  test2 <- function(cluster) {
    stop("dummy error", call. = FALSE)
  }

  expect_error(catalog_apply(ctg, test), "User's function does not return NULL")
  expect_error(catalog_apply(ctg, test2), "dummy error")
})

