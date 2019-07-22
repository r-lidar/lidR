context("catalog_apply")

LASfile  <- system.file("extdata", "Megaplot.laz", package="lidR")
ctg      <- catalog(LASfile)
ctg@data <- ctg@data[1,]
las      <- readLAS(ctg)

opt_chunk_buffer(ctg)    <- 0
opt_chunk_size(ctg)      <- 150
opt_chunk_alignment(ctg) <- c(0, 90)
opt_progress(ctg)        <- FALSE

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

test_that("catalog_apply options fix alignment", {

  test <- function(cluster, res)
  {
    las <- readLAS(cluster)
    if (is.empty(las)) return(NULL)
    r <- lidR:::rOverlay(las, res)
    return(r)
  }

  # Without option
  L      <- catalog_apply(ctg, test, res = 18)
  bboxes <- lapply(L, raster::extent)
  bbox   <- do.call(raster::merge, bboxes)
  area1  <- (bbox@xmax - bbox@xmin) * (bbox@ymax - bbox@ymin)
  areas1 <- sum(sapply(bboxes, function(x)  (x@xmax - x@xmin) * (x@ymax - x@ymin)))

  # With option
  option <- list(raster_alignment = 18)
  L      <- catalog_apply(ctg, test, res = 18, .options = option)
  bboxes <- lapply(L, raster::extent)
  bbox   <- do.call(raster::merge, bboxes)
  area2  <- (bbox@xmax - bbox@xmin) * (bbox@ymax - bbox@ymin)
  areas2 <- sum(sapply(bboxes, function(x)  (x@xmax - x@xmin) * (x@ymax - x@ymin)))

  expect_equal(area1, 63504)
  expect_equal(area1, area2)
  expect_gt(areas1, area1)
  expect_equal(area2, areas2)
  expect_message(catalog_apply(ctg, test, res = 18, .options = option), "Chunk size changed")
})

test_that("catalog_apply generates errors if function does not return NULL for empty chunks", {

  test <- function(cluster)
  {
    las <- readLAS(cluster)
    return(0)
  }

  expect_error(catalog_apply(ctg, test), "not return NULL for empty chunks")
})


test_that("catalog_apply writes in file if option is set", {

  # Write a data.frame
  test <- function(cluster)
  {
    las <- readLAS(cluster)
    if (is.empty(las)) return(NULL)
    return(head(las@data))
  }

  opt_output_files(ctg) <- paste0(tempdir(), "/{ID}")

  req <- catalog_apply(ctg, test)
  req <- unlist(req)

  expect_equal(basename(req), paste0(1:4, ".txt"))
  expect_true(all(file.exists(req)))

  # Write a Spatial object
  test <- function(cluster)
  {
    las <- readLAS(cluster)
    if (is.empty(las)) return(NULL)
    las <- lasfilterground(las)
    las <- as.spatial(las)
    return(las)
  }

  opt_output_files(ctg) <- paste0(tempdir(), "/{ID}")

  req <- suppressWarnings(catalog_apply(ctg, test))
  req <- unlist(req)

  expect_equal(basename(req), paste0(1:4, ".shp"))
  expect_true(all(file.exists(req)))
})

test_that("catalog_apply can write with custom drivers", {

  test <- function(cluster)
  {
    las <- readLAS(cluster)
    if (is.empty(las)) return(NULL)
    return(list(0))
  }

  opt_output_files(ctg) <- paste0(tempdir(), "/{ID}")

  expect_error(catalog_apply(ctg, test), "write an object of class list")

  ctg@output_options$drivers$list <- list(
    write     = base::saveRDS,
    object    = "object",
    path      = "file",
    extension = ".rds",
    param     = list(compress = TRUE))

  req <- catalog_apply(ctg, test)
  req <- unlist(req)

  expect_equal(basename(req), paste0(1:4, ".rds"))
  expect_true(all(file.exists(req)))
})

