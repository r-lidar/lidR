context("catalog_apply")

# Generate a catalog
xshift <- c(0, 100, 0, 100)
yshift <- c(0, 0, 100, 100)
shift  <- cbind(xshift, yshift)
temp   <- sapply(1:nrow(shift), function(x) tempfile(fileext = ".las"))

for (i in 1:nrow(shift))
{
  las <- lidR:::dummy_las(500, seeds = i)
  las@data[, X := X + shift[i,1]]
  las@data[, Y := Y + shift[i,2]]
  writeLAS(las, temp[i])
}

ctg <- readLAScatalog(temp)
opt_chunk_buffer(ctg)    <- 0
opt_chunk_size(ctg)      <- 150
opt_progress(ctg)        <- FALSE
opt_output_files(ctg)    <- ""

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

test_that("catalog_apply automerge works with raster", {

  test <- function(cluster)
  {
    las <- readLAS(cluster)
    if (is.empty(las)) return(NULL)
    r = lidR:::rOverlay(las, 2)
    r[] <- 1L
    r[55:56] <- NA
    return(r)
  }

  opt_chunk_size(ctg) <- 0

  # No automerge option
  req1 <- catalog_apply(ctg, test)
  req1 <- lidR:::rMergeList(req1)

  # automerge option
  option <- list(automerge = TRUE)
  req2 <- catalog_apply(ctg, test, .options = option)

  # automerge + vrt
  #if (.Machine$sizeof.pointer == 8)
  #{
  #  opt_output_files(ctg) <- paste0(tempdir(), "/{ORIGINALFILENAME}")
  #  req3 <- catalog_apply(ctg, test, .options = option)
  #}

  expect_is(req1, "RasterLayer")
  expect_is(req2, "RasterLayer")
  expect_equal(req1, req2)
  expect_equal(raster::extent(req2), raster::extent(0,200,0,200))

  #if (.Machine$sizeof.pointer == 8)
  #{
  #  expect_is(req3, "RasterLayer")
  #  expect_equal(raster::extent(req3), raster::extent(0,200,0,200))
  #  expect_equal(req2[], req3[])
  #  expect_equal(sum(is.na(req3[])), 8L)
  #}
})

test_that("catalog_apply automerge works with spatial points", {

  test <- function(cluster)
  {
    las <- readLAS(cluster)
    if (is.empty(las)) return(NULL)
    return(sp::SpatialPoints(head(lidR:::coordinates(las))))
  }

  opt_chunk_size(ctg)      <- 0

  option <- list(automerge = TRUE)
  req2 <- catalog_apply(ctg, test, .options = option)

  expect_is(req2, "SpatialPoints")

  test <- function(cluster)
  {
    las <- readLAS(cluster)
    if (is.empty(las)) return(NULL)
    return(sp::SpatialPointsDataFrame(head(lidR:::coordinates(las)), data.frame(u = runif(6))))
  }

  option <- list(automerge = TRUE)
  req2 <- catalog_apply(ctg, test, .options = option)

  opt_output_files(ctg) <- paste0(tempdir(), "/{ORIGINALFILENAME}")
  req3 <- catalog_apply(ctg, test, .options = option)

  expect_is(req2, "SpatialPointsDataFrame")
  expect_equal(dim(req2), c(24,1))
  expect_true(is.character(unlist(req3)))
})

test_that("catalog_apply automerge works with LAS", {

  test <- function(cluster)
  {
    las <- readLAS(cluster)
    if (is.empty(las)) return(NULL)
    LAS(head(lidR:::coordinates3D(las)))
  }

  opt_chunk_size(ctg) <- 0

  option <- list(automerge = TRUE)
  req2 <- catalog_apply(ctg, test, .options = option)

  opt_output_files(ctg) <- paste0(tempdir(), "/{ORIGINALFILENAME}")
  req3 <- catalog_apply(ctg, test, .options = option)

  expect_is(req2, "LAS")
  expect_equal(npoints(req2), 24L)
  expect_is(req3, "LAScatalog")
})

test_that("catalog_apply return partial ouptut generates logs", {

  test <- function(cluster)
  {
    if (raster::extent(cluster)@ymin > 80)
      stop("Test error")

    return(0)
  }

  opt_chunk_size(ctg) <- 0
  opt_wall_to_wall(ctg) <- FALSE

  req = suppressMessages(catalog_apply(ctg, test))

  expect_is(req, "list")
  expect_equal(length(req), 2)
  expect_message(catalog_apply(ctg, test), "chunk3.rds")
  expect_message(catalog_apply(ctg, test), "Test error")
})

