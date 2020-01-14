context("catalog_apply writemode")

ctg <- lidR:::catalog_generator(10)

test <- function(cluster) {
  las <- readLAS(cluster)
  if (is.empty(las)) return(NULL)
  head(lidR:::coordinates3D(las))
}

test_that("catalog_apply respects ORIGINALFILENAME template", {

  opt_output_files(ctg) <- paste0(tempdir(), "/{ORIGINALFILENAME}")
  o <- catalog_apply(ctg, test)
  o <- unlist(o)

  ifiles <- tools::file_path_sans_ext(basename(ctg$filename))
  ofiles <- tools::file_path_sans_ext(basename(o))

  expect_equal(ifiles, ofiles)
})

test_that("catalog_apply fails with ORIGINALFILENAME template if chunks != files", {

  opt_chunk_size(ctg) <- 75
  opt_output_files(ctg) <- paste0(tempdir(), "/{ORIGINALFILENAME}")

  expect_error(catalog_apply(ctg, test))
})

test_that("catalog_apply respects ID template", {

  opt_output_files(ctg) <- paste0(tempdir(), "/p{ID}")
  o <- catalog_apply(ctg, test)
  o <- unlist(o)

  i <- paste0("p", 1:4)
  o <- tools::file_path_sans_ext(basename(o))

  expect_equal(i, o)
})

test_that("catalog_apply respects COORDINATES template", {

  opt_chunk_size(ctg) <- 75
  opt_output_files(ctg) <- paste0(tempdir(), "/{XLEFT}_{YBOTTOM}")
  o <- catalog_apply(ctg, test)
  o <- unlist(o)

  i <- c("0_0", "75_0", "150_0", "0_75", "75_75", "150_75", "0_150", "75_150", "150_150")
  o <- tools::file_path_sans_ext(basename(o))

  expect_equal(i, o)
})

test_that("catalog_apply can write with custom drivers", {

  test <- function(cluster) {
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

test_that("catalog_apply can create missing directories", {

 opt_output_files(ctg) <- paste0(tempdir(), "/subfolder/{ID}")

 req <- catalog_apply(ctg, test)

 expect_true(dir.exists(paste0(tempdir(), "/subfolder/")))
})
