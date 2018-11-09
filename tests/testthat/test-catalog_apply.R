context("catalog_apply")

LASfile          <- system.file("extdata", "Megaplot.laz", package="lidR")
ctg              <- catalog(LASfile)
ctg@data         <- ctg@data[1,]
opt_cores(ctg)       <- 1
opt_chunk_buffer(ctg) <- 0
opt_chunk_size(ctg) <- 150
opt_progress(ctg)    <- FALSE

test_that("catalog apply works", {

  test = function(cluster)
  {
    las = readLAS(cluster)
    if (is.empty(las)) return(NULL)
    return(nrow(las@data))
  }

  req = catalog_apply(ctg, test)

  s1 = do.call(sum, req)
  s2 = sum(ctg@data$Number.of.point.records)

  expect_equal(s1,s2)

  test = function(cluster)
  {
    las = readLAS(cluster)
    if (is.empty(las)) return(NULL)
    return(sum(las@data$ReturnNumber == 1))
  }

  req = catalog_apply(ctg, test)

  s1 = do.call(sum, req)
  s2 = sum(ctg@data$Number.of.1st.return)

  expect_equal(s1,s2)
})


test_that("catalog apply write drivers work", {

  test = function(cluster)
  {
    las = readLAS(cluster)
    if (is.empty(las)) return(NULL)
    return(head(las@data))
  }

  opt_output_files(ctg) <- paste0(tempdir(), "/{ID}")

  req = catalog_apply(ctg, test)
  req = unlist(req)

  expect_true(all(file.exists(req)))

  test = function(cluster)
  {
    las = readLAS(cluster)
    if (is.empty(las)) return(NULL)
    las = lasfilterground(las)
    las = as.spatial(las)
    return(las)
  }

  opt_output_files(ctg) <- paste0(tempdir(), "/{ID}")

  req = suppressWarnings(catalog_apply(ctg, test))
  req = unlist(req)

  expect_true(all(file.exists(req)))

})

test_that("catalog apply custom drivers work", {

  test = function(cluster)
  {
    las = readLAS(cluster)
    if (is.empty(las)) return(NULL)
    return(list(0))
  }

  opt_output_files(ctg) <- paste0(tempdir(), "/{ID}")

  expect_error(catalog_apply(ctg, test), "write an object of class list")

  ctg@output_options$drivers$list = list(write = base::saveRDS,
                                         object = "object",
                                         path = "file",
                                         extension = ".rds",
                                         param = list(compress = TRUE))

  req = catalog_apply(ctg, test)
  req = unlist(req)

  expect_true(all(file.exists(req)))
})

