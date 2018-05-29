context("catalog_apply")

LASfile          <- system.file("extdata", "Megaplot.laz", package="lidR")
ctg              <- catalog(LASfile)
ctg@data         <- ctg@data[1]
cores(ctg)       <- 1
buffer(ctg)      <- 0
tiling_size(ctg) <- 150
progress(ctg)    <- FALSE

test_that("catalog apply works", {

  test = function(las){ return(nrow(las@data)) }

  req = catalog_apply(ctg, test)

  s1 = do.call(sum, req)
  s2 = sum(ctg@data$`Number of point records`)

  expect_equal(s1,s2)

  test = function(las){ return(sum(las@data$ReturnNumber == 1)) }

  req = catalog_apply(ctg, test)

  s1 = do.call(sum, req)
  s2 = sum(ctg@data$`Number of 1st return`)

  expect_equal(s1,s2)
})

