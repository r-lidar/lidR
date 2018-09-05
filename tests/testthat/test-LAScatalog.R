context("LAScatalog")

LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
ctg     <- catalog(LASfile)

test_that("LAS redefined behavior of $, [, and [[", {

  expect_true(is.character(ctg$`File Signature`))
  expect_equal(length(ctg$`File Signature`), 1)

  expect_true(is.null(ctg$U))

  expect_error(ctg$`File Signature` <- 1, "cannot be modified")

  expect_true(is.character(ctg[["File Signature"]]))
  expect_equal(length(ctg[["File Signature"]]), 1)

  expect_true(is.null(ctg[["U"]]))

  #expect_error(ctg[["File Signature"]] <- 1, "cannot be modified")

  expect_error(ctg[1:2], "not allowed")
  expect_error(ctg[1:2,1:2], "not allowed")
  y <- ctg[1,]
  expect_error(ctg[1] <- 2, "cannot be modified")
  expect_error(ctg[1,2] <- 2, "cannot be modified")
})
