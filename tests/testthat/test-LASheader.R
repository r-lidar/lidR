test_that("$ and [[ works", {
  expect_equal(megaplot$`File Signature`, "LASF")
  expect_equal(megaplot$`X scale factor`, 0.01)
})

test_that("$<- and [[<-] works", {
  megaplot$`File Signature` <- "ABC"
  megaplot$`X scale factor` <- 0.1
  expect_equal(megaplot[["File Signature"]], "ABC")
  expect_equal(megaplot[["X scale factor"]], 0.1)
})

test_that("vlr works", {
  VLR <- vlr(megaplot)
  EVLR <- evlr(megaplot)
  expect_is(VLR, "list")
  expect_is(EVLR, "list")

  VLR <- vlr(header(megaplot))
  EVLR <- evlr(header(megaplot))
  expect_is(VLR, "list")
  expect_is(EVLR, "list")
})
