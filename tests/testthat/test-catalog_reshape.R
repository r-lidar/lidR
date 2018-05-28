context("catalog_reshape")

# lidr_options(interactive = FALSE)
# folder <- system.file("extdata", "", package="lidR")
# ctg = catalog(folder)
# cores(ctg) <- 1
# progress(ctg) <- FALSE

# test_that("catalog reshape works", {
#   ctg = catalog(folder)
#   progress(ctg) <- FALSE
#   ctg@data = ctg@data[1]
#   temp = tempfile()
#
#   ctg2 = catalog_reshape(ctg, 80, temp, prefix = "test_")
#
#   unlink(temp, recursive = T)
#
#   expect_equal(sum(ctg@data$`Number of point records`), sum(ctg2@data$`Number of point records`))
#   expect_equal(nrow(ctg2@data), 9)
# })

#sink(NULL)
