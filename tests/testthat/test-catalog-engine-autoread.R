context("catalog_apply autoread")

ctg <- random_2files_250points
opt_progress(ctg) = FALSE

test <- function(las, bbox, layers = 1L) {
  sp = as.spatial(las)
  sp = raster::crop(sp, bbox)
  return(sp)
}

test_that("catalog_apply autoread works", {
  # automerge option
  option <- list(autoread = TRUE, automerge = TRUE)
  req2 <- catalog_apply(ctg, test, .options = option)

  expect_equal(nrow(req2), 500L)
})

