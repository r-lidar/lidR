context("catalog_apply autoread")

ctg <- lidR:::catalog_generator(250)

rtest <- function(las, bbox, layers = 1L) {
  r = lidR:::rOverlay(las, 2)
  r[] <- 1L
  r[55:56] <- NA
  if (layers > 1) {
    r <- raster::brick(r,r)
    layers <- 2L
  }
  names(r) <- paste0("layername", 1:layers)
  return(r)
}

test_that("catalog_apply autoread works", {
  # automerge option
  option <- list(autoread = TRUE, automerge = TRUE)
  req2 <- catalog_apply(ctg, rtest, .options = option)

  expect_true(raster::inMemory(req2))
  expect_equal(names(req2), "layername1")
  expect_equal(raster::extent(req2), raster::extent(0,200,0,200))
  expect_equal(sum(is.na(req2[])), 8L)
})

