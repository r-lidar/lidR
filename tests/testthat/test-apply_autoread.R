test_that("catalog_apply autoread works", {

  skip_on_cran()

  ctg <- random_2files_250points
  opt_progress(ctg) = FALSE

  test <- function(las, bbox, layers = 1L) {
    sf <- sf::st_as_sf(las[1:10])
    sf::st_agr(sf) <- "constant"
    sp <- sf::st_crop(sf, bbox)
    return(sp)
  }

  # automerge option
  option <- list(autoread = TRUE, automerge = TRUE)
  req2 <- catalog_apply(ctg, test, .options = option)

  expect_equal(nrow(req2), 20)
})

