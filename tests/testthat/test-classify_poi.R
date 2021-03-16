shp <- system.file("extdata", "lake_polygons_UTM17.shp", package = "lidR")
lake <- sf::st_read(shp, quiet = TRUE)

test_that("classify_poi works with LAS", {
  megaplot <- classify_poi(megaplot, LASHIGHVEGETATION, poi = ~Classification != 2, roi = lake, inverse = TRUE)
  cl <- megaplot$Classification
  expect_equal(sum(megaplot$Classification == LASHIGHVEGETATION), 71953L)
  expect_equal(sum(megaplot$Classification == LASGROUND), 7389L)

  megaplot <- classify_poi(megaplot, LASWATER, roi = lake, inverse = FALSE, by_reference = TRUE)

  expect_equal(sum(megaplot$Classification == LASHIGHVEGETATION), 71953L)
  expect_equal(sum(megaplot$Classification == LASGROUND), 2599L)
  expect_equal(sum(megaplot$Classification == LASWATER), 7038L)
  expect_reference(megaplot$Classification, cl)
})


test_that("classify_poi works with LAScatalog", {

  skip_on_cran()

  opt_chunk_size(megaplot_ctg) = 250
  opt_chunk_alignment(megaplot_ctg) = c(0, 100)
  opt_output_files(megaplot_ctg) = "{tempdir()}/{ID}_classified"
  opt_progress(megaplot_ctg) = FALSE

  megaplot_ctg <- classify_poi(megaplot_ctg, LASHIGHVEGETATION, poi = ~Classification != 2, roi = lake, inverse = TRUE)

  megaplot = readLAS(megaplot_ctg)
  expect_equal(sum(megaplot$Classification == LASHIGHVEGETATION), 71953L)
  expect_equal(sum(megaplot$Classification == LASGROUND), 7389L)

  opt_output_files(megaplot_ctg) = "{tempdir()}/{ID}_classified2"
  opt_progress(megaplot_ctg) = FALSE
  megaplot_ctg <- classify_poi(megaplot_ctg, LASWATER, roi = lake, inverse = FALSE, by_reference = TRUE)

  megaplot = readLAS(megaplot_ctg)
  expect_equal(sum(megaplot$Classification == LASHIGHVEGETATION), 71953L)
  expect_equal(sum(megaplot$Classification == LASGROUND), 2599L)
  expect_equal(sum(megaplot$Classification == LASWATER), 7038L)
})
