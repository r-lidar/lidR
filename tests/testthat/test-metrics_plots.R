test_that("plot_metrics works if some plot contain 0 points (#551)", {

  shpfile <- system.file("extdata", "efi_plot.shp", package="lidR")
  inventory <- sf::st_read(shpfile, quiet = TRUE)
  inventory$geometry[4][[1]][2] = 5017891 + 300

  expect_warning(m <- plot_metrics(megaplot, .stdmetrics_z, inventory, radius = 11.28), "are empty")
  expect_equal(nrow(m), nrow(inventory))
  expect_equal(m[4,]$zmax, NA_real_)
  expect_equal(m[4,]$zq5, NA_real_)
})
