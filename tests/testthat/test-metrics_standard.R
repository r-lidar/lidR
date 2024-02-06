las = clip_rectangle(megaplot, 684766.4, 5017773, 684866.4, 5017973)

xall = pixel_metrics(las, .stdmetrics, 20)

test_that("stdmetric returns the same result than .stdmetric", {
  y = pixel_metrics(las, ~stdmetrics(X,Y,Z, Intensity, ReturnNumber, Classification), 20)
  expect_equal(xall, y)
})

test_that("stdmetric_i returns the same result than .stdmetric_i", {
  x = pixel_metrics(las, .stdmetrics_i, 20)
  y = pixel_metrics(las, ~stdmetrics_i(Intensity, Z, Classification), 20)

  expect_equal(x,y)
})

test_that("stdmetric_pulse works", {
  las = retrieve_pulses(las)
  expect_error(pixel_metrics(las, .stdmetrics_pulse, 20), NA)
})

