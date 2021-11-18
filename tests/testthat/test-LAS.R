context("LAS")

las  <- megaplot
data <- data.frame(X = runif(10), Y = runif(10), Z = runif(10))
outs <- data.frame(x = runif(10), Y = runif(10), z = runif(10), pointsourceid = 1:10)

test_that("Print a LAS object works", {

  sink(tempfile())
  expect_error(print(las), NA)
  expect_error(summary(las), NA)
  sink(NULL)
})

test_that("LAS builds a LAS 1.2 prf 0 object from basic data", {

  las2 <- LAS(data)

  expect_is(las2, "LAS")
  expect_equal(las2@header@PHB$`Version Minor`, 2L)
  expect_equal(las2@header@PHB$`Point Data Format ID`, 0L)
  expect_equal(las2@header@PHB$`Min X`, min(data$X))
  expect_equal(las2@header@PHB$`Min Y`, min(data$Y))
  expect_equal(las2@header@PHB$`Min Z`, min(data$Z))
  expect_equal(las2@header@PHB$`Max X`, max(data$X))
  expect_equal(las2@header@PHB$`Max Y`, max(data$Y))
  expect_equal(las2@header@PHB$`Max Z`, max(data$Z))
})

test_that("LAS builds a LAS 1.2 prf 1 object from time data", {

  data$gpstime   <- runif(10, 0, 100)
  data$Intensity <- as.integer(runif(10, 0, 100))
  las2 <- LAS(data)

  expect_is(las2, "LAS")
  expect_equal(las2@header@PHB$`Version Minor`, 2L)
  expect_equal(las2@header@PHB$`Point Data Format ID`, 1L)
  expect_equal(las2@header@PHB$`Min X`, min(data$X))
  expect_equal(las2@header@PHB$`Min Y`, min(data$Y))
  expect_equal(las2@header@PHB$`Min Z`, min(data$Z))
  expect_equal(las2@header@PHB$`Max X`, max(data$X))
  expect_equal(las2@header@PHB$`Max Y`, max(data$Y))
  expect_equal(las2@header@PHB$`Max Z`, max(data$Z))
})

test_that("LAS builds a LAS 1.2 prf 3 object from RGB data", {

  data$gpstime   <- runif(10, 0, 100)
  data$Intensity <- as.integer(runif(10, 0, 100))
  data$R         <- as.integer(runif(10, 0, 2^16))
  data$G         <- as.integer(runif(10, 0, 2^16))
  data$B         <- as.integer(runif(10, 0, 2^16))
  las2 <- LAS(data)

  expect_is(las2, "LAS")
  expect_equal(las2@header@PHB$`Version Minor`, 2L)
  expect_equal(las2@header@PHB$`Point Data Format ID`, 3L)
})

test_that("LAS builds a LAS object respecting a header", {

  las2 <- LAS(data, las@header, check = FALSE)

  expect_is(las2, "LAS")
  expect_equal(las2@header@PHB$`Version Minor`, las@header@PHB$`Version Minor`)
  expect_equal(las2@header@PHB$`Point Data Format ID`, las@header@PHB$`Point Data Format ID`)
  expect_equal(las2@header@PHB$`Min X`, min(data$X))
  expect_equal(las2@header@PHB$`Min Y`, min(data$Y))
  expect_equal(las2@header@PHB$`Min Z`, min(data$Z))
  expect_equal(las2@header@PHB$`Max X`, max(data$X))
  expect_equal(las2@header@PHB$`Max Y`, max(data$Y))
  expect_equal(las2@header@PHB$`Max Z`, max(data$Z))
})

test_that("LAS builds a LAS object with a CRS", {

  las2 <- LAS(data)

  expect_true(is.na(las2@crs))
  expect_equal(epsg(las2), 0)

  las2 <- LAS(data, las@header, check = FALSE)

  expect_equal(st_crs(las2), st_crs(las))
  expect_equal(epsg(las2), 26917)

  las2 <- LAS(data, crs = las@crs)

  expect_equal(st_crs(las2), st_crs(las))
  #expect_equal(epsg(las2), 26917)
})

test_that("LAS throws a warning/error if building an invalid LAS", {

  # Invalid point format

  data$R <- as.integer(runif(10, 0, 2^16))
  data$G <- as.integer(runif(10, 0, 2^16))
  data$B <- as.integer(runif(10, 0, 2^16))

  expect_warning(LAS(data, las@header), "point data format")

  # Invalid scale factor

  header <- las@header
  header@PHB$`Point Data Format ID` <- 3L
  header@PHB$`X scale factor` <- 0.412

  expect_warning(LAS(data, header), "X scale factor")

  # Invalid RGB

  data$R <- as.integer(runif(10, 0, 2^8))
  data$G <- as.integer(runif(10, 0, 2^8))
  data$B <- as.integer(runif(10, 0, 2^8))

  header <- las@header
  header@PHB$`Point Data Format ID` <- 3L

  expect_warning(LAS(data, header), "8 bits")

  # Invalid coordinates

  data$X <- as.integer(runif(10, 1, 100))
  expect_error(LAS(data), "double")

  data$X <- NULL
  expect_error(LAS(data), "coordinates")

  # Invalid ReturnNumber

  data$X <- runif(10)
  data$R <- NULL
  data$G <- NULL
  data$B <- NULL
  data$ReturnNumber <- as.integer(rep(1, 10L))
  data$ReturnNumber[5] <- 0L

  expect_warning(LAS(data), "return number")

  # Invalid NumberOfReturns

  set.seed(0)
  data$ReturnNumber <- as.integer(sample(1:3, 10, replace = TRUE))
  set.seed(1)
  data$NumberOfReturns <- as.integer(sample(1:3, 10, replace = TRUE))

  expect_warning(LAS(data), "number of returns")

  # Invalid Version

  header <- las@header
  header@PHB$`Version Minor` <- NULL
  data <- data.frame(X = runif(10), Y = runif(10), Z = runif(10))

  expect_error(LAS(data, header), "Version")

  # Non quantized coordinates

  header@PHB$`Version Minor` <- 1L
  expect_warning(LAS(data, header), "quantization errors")
})

test_that("LAS redefined behavior of $, [, and [[", {

  las <- random_10_points

  expect_true(is.numeric(las$Z))
  expect_equal(length(las$Z), 10)

  expect_true(is.null(las$U))

  expect_error(las$Z <- 1:10, "replace data of type double")
  expect_error(las$U <- 1:10, "Addition of a new column")

  expect_true(is.numeric(las[["Z"]]))
  expect_equal(length(las[["Z"]]), 10)

  expect_true(is.null(las[["U"]]))

  expect_error(las[["Z"]] <- 1:10, "replace data of type double")
  expect_error(las[["U"]] <- 1:10, "Addition of a new column")
})

test_that("LAS operator $ quantize on the fly and update header", {

  las <- random_10_points

  x = runif(10)
  y = runif(10)
  z = runif(10)

  las$X <- x
  las$Y <- y
  las$Z <- z

  expect_equal(las$X, quantize(x, 0.001, 0, FALSE))
  expect_equal(las$Y, quantize(y, 0.001, 0, FALSE))
  expect_equal(las$Z, quantize(z, 0.001, 0, FALSE))

  xbbox = range(quantize(x, 0.001, 0, FALSE))
  ybbox = range(quantize(y, 0.001, 0, FALSE))

  expect_equal(las@header@PHB[["Min X"]], xbbox[1])
  expect_equal(las@header@PHB[["Min Y"]], ybbox[1])
})

test_that("LAS operator [[ quantize on the fly and update header", {

  las <- random_10_points

  x = runif(10)
  y = runif(10)
  z = runif(10)

  las[["X"]] <- x
  las[["Y"]] <- y
  las[["Z"]] <- z

  expect_equal(las$X, quantize(x, 0.001, 0, FALSE))
  expect_equal(las$Y, quantize(y, 0.001, 0, FALSE))
  expect_equal(las$Z, quantize(z, 0.001, 0, FALSE))

  xbbox = range(quantize(x, 0.001, 0, FALSE))
  ybbox = range(quantize(y, 0.001, 0, FALSE))

  expect_equal(las@header@PHB[["Min X"]], xbbox[1])
  expect_equal(las@header@PHB[["Min Y"]], ybbox[1])
})

test_that("LAS operator[[ and $ throw error for not storable coordinates", {
  las <- random_10_points
  x <- round(runif(10),2)
  x[5] <- 21474836.47

  expect_error(las$X <- x, "Trying to store values ranging in")
  expect_error(las$Y <- x, "Trying to store values ranging in")
  expect_error(las[["X"]] <- x, "Trying to store values ranging in")
  expect_error(las[["Y"]] <- x, "Trying to store values ranging in")
})


test_that("LAS conversion to SpatialPointsDataFrame works", {
  las <- random_10_points
  splas <- as.spatial(las)

  expect_true(is(splas, "SpatialPointsDataFrame"))
})

test_that("LAS build an empty point cloud with no header (#314)", {
  las = LAS(data.frame(X = numeric(0), Y = numeric(0), Z = numeric(0)))
  expect_equal(npoints(las), 0L)
  expect_equal(names(las), c("X", "Y", "Z"))
})

test_that("LAS rename the attribute", {
  x <- outs[["x"]]
  las = LAS(outs)
  expect_equal(names(las), c("X", "Y", "Z", "PointSourceID"))
  expect_reference(las@data[["X"]], x)
})

