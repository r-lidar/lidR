context("projection")

las = random_10_points

test_that("st_crs works", {

  expect_error(st_crs(las), NA)
  expect_error(st_crs(header(las)), NA)
})

test_that("Internal projection conversion works", {

  wkt <- "PROJCS[\"NAD27(CGQ77) / SCoPQ zone 2\",GEOGCS[\"NAD27(CGQ77)\",DATUM[\"North_American_Datum_1927_CGQ77\",SPHEROID[\"Clarke 1866\",6378206.4,294.978698213898,AUTHORITY[\"EPSG\",\"7008\"]],AUTHORITY[\"EPSG\",\"6609\"]],PRIMEM[\"Greenwich\",0,AUTHORITY[\"EPSG\",\"8901\"]],UNIT[\"degree\",0.0174532925199433,AUTHORITY[\"EPSG\",\"9122\"]],AUTHORITY[\"EPSG\",\"4609\"]],PROJECTION[\"Transverse_Mercator\"],PARAMETER[\"latitude_of_origin\",0],PARAMETER[\"central_meridian\",-55.5],PARAMETER[\"scale_factor\",0.9999],PARAMETER[\"false_easting\",304800],PARAMETER[\"false_northing\",0],UNIT[\"metre\",1,AUTHORITY[\"EPSG\",\"9001\"]],AXIS[\"Easting\",EAST],AXIS[\"Northing\",NORTH]]"
  expected <- sf::st_crs(2008)

  expect_equal(lidR:::epsg2crs(2008), expected)
  expect_equal(lidR:::epsg2crs(200800), sf::NA_crs_)
  expect_error(lidR:::epsg2crs(200800, fail = TRUE), "Invalid epsg code")
  expect_equal(lidR:::wkt2crs("INVALID"), sf::NA_crs_)
  expect_error(lidR:::wkt2crs("INVALID", fail = TRUE), "Invalid WKT")
})

test_that("st_crs<- with EPSG works", {
  st_crs(las) <- 26919
  expect_equal(epsg(las), 26919)

  st_crs(las) <- 26918
  expect_equal(epsg(las), 26918)
})

test_that("st_crs<- with crs works", {
  st_crs(las) <- sf::st_crs(26919)
  expect_equal(epsg(las), 26919)

  st_crs(las) <- sf::st_crs(26918)
  expect_equal(epsg(las), 26918)
})

test_that("st_crs<- with wkt works", {

  las@header@PHB[["Global Encoding"]][["WKT"]] <- TRUE

  st_crs(las) <- 26919
  expect_match(wkt(las), "NAD83 / UTM zone 19N")

  st_crs(las) <- 26918
  expect_match(wkt(las), "NAD83 / UTM zone 18N")
})

test_that("epsg<- works", {

  epsg(las) <- 26917
  expect_equal(epsg(las), 26917)
})

test_that("Set an invalid code or WKT fails", {
  expect_error({ epsg(las) <- 200800 }, "Invalid epsg")

  las@header@PHB[["Global Encoding"]][["WKT"]] <- TRUE

  expect_error({ wkt(las) <- "INVALID" }, "Invalid WKT")
})

test_that("#323 do not segfault", {
  wkt = "PROJCS[\"NAD83 (2011) / Conus Albers\",GEOGCS[\"GCS_NAD_1983_2011\",DATUM[\"D_NAD_1983_2011\",SPHEROID[\"GRS_1980\",6378137.0,298.257222101,AUTHORITY[\"EPSG\",7019]],AUTHORITY[\"EPSG\",1116]],PRIMEM[\"Greenwich\",0.0,AUTHORITY[\"EPSG\",8901]],UNIT[\"Degree\",0.0174532925199433,AUTHORITY[\"EPSG\",9102]],AUTHORITY[\"EPSG\",6318]],PROJECTION[\"Albers\",AUTHORITY[\"Esri\",43007]],PARAMETER[\"False_Easting\",0.0,AUTHORITY[\"Esri\",100001]],PARAMETER[\"False_Northing\",0.0,AUTHORITY[\"Esri\",100002]],PARAMETER[\"Central_Meridian\",-96.0,AUTHORITY[\"Esri\",100010]],PARAMETER[\"Standard_Parallel_1\",29.5,AUTHORITY[\"Esri\",100025]],PARAMETER[\"Standard_Parallel_2\",45.5,AUTHORITY[\"Esri\",100026]],PARAMETER[\"Latitude_Of_Origin\",23.0,AUTHORITY[\"Esri\",100021]],UNIT[\"Meter\",1.0,AUTHORITY[\"EPSG\",9001]]],VERTCS[\"NAVD_1988\",VDATUM[\"North_American_Vertical_Datum_1988\",AUTHORITY[\"EPSG\",5103]],PARAMETER[\"Vertical_Shift\",0.0,AUTHORITY[\"Esri\",100006]],PARAMETER[\"Direction\",1.0,AUTHORITY[\"Esri\",100007]],UNIT[\"Meter\",1.0,AUTHORITY[\"EPSG\",9001]],AUTHORITY[\"EPSG\",5703]]"
  expect_error(lidR:::wkt2crs(wkt), NA)
})

test_that("legacy crs works", {
   expect_is(crs(las), "CRS")
})

test_that("legacy projection works", {
  expect_is(projection(las), "character")
})


test_that("legacy crs<- works", {

  skip_on_cran() # For solaris old GDAL/PROJ

  las@header@PHB[["Global Encoding"]][["WKT"]] <- TRUE

  crs(las) <- as(sf::st_crs(26919), "CRS")
  expect_match(wkt(las), "NAD83 / UTM zone 19N")

  las@header@PHB[["Global Encoding"]][["WKT"]] <- FALSE

  crs(las) <- as(sf::st_crs(26918), "CRS")
  expect_equal(epsg(las), 26918)

  las@header@PHB[["Global Encoding"]][["WKT"]] <- TRUE

  crs(las) <- sf::st_crs(26919)
  expect_match(wkt(las), "NAD83 / UTM zone 19N")

  las@header@PHB[["Global Encoding"]][["WKT"]] <- FALSE

  crs(las) <- sf::st_crs(26918)
  expect_equal(epsg(las), 26918)
})

test_that("legacy projection<- works with CRS and crs", {

  skip_on_cran() # For solaris old GDAL/PROJ

  projection(las) <- as(sf::st_crs(26917), "CRS")
  expect_equal(epsg(las), 26917)

  projection(las) <- as(sf::st_crs(26918), "CRS")
  expect_equal(epsg(las), 26918)

  projection(las) <- sf::st_crs(26917)
  expect_equal(epsg(las), 26917)

  projection(las) <- sf::st_crs(26918)
  expect_equal(epsg(las), 26918)
})

