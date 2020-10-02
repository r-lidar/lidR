context("projection")

las <- lidR:::dummy_las(10)

test_that("Internal projection conversion works", {

  wkt <- rgdal::showWKT("+init=epsg:2008")

  if (rgdal::new_proj_and_gdal()) {
    expected <- sp::CRS(SRS_string = "EPSG:2008")
  } else {
    expected <- sp::CRS("+init=epsg:2008")
    expected@projargs <- sub("\\+init=epsg:\\d+\\s", "", expected@projargs)
  }

  expect_equal(lidR:::epsg2CRS(2008), expected)
  expect_equal(lidR:::epsg2CRS(200800), sp::CRS())
  expect_error(lidR:::epsg2CRS(200800, fail = TRUE), "Invalid epsg code")

  #expect_equal(lidR:::wkt2CRS(wkt), expected) # commented because with rgdal 1.5-8 it returns a slightly modified WKT comment
  expect_equal(lidR:::wkt2CRS("INVALID")@projargs, NA_character_)
  expect_error(lidR:::wkt2CRS("INVALID", fail = TRUE), "Invalid WKT")
})

test_that("projection<- with EPSG works", {

  projection(las) <- 26919

  expect_equal(epsg(las), 26919)

  projection(las) <- 26918

  expect_equal(epsg(las), 26918)
})


test_that("projection<- with CRS works", {

  projection(las) <- sp::CRS("+init=epsg:26917")

  expect_equal(epsg(las), 26917)

  projection(las) <- sp::CRS("+init=epsg:26918")

  expect_equal(epsg(las), 26918)
})

test_that("crs<- with epsg code works", {

  crs(las) <- sp::CRS("+init=epsg:26917")

  expect_equal(epsg(las), 26917)

  crs(las) <- sp::CRS("+init=epsg:26918")

  expect_equal(epsg(las), 26918)
})

test_that("projection<- with wkt code works", {

  las@header@PHB[["Global Encoding"]][["WKT"]] <- TRUE

  if (rgdal::new_proj_and_gdal())
  {
    projection(las) <- sp::CRS(SRS_string = "EPSG:26919")
    expect_match(wkt(las), "NAD83 / UTM zone 19N")

    projection(las) <- sp::CRS(SRS_string = "EPSG:26918")
    expect_match(wkt(las), "NAD83 / UTM zone 18N")
  }
  else
  {
    projection(las) <- sp::CRS("+init=epsg:26919")
    expect_match(wkt(las), "PROJCS")

    projection(las) <- sp::CRS("+init=epsg:26918")
    expect_match(wkt(las), "PROJCS")
  }
})

test_that("crs<- with wkt code works", {

  las@header@PHB[["Global Encoding"]][["WKT"]] <- TRUE

  if (rgdal::new_proj_and_gdal())
  {
    crs(las) <- sp::CRS(SRS_string = "EPSG:26919")
    expect_match(wkt(las), "NAD83 / UTM zone 19N")

    crs(las) <- sp::CRS(SRS_string = "EPSG:26918")
    expect_match(wkt(las), "NAD83 / UTM zone 18N")
  }
  else
  {
    crs(las) <- sp::CRS("+init=epsg:26919")
    expect_match(wkt(las), "PROJCS")

    crs(las) <- sp::CRS("+init=epsg:26918")
    expect_match(wkt(las), "PROJCS")
  }
})

test_that("epsg<- works", {

  epsg(las) <- 26917
  expect_equal(epsg(las), 26917)
})

test_that("Set an invalid code or WKT fails", {
  expect_error({ epsg(las) <- 200800 }, "Invalid epsg")
  expect_error({ wkt(las) <- "INVALID" }, "Invalid WKT")
})

test_that("#323 do not segfault", {
  wkt = "PROJCS[\"NAD83 (2011) / Conus Albers\",GEOGCS[\"GCS_NAD_1983_2011\",DATUM[\"D_NAD_1983_2011\",SPHEROID[\"GRS_1980\",6378137.0,298.257222101,AUTHORITY[\"EPSG\",7019]],AUTHORITY[\"EPSG\",1116]],PRIMEM[\"Greenwich\",0.0,AUTHORITY[\"EPSG\",8901]],UNIT[\"Degree\",0.0174532925199433,AUTHORITY[\"EPSG\",9102]],AUTHORITY[\"EPSG\",6318]],PROJECTION[\"Albers\",AUTHORITY[\"Esri\",43007]],PARAMETER[\"False_Easting\",0.0,AUTHORITY[\"Esri\",100001]],PARAMETER[\"False_Northing\",0.0,AUTHORITY[\"Esri\",100002]],PARAMETER[\"Central_Meridian\",-96.0,AUTHORITY[\"Esri\",100010]],PARAMETER[\"Standard_Parallel_1\",29.5,AUTHORITY[\"Esri\",100025]],PARAMETER[\"Standard_Parallel_2\",45.5,AUTHORITY[\"Esri\",100026]],PARAMETER[\"Latitude_Of_Origin\",23.0,AUTHORITY[\"Esri\",100021]],UNIT[\"Meter\",1.0,AUTHORITY[\"EPSG\",9001]]],VERTCS[\"NAVD_1988\",VDATUM[\"North_American_Vertical_Datum_1988\",AUTHORITY[\"EPSG\",5103]],PARAMETER[\"Vertical_Shift\",0.0,AUTHORITY[\"Esri\",100006]],PARAMETER[\"Direction\",1.0,AUTHORITY[\"Esri\",100007]],UNIT[\"Meter\",1.0,AUTHORITY[\"EPSG\",9001]],AUTHORITY[\"EPSG\",5703]]"
  expect_error(lidR:::wkt2CRS(wkt), NA)
})
