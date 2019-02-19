context("projection")

las = lidR:::dummy_las(10)

test_that("projection with epsg code works", {

  expect_equal(projection(las), NA_character_)

  projection(las) <- sp::CRS("+init=epsg:26917")

  expect_equal(las@header@VLR$GeoKeyDirectoryTag$tags[[1]]$`value offset`, 26917)

  projection(las) <- sp::CRS("+init=epsg:26918")

  expect_equal(las@header@VLR$GeoKeyDirectoryTag$tags[[1]]$`value offset`, 26918)
})

las = lidR:::dummy_las(10)

test_that("projection with wkt code works", {

  las@header@PHB$`Global Encoding`$WKT = TRUE

  projection(las) <- sp::CRS("+init=epsg:26917")

  expect_equal(las@header@VLR$GeoKeyDirectoryTag$tags[[1]]$`value offset`, NULL)
  expect_equal(las@header@VLR$`WKT OGC CS`$`WKT OGC COORDINATE SYSTEM`, "PROJCS[\"NAD_1983_UTM_Zone_17N\",GEOGCS[\"GCS_North_American_1983\",DATUM[\"D_North_American_1983\",SPHEROID[\"GRS_1980\",6378137,298.257222101]],PRIMEM[\"Greenwich\",0],UNIT[\"Degree\",0.017453292519943295]],PROJECTION[\"Transverse_Mercator\"],PARAMETER[\"latitude_of_origin\",0],PARAMETER[\"central_meridian\",-81],PARAMETER[\"scale_factor\",0.9996],PARAMETER[\"false_easting\",500000],PARAMETER[\"false_northing\",0],UNIT[\"Meter\",1]]")

  projection(las) <- sp::CRS("+init=epsg:26918")

  expect_equal(las@header@VLR$GeoKeyDirectoryTag$tags[[1]]$`value offset`, NULL)
  expect_equal(las@header@VLR$`WKT OGC CS`$`WKT OGC COORDINATE SYSTEM`, "PROJCS[\"NAD_1983_UTM_Zone_18N\",GEOGCS[\"GCS_North_American_1983\",DATUM[\"D_North_American_1983\",SPHEROID[\"GRS_1980\",6378137,298.257222101]],PRIMEM[\"Greenwich\",0],UNIT[\"Degree\",0.017453292519943295]],PROJECTION[\"Transverse_Mercator\"],PARAMETER[\"latitude_of_origin\",0],PARAMETER[\"central_meridian\",-75],PARAMETER[\"scale_factor\",0.9996],PARAMETER[\"false_easting\",500000],PARAMETER[\"false_northing\",0],UNIT[\"Meter\",1]]")
})
