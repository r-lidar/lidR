context("engine_index")

data <- data.table::data.table(
  Max.X   = c(885228.88, 886993.96, 885260.93, 887025.96,
              885292.94, 887056.88, 892199.94, 893265.54, 892229.99, 893295.15,
              888759.96, 890524.95, 892259.98, 894025.98, 892289.96, 894055.93,
              888790.91, 890554.98, 888820.95, 890585.99, 892319.96, 894084.97,
              892349.89, 894114.29, 895250.23, 895094.78, 895044.96, 895053.55,
              885323.96, 887087.95, 885355.95, 887119.96, 883657.85, 885387.95,
              887150.97, 885419.98, 887182.95, 883688.44, 885442.91, 887193.9,
              888851.96, 890615.97, 888882.94, 890646.97, 892379.94, 894127.84,
              892409.97, 892676.58, 888913.92, 890676.93, 888944.86, 890707.98,
              892439.95, 894124.59, 892469.86, 894232.94, 894786.68, 888958.83,
              890713.51, 892476.43, 894239.97, 894786.07),
  Min.X   = c(885022.37,
              885204.73, 885027.52, 885229.03, 885040.86, 885261.03, 891503.09,
              892198.69, 891501.42, 892200.07, 886970.07, 888735.55, 891499.96,
              892230.05, 890521.99, 892260.01, 886994.05, 888760.09, 887026.07,
              888791.01, 890525.05, 892290.04, 890555.01, 892320.12, 894002.98,
              894026.02, 894056.02, 894085.03, 885051.45, 885293.03, 885063.29,
              885324.03, 883166.09, 885072.16, 885356.09, 883642.36, 885388.15,
              883180.23, 883658.11, 885420.02, 887057.07, 888821.02, 887088.11,
              888852.03, 890586.03, 892350.02, 890616.07, 892380.01, 887120.07,
              888883.03, 887151.11, 888914.02, 890647.06, 892410.06, 890677.07,
              892440.07, 894209.19, 887183.07, 888945.12, 890708.03, 892470.16,
              894233.07),
  Max.Y   = c(630219.48, 630214.96, 631609.95, 631604.97,
              633001.65, 632995.99, 625898.35, 625882.94, 627289.82, 627273.89,
              630174.88, 630134.94, 628681.66, 628664.99, 630094.95, 630057.95,
              631564.98, 631524.94, 632955.82, 632915.99, 631486.9, 631447.96,
              632876.93, 632838.96, 628627.89, 630019.93, 631410.97, 631740.88,
              634393.05, 634386.96, 635786.24, 635779.96, 638613.36, 637176.84,
              637169.92, 638601.99, 638560.96, 639938.36, 639926.95, 639558.31,
              634346.93, 634307.92, 635739.92, 635699.92, 634268.97, 634229.95,
              635659.89, 635622.88, 637129.84, 637089.93, 638520.94, 638481.91,
              637051.99, 637012.92, 638442.98, 638403.94, 638366.87, 639177.04,
              639133.74, 638702.56, 638702.56, 638702.56),
  Min.Y   = c(629157.18,
              629099.31, 630215.04, 630175.05, 631605.02, 631565.05, 625816.52,
              625793.6, 625883.01, 625860.81, 629036.82, 629017.72, 627274.01,
              627251.36, 628665.04, 628628.01, 630135.08, 630095.02, 631525.01,
              631487.19, 630058.02, 630020.05, 631448.08, 631411.03, 627506.32,
              628612.41, 629999.84, 631390.38, 632996.06, 632956.04, 634387.01,
              634347.01, 637939.24, 635780.07, 635740.05, 637170.11, 637130.14,
              638602.13, 638561.04, 638521.07, 632916.05, 632877.04, 634308.06,
              634269.04, 632839.06, 632801.04, 634230.04, 634223.9, 635700.07,
              635660.11, 637090.03, 637052.15, 635623.06, 635619.13, 637013.1,
              636979.71, 637259.33, 638482.01, 638443.02, 638404.08, 638367.11,
              638355.37),
  filename = paste0("abc", 1:62)
)

geom <- lapply(1:nrow(data), function(i)
{
  mtx <- matrix(c(data$Min.X[i], data$Max.X[i], data$Min.Y[i], data$Max.Y[i])[c(1, 1, 2, 2, 1, 3, 4, 4, 3, 3)], ncol = 2)
  sf::st_polygon(list(mtx))
})

geom <-sf::st_sfc(geom)
sf::st_crs(geom) <- 26917
data <- sf::st_set_geometry(data, geom)

ctg       <- new("LAScatalog")
ctg@data  <- data

test_that("engine_index finds files and makes correct chunks", {

  bboxes <- list(
    sf::st_bbox(c(xmin = 890000, xmax = 890800, ymin = 630000, ymax = 630800)),
    sf::st_bbox(c(xmin = 890000 - 400, xmax = 890800 - 400, ymin = 630000 + 400, ymax = 630800 + 400))
  )

  clusters <- lidR:::engine_index(ctg, bboxes)

  expect_is(clusters, "list")
  expect_equal(length(clusters), 2L)
  expect_equal(clusters[[1]]@files, c("abc12", "abc15", "abc18",  "abc21"))
  expect_equal(clusters[[1]]@filter, "-inside 890000 630000 890800 630800 ")
  expect_equal(clusters[[2]]@files, c("abc18"))
  expect_equal(clusters[[2]]@filter, "-inside 889600 630400 890400 631200 ")

  clusters <- lidR:::engine_index(ctg, bboxes, lidR:::LIDRCIRCLE)

  expect_is(clusters, "list")
  expect_equal(length(clusters), 2L)
  expect_equal(clusters[[1]]@files, c("abc12", "abc15", "abc18",  "abc21"))
  expect_equal(clusters[[1]]@filter, "-inside_circle 890400 630400 400 ")
  expect_equal(clusters[[2]]@files, c("abc18"))
  expect_equal(clusters[[2]]@filter, "-inside_circle 890000 630800 400 ")
})

test_that("engine_index returns NULL if there is no match", {

  bboxes <- list(
    sf::st_bbox(c(xmin = 890000, xmax = 890800, ymin = 630000, ymax = 630800)),
    sf::st_bbox(c(xmin = 890000 - 400, xmax = 890800 - 400, ymin = 630000 - 2000, ymax = 630800 - 2000))
  )

  clusters <- lidR:::engine_index(ctg, bboxes)

  expect_is(clusters, "list")
  expect_equal(clusters[[1]]@files, c("abc12", "abc15", "abc18",  "abc21"))
  expect_true(is.null(clusters[[2]]))
})

