---
layout: default
title: Classify points from shapefile
---

The function `classifyFromShapefile` enable the classification of each point from geographic data contained in a shapefile. So many applications can derive from this function. The following example shows how to remove lakes from the data using this tool.

## Load the data and read a shapefile

    LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
    shapefile_dir <- system.file("extdata", package = "lidR")
    
    lidar = LoadLidar(LASfile)
    lake = rgdal::readOGR(shapefile_dir, "lac_ontario_UTM17")
    
## Classify the points

The function checks, for each polygon contained in the shapefile, if the points are in or out of the polygons. If a point is in a polygon the function attributes to this point the value of the shapefile's field passed in the parameter `field`. If the field does not exist it attributes a boolean value to the point. In the following example the field `lake` does not exist in the shapefile. The function creates a new field in the `lidar` object named `lake` with the binary information: `TRUE` (the point is in a lake), or `FALSE` (the point is not in a lake).

    lidar %<>% classifyFromShapefile(lake, field="lake")
    
## Filter the lakes to retain only forest

    forest = lidar %>% extract(lake == FALSE)
    
![](images/plot3d_1.jpg)

![](images/plot3d_-lake.jpg)
