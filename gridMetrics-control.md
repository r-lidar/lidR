---
layout: default
title: Advanced used of gridMetrics
---

gridMetrics returns a list of plots with the associated metrics. But some plots fall in water and the algorithm cannot guess that. Some plots are incomplete because they fall at the edge of the file or the edge of a flightline, or in this edge of a void area (providers removed some data), or for other good reasons. The algorithm makes cells but it does not control what was in the cells. You can control the quality of cells in two ways.

The user must be able to filter the bad cells. Several possibilities are available.

## Using a basic controller: area

We can add control metrics in the metric function. For example, compute the real spatial extent area of the plot

    myMetrics = function(x, y, z, i, angle, pulseID)
    {
    	xmax = max(x)
    	ymax = max(y)
    	xmin = min(x)
    	ymin = min(y)
    
      ret = list(
          A		    = (xmax - xmin)*(ymax - ymin),
          density = length(unique(pulseID))/400,
          hmean   = mean(z),
          hmax    = max(z),
          imean   = mean(i),
          angle   = mean(abs(angle))
      )
    }
  
    metrics = gridMetrics(lidar, 10, myMetrics(X, Y, Z, Intensity, ScanAngle, pulseID))

You can see that some plots have an area which is not 100 m^2 (10 x 10). That means that these plots are probably on the edge of the file or on the edge of the lake. Verification:

    plot(metrics, "A")
    
![](images/gridMetrics-A.jpg)

So the metrics must be cleaned
    
    metricsCleaned = dplyr::filter(metrics, A > 100*0.90)
    
The real area cannot be exactly 100 m^2 because the returns are discrete. Here we chose 90% of 100 m^2.

## Advanced controller: filter from shapefiles

Area control is not enough. We can classify the data based on geographic information found in shapefiles.

    shapefile_dir <- system.file("extdata", package = "lidR")
    lake = rgdal::readOGR(shapefile_dir, "lac_ontario_UTM17")

We can check if the LiDAR points are in polygons given in the shapefile (here polygons are lakes) with the function `classifyFromShapefile`. This function is documented [here](classifyFromShapefile.html).

    lidar = classifyFromShapefile(lidar, lake, field="lake")

There is a now a new column named `lake`. Each point is classified. `TRUE`: the point is in a lake, `FALSE`: the point is not in a lake. You can filter the points with `extract`.

    forest = lidar %>% leach(lake == FALSE)
    forest %>% plot
    
You can also compute the number of points classified as lake in the `gridMetrics` function:

    myMetrics = function(x, y, z, i, angle, pulseID, lake)
    {
    	xmax = max(x)
    	ymax = max(y)
    	xmin = min(x)
    	ymin = min(y)
    
      ret = list(
          A		    = (xmax - xmin)*(ymax - ymin),
          nlake   = sum(lake),
          density = length(unique(pulseID))/400,
          hmean   = mean(z),
          hmax    = max(z),
          imean   = mean(i),
          angle   = mean(abs(angle))
      )
    }
  
    metrics = gridMetrics(lidar, 10, myMetrics(X, Y, Z, Intensity, ScanAngle, pulseID, lake))
    
    plot(metrics, "nlake")
    
![](images/gridMetrics-nlake.jpg)

And filter the resulting raster which has no points into a lake and which has a proper defined area.

    metricsCleaned = dplyr::filter(metrics, A > 380, nlake == 0)
    plot(metricsCleaned, "hmean")
    
![](images/gridMetrics-hmeanclean.jpg)