---
layout: default
title: lidR package
---

R package for airborne LiDAR data manipulation and visualisation for forestry applications

lidR package provides functions to read `.las` files, plot a cloud of points, compute metrics in an area-based approach, compute a digital canopy model, thin liDAR data, automatically extract ground inventories, process a set of tiles in multicore, classify data from shapefiles and provides other tools to manipulate liDAR data. LidR is designed mainly for research purposes using an area-based approach.

lidR provides an open-source and R-based implementation of the main functions from sofware like FUSION or lastools. lidR is flexible because it allows users to program their own tools rather than using a set of predetermined tools.

# Install lidR from Github with devtools

    install.package("devtools")
    devtools::install_github("Jean-Romain/lidR")
    library(lidR)
    
# Features 

- [Read .las files](loadLidar.html)
- [Retrieve indiviual pulses](loadLidar.html#dynamically-computed-field)
- [Retrieve individual flightlines](loadLidar.html#dynamically-computed-fields)
- [Compute a digital canopy model](canopy.html)
- Compute any set of metrics on a cloud of points
- Rasterize and apply any function to compute a set of metrics using an area-based approach
- [Classify and filter data from geographic shapefiles](classifyFromShapefile.html)
- [Filter a cloud of points based on any condition test](extract.html)
- Thin a cloud of points to reach an homogeneous point density
- Clip data based on discs, rectangles or polygons.
- Compute point and pulse densities
- Manage a catalog of `.las` tiles
- Extract automatically a set of ground plot inventories (even plots falling between two or more tiles)
- Analyse a full set of tiles in parallel computing
- [Plot 3D LiDAR data](plotLidar.html)
- plot metrics in 2D and 3D
- Compute a simple triangular irregular network (TIN)
    
# Some examples
    
    
## Plot data

	lidar = LoadLidar("myfile.las")
	plot(lidar)

![](images/plot3d_1.jpg)

## Compute a simple metric

    metric = gridMetrics(lidar, 20, mean(Z))
    plot(metric)

![](images/gridMetrics-mean.jpg)
    
## Compute a set of user-defined metrics

    myMetrics = function(z, i, angle, pulseID)
    {
      ret = list(
            npulse  = length(unique(pulseID)),
            hmean   = mean(z),
            hmax    = max(z),
            imean   = mean(i),
            angle   = mean(abs(angle))
            )

       return(ret)
    }
    
    metrics = gridMetrics(lidar, 20, myMetrics(Z, Intensity, ScanAngle, pulseID))

    plot(metrics, "hmean")
    plot(metrics, "hmax")
    plot(metrics, "imean")
    #etc.

