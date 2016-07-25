R package for airborne LiDAR data manipulation and visualisation for forestry applications

lidR package provides functions to read `.las` files, plot a cloud of points, compute metrics using an area-based approach, compute digital canopy models, thin lidar data, automatically extract ground inventories, process a set of tiles in multicore, classify data from shapefiles and provides other tools to manipulate liDAR data. LidR is designed mainly for research purposes using an area-based approach.

lidR provides an open-source and R-based implementation of the main functions from software like FUSION or lastools. lidR is flexible because it allows the user to program their own tools rather rely on a set of predefined tools.

# Install lidR from Github with devtools

    install.package("devtools")
    devtools::install_github("Jean-Romain/lidR")
    library(lidR)
    
# Features 

- [Read .las files](http://jean-romain.github.io/lidR/loadLidar.html)
- [Retrieve indiviual pulses](http://jean-romain.github.io/lidR/loadLidar.html#dynamically-computed-fields)
- [Retrieve individual flightlines](http://jean-romain.github.io/lidR/loadLidar.html#dynamically-computed-fields)
- [Compute a digital canopy model](http://jean-romain.github.io/lidR/canopy.html)
- [Compute any set of metrics on a cloud of points](http://jean-romain.github.io/lidR/gridMetrics.html#cloudmetrics)
- [Rasterize and apply any function to compute a set of metrics using an area based approach](http://jean-romain.github.io/lidR/gridMetrics.html)
- [Classify and filter data from geographic shapefiles](http://jean-romain.github.io/lidR/classifyFromShapefile.html)
- [Filter cloud of points based on any condition test](http://jean-romain.github.io/lidR/extract.html)
- Thin a cloud of points to reach an homogeneous point density
- [Clip data bases on discs, rectangles or polygons](http://jean-romain.github.io/lidR/clip.html)
- Compute point and pulse densities
- Manage a catalog of `.las` tiles
- Extract automatically a set of ground plot inventories (even plots falling between two or more tiles)
- Analyse a full set a tiles in parallel computing
- [Plot 3D LiDAR data](http://jean-romain.github.io/lidR/plotLidar.html)
- plot the metrics in 2D and 3D
    
# Some examples
     
## Plot data

	lidar = LoadLidar("myfile.las")
	plot(lidar)

![](https://github.com/Jean-Romain/lidR/blob/gh-pages/images/plot3d_1.jpg)

## Compute a simple metric

    metric = gridMetrics(lidar, 20, mean(Z))
    plot(metric)

![](https://github.com/Jean-Romain/lidR/blob/gh-pages/images/gridMetrics-mean.jpg)
