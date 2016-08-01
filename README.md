R package for airborne LiDAR data manipulation and visualisation for forestry applications

lidR package provides functions to read `.las` files, plot a cloud of points, compute metrics using an area-based approach, compute digital canopy models, thin lidar data, automatically extract ground inventories, process a set of tiles in multicore, classify data from shapefiles and provides other tools to manipulate liDAR data. LidR is designed mainly for research purposes using an area-based approach.

lidR provides an open-source and R-based implementation of the main functions from software like FUSION or lastools. lidR is flexible because it allows the user to program their own tools rather rely on a set of predefined tools.

# Install lidR

## Install lidR - Linux

Install dependencies

    sudo apt-get install liblas-dev libboost-dev

Install from github with devtools

    install.package("devtools")
    devtools::install_github("Jean-Romain/lidR")
    library(lidR)

## Install lidR - Windows

Since v1.1.0 it not so easy to install it on Windows because lidR depend on external library such as `liblas`. Windows does not provide fair tools easily develop computer tools.
    
# Features 

- [Read .las files](http://jean-romain.github.io/lidR/loadLidar.html)
- [Retrieve indiviual pulses](http://jean-romain.github.io/lidR/loadLidar.html#dynamically-computed-fields)
- [Retrieve individual flightlines](http://jean-romain.github.io/lidR/loadLidar.html#dynamically-computed-fields)
- [Compute a digital canopy model](http://jean-romain.github.io/lidR/canopy.html)
- [Compute any set of metrics on a cloud of points](http://jean-romain.github.io/lidR/gridMetrics.html#cloudmetrics)
- [Rasterize and apply any function to compute a set of metrics using an area based approach](http://jean-romain.github.io/lidR/gridMetrics.html)
- [Classify and filter data from geographic shapefiles](http://jean-romain.github.io/lidR/classifyFromShapefile.html)
- [Filter cloud of points based on any condition test](http://jean-romain.github.io/lidR/extract.html)
- [Thin a cloud of points to reach an homogeneous point density](http://jean-romain.github.io/lidR/thin.html)
- [Clip data bases on discs, rectangles or polygons](http://jean-romain.github.io/lidR/clip.html)
- [Manage a catalog of `.las` tiles](http://jean-romain.github.io/lidR/catalog.html)
- [Extract automatically a set of ground plot inventories (even plots falling between two or more tiles)](http://jean-romain.github.io/lidR/catalog.html#extract-a-ground-inventory)
- [Analyse a full set of tiles in parallel computing](http://jean-romain.github.io/lidR/catalog.html)
- [Plot 3D LiDAR data](http://jean-romain.github.io/lidR/plotLidar.html)
- [plot metrics in 2D and 3D](http://jean-romain.github.io/lidR/gridMetrics.html)
    
# Some examples
     
## Plot data

	lidar = LoadLidar("myfile.las")
	plot(lidar)

![](https://github.com/Jean-Romain/lidR/blob/gh-pages/images/plot3d_1.jpg)

## Compute a simple metric

    metric = gridMetrics(lidar, 20, mean(Z))
    plot(metric)

![](https://github.com/Jean-Romain/lidR/blob/gh-pages/images/gridMetrics-mean.jpg)

## Changelog v1.1.0

- `classifyFromShapefile` is, at least, 3 times faster. Parts of the function have been rewritten in C++. The new column is added by reference
- Add functions `DEM` and `DTM` : note that they are experimental functions ! They cannot be used for normalization purpose
- `readLAs` have been rewritten in C++ using `liblas`. It is 2 times faster and it's safer.
- Add function `writeLAS` using `liblas`.