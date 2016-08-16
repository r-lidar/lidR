R package `v 1.1.1` for airborne LiDAR data manipulation and visualisation for forestry applications. 

lidR package provides functions to read and write `.las` and `.laz` files, plot a cloud of points, compute metrics using an area-based approach, compute digital canopy models, thin lidar data, automatically extract ground inventories, process a set of tiles in multicore, classify data from shapefiles and provides other tools to manipulate liDAR data. lidR package is designed mainly for research purposes using an area-based approach.

lidR provides an open-source and R-based implementation of several classical functions used in softwares dedicated to LiDAR data manipulation. lidR is flexible because it allows the user to program their own tools in R rather rely on a set of predefined tools.

1. [Feature](#features)
2. [Install lidR from github](#install-lidr-from-github)
3. [Some examples](#some-examples)
4. [Changelog](#changelog)

# Features 

- [Read .las and .laz files](http://jean-romain.github.io/lidR/loadLidar.html)
- Write .las and .laz files
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

# Install lidR from github

Since version 1.1.0 the package contains C++ code. The process to install the package from github for Windows users is more complex than before as you need developpement tools to be able to compile C++ code. Windows users can download and install a [binary version of the package](https://github.com/Jean-Romain/lidR/tree/gh-pages/win-bin/) (not necesseraly up-to date).
    
## Install development tools

### Linux users

Install R development package `sudo apt-get install r-base-dev`

### Windows users

Install Rtools: https://cran.r-project.org/bin/windows/Rtools/

### Mac users

I can't help you. Reading documentation seems prohibited for non mac user. Read this page: https://www.rstudio.com/products/rpackages/devtools/

## Install dependencies

    installed.packages(c("methods","magrittr","dtplyr","rgl","reshape2","tools","parallel","fields","raster","rgdal","plyr","rgeos","data.table","dplyr","sp","Rcpp"))

## Install from github with devtools

    install.packages("devtools")
    devtools::install_github("Jean-Romain/lidR")
    library(lidR)

**Note for Windows users** : tested on Windows 7. Installation might work as well as for GNU/Linux. But maybe not... Windows behaviours are... unpredictable.
    
# Some examples
     
## Plot data

	lidar = LoadLidar("myfile.las")
	plot(lidar)

![](https://github.com/Jean-Romain/lidR/blob/gh-pages/images/plot3d_1.jpg)

## Compute a simple metric

    metric = gridMetrics(lidar, 20, mean(Z))
    plot(metric)

![](https://github.com/Jean-Romain/lidR/blob/gh-pages/images/gridMetrics-mean.jpg)

# Changelog

## Changelog v1.1.1

- Fix: readLAS can read file from a `Catalog` object again.
- Fix: bug when building a `Catalog` from a folder containing not only las or laz files. Add a regular expression.
- Fix: bug when building a `Catalog` since version 1.1.0. Works angain.

## Changelog v1.1.0

- Update: function `classifyFromShapefile` is, at least, 3 times faster. Parts of the function have been rewritten in C++. The new column is added by reference
- Add: include the [https://rapidlasso.com/](Martin Isenburg) source code of `LASlib` and `LASzip`.
- Update: function `readLAs` have been rewritten in C++ using `LASlib`. It is 2 times faster and it's safer.
- Add: function `writeLAS` using `LASlib`.
- Add: support of compressed `.laz` format in `readLAS` and `writeLAS` thanks to `LASlib` and `LASzip`.
- Update: function `readLAS` replace `loadLidar`. `loadLidar` does not exist anymore.
- Update: objects `Lidar` do not exist anymore. They are called `LAS`. It does not change anything for users.
