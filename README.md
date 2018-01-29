![](https://raw.githubusercontent.com/Jean-Romain/lidR/gh-pages/images/lidr-ban.png)<br/>

![CRAN](https://img.shields.io/badge/CRAN-1.4.1-brightgreen.svg)  ![Github](https://img.shields.io/badge/Github-1.4.1-green.svg) ![licence](https://img.shields.io/badge/Licence-GPL--3-blue.svg) 

R package for Airborne LiDAR Data Manipulation and Visualization for Forestry Applications

The lidR package provides functions to read and write `.las` and `.laz` files, plot a point cloud, compute metrics using an area-based approach, compute digital canopy models, thin lidar data, manage a catalog of datasets, automatically extract ground inventories, process a set of tiles in multicore, individual tree segmentation, classify data from geographic data, and provides other tools to manipulate LiDAR data.

Development of the lidR package between 2015 and 2017 was made possible thanks to the financial support of the [AWARE project  (NSERC CRDPJ 462973-14)](http://aware.forestry.ubc.ca/); grantee [Prof Nicholas Coops](http://profiles.forestry.ubc.ca/person/nicholas-coops/).

Please contact the author for bug reports or feature requests (on github, preferably). I enjoy implementing new features!

1. [Features](#features-not-exhaustive)
2. [Install lidR](#install-lidr)
3. [Some examples](#some-examples)
4. [Changelog](#changelog)

# Features (not exhaustive)

- [Read write .las and .laz files](https://github.com/Jean-Romain/lidR/wiki/readLAS)
- [Plot 3D LiDAR data](https://github.com/Jean-Romain/lidR/wiki/lasplot)
- [Retrieve indiviual pulses and flightlines](https://github.com/Jean-Romain/lidR/wiki/readLAS#dynamically-computed-fields)
- [Compute any set of metrics using an area based approach](https://github.com/Jean-Romain/lidR/wiki/grid_metrics)
- [Compute any set of metrics on a cloud of points](https://github.com/Jean-Romain/lidR/wiki/cloud_metrics)
- [Classify and clip data from geographic shapefiles](https://github.com/Jean-Romain/lidR/wiki/lasclassify)
- [Colorize a point cloud from RGB images](https://github.com/Jean-Romain/lidR/wiki/lasclassify)
- [Filter a cloud of points based on any condition test](https://github.com/Jean-Romain/lidR/wiki/lasfilter)
- [Clip data based on discs, rectangles or polygons](https://github.com/Jean-Romain/lidR/wiki/lasclip)
- [Manage a catalog of `.las` tiles](https://github.com/Jean-Romain/lidR/wiki/catalog)
- [Thin a point cloud to reach a homogeneous pulse density](https://github.com/Jean-Romain/lidR/wiki/lasdecimate)
- [Automatically extract a set of ground plot inventories](https://github.com/Jean-Romain/lidR/wiki/catalog_queries)
- [Analyse a full set of tiles in parallel computing](https://github.com/Jean-Romain/lidR/wiki/catalog_apply)
- [Compute a digital canopy model (DCM)](https://github.com/Jean-Romain/lidR/wiki/grid_canopy)
- [Compute a digital terrain model (DTM)](https://github.com/Jean-Romain/lidR/wiki/grid_terrain)
- [Normalize a point cloud substracting a DTM](https://github.com/Jean-Romain/lidR/wiki/lasnormalize)
- [Individual tree segmentation](https://github.com/Jean-Romain/lidR/wiki/Tree-segmentation-from-A-to-Z)

# Install `lidR`

* The latest released version from CRAN with

```r
install.packages("lidR")
```

* The latest development version from github with

```r
devtools::install_github("Jean-Romain/rlas", dependencies=TRUE)
devtools::install_github("Jean-Romain/lidR", dependencies=TRUE)
```

To install the package from github make sure you have a working development environment.

* **Windows**: Install [Rtools.exe](https://cran.r-project.org/bin/windows/Rtools/).  
* **Mac**: Install `Xcode` from the Mac App Store.
* **Linux**: Install the R development package, usually called `r-devel` or `r-base-dev`
    
# Some examples

![](https://raw.githubusercontent.com/Jean-Romain/lidR/gh-pages/images/examplereadme.png)

# Changelog

[See changelogs on NEW.md](https://github.com/Jean-Romain/lidR/blob/master/NEWS.md)