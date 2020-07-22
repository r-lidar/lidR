
lidR <img src="https://raw.githubusercontent.com/Jean-Romain/lidR/master/man/figures/logo200x231.png" align="right"/>
======================================================================================================
![license](https://img.shields.io/badge/Licence-GPL--3-blue.svg) 
[![Travis build status](https://travis-ci.org/Jean-Romain/lidR.svg?branch=master)](https://travis-ci.com/Jean-Romain/lidR)
[![Codecov test coverage](https://codecov.io/gh/Jean-Romain/lidR/branch/master/graph/badge.svg)](https://codecov.io/gh/Jean-Romain/lidR?branch=master)

R package for Airborne LiDAR Data Manipulation and Visualization for Forestry Applications

The lidR package provides functions to read and write `.las` and `.laz` files, plot point clouds, compute metrics using an area-based approach, compute digital canopy models, thin lidar data, manage a catalog of datasets, automatically extract ground inventories, process a set of tiles using multicore processing, individual tree segmentation, classify data from geographic data, and provides other tools to manipulate LiDAR data in a research and development context.

:book: Read [the book](https://jean-romain.github.io/lidRbook/index.html) and the [Wiki pages](https://github.com/Jean-Romain/lidR/wiki) to get started with the lidR package.

To cite the package use `citation()` from within R:

```r
citation("lidR")
```     

# Key features

<img align="right" src="https://raw.githubusercontent.com/Jean-Romain/storage/master/README/point-cloud-rotating.gif">

### Read and display a las file

In R-fashion style the function `plot`, based on `rgl`, enables the user to display, rotate and zoom a point cloud. Because `rgl` has limited capabilities with respect to large datasets, we also made a package [PointCloudViewer](https://github.com/Jean-Romain/PointCloudViewer) with greater display capabilities.

```r
las <- readLAS("<file.las>")
plot(las)
```

### Compute a canopy height model

<img align="left" src="https://raw.githubusercontent.com/Jean-Romain/storage/master/README/chm-Khosravipour.png">

`lidR` has several algorithms from the literature to compute canopy height models either point-to-raster based or triangulation based. This allows testing and comparison of some methods that rely on a CHM, such as individual tree segmentation or the computation of a canopy roughness index.

```r
las <- readLAS("<file.las>")

# Khosravipour et al. pitfree algorithm
thr <- c(0,2,5,10,15)
edg <- c(0, 1.5)
chm <- grid_canopy(las, 1, pitfree(thr, edg))

plot(chm)
```

### Read and display a catalog of las files

<img align="right" src="https://raw.githubusercontent.com/Jean-Romain/storage/master/README/catalog-plot_interactive.gif">

`lidR` enables the user to manage, use and process a catalog of `las` files. The function `catalog` builds a `LAScatalog` object from a folder. The function `plot` displays this catalog on an interactive map using the `mapview` package (if installed).

```r
ctg <- readLAScatalog("<folder/>")
plot(ctg, map = TRUE)
```

From a `LAScatalog` object the user can (for example) extract some regions of interest (ROI) with `clip_roi()`. Using a catalog for the extraction of the ROI guarantees fast and memory-efficient clipping. `LAScatalog` objects allow many other manipulations that can be done with multicore processing, where possible.

### Individual tree segmentation

<img align="left" src="https://raw.githubusercontent.com/Jean-Romain/storage/master/README/its-rotating-tree-segmented.gif" margin-right="5px">

The `segment_trees()` function has several algorithms from the literature for individual tree segmentation, based either on the digital canopy model or on the point-cloud. Each algorithm has been coded from the source article to be as close as possible to what was written in the peer-reviewed papers. Our goal is to make published algorithms usable, testable and comparable.

```r
las <- readLAS("<file.las>")

las <- segment_trees(las, li2012())
col <- random.colors(200)
plot(las, color = "treeID", colorPalette = col)
```

### Wall-to-wall dataset processing

<img align="right" src="https://raw.githubusercontent.com/Jean-Romain/storage/master/README/catalog-processing.gif">

Most of the lidR functions can process seamlessly a set of tiles and return a continuous output. Users can create their own methods using the `LAScatalog` processing engine via the `catalog_apply()` function. Among other features the engine takes advantage of point indexation with lax files, takes care of processing tiles with a buffer and allows for processing big files that do not fit in memory.

```r
# Load a LAScatalog instead of a LAS file
ctg <- readLAScatalog("<path/to/folder/>")

# Process it like a LAS file
chm <- grid_canopy(ctg, 2, p2r())
col <- random.colors(50)
plot(chm, col = col)
```

### Other tools

`lidR` has many other tools and is a continuously improved package. If it does not exist in `lidR` please ask us for a new feature, and depending on the feasibility we will be glad to implement your requested feature.

# About

**lidR** is developed openly at [Laval University](https://www.ulaval.ca/en/).

* Development of the `lidR` package between 2015 and 2018 was made possible thanks to the financial support of the [AWARE project  (NSERC CRDPJ 462973-14)](http://aware.forestry.ubc.ca/); grantee [Prof Nicholas Coops](http://profiles.forestry.ubc.ca/person/nicholas-coops/).
* Development of the `lidR` package between 2018 and 2020 was made possible thanks to the financial support of the [Ministère des Forêts, de la Faune et des Parcs of Québec](https://mffp.gouv.qc.ca/).

<img src="https://raw.githubusercontent.com/Jean-Romain/storage/master/README/logos.svg" width="600" align="center">

# Install `lidR`

```r
# The latest released version from CRAN with
install.packages("lidR")

# The latest stable development version from github with
remotes::install_github("Jean-Romain/lidR")
```

To install the package from github make sure you have a working development environment.

* **Windows**: Install [Rtools.exe](https://cran.r-project.org/bin/windows/Rtools/).  
* **Mac**: Install `Xcode` from the Mac App Store.
* **Linux**: Install the following libraries:

```
# Ubuntu
sudo add-apt-repository ppa:ubuntugis/ubuntugis-unstable
sudo apt-get update
sudo apt-get install libgdal-dev libgeos++-dev libudunits2-dev libproj-dev libx11-dev libgl1-mesa-dev libglu1-mesa-dev libfreetype6-dev libv8-3.14-dev libxt-dev

# Fedora
sudo dnf install gdal-devel geos-devel udunits2-devel proj-devel mesa-libGL-devel mesa-libGLU-devel freetype-devel libjpeg-turbo-devel v8-devel
```

# Changelog

[See changelogs on NEW.md](https://github.com/Jean-Romain/lidR/blob/master/NEWS.md)
  
