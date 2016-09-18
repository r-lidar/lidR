![Version](http://img.shields.io/Version/1.2.0%20beta.png)  ![licence](https://img.shields.io/badge/Licence-GPL--3-blue.svg) ![CRAN](https://img.shields.io/badge/CRAN-not%20yet-lightgray.svg)

R package for airborne LiDAR data manipulation and visualisation for forest applications. 

lidR package provides functions to read and write `.las` and `.laz` files, plot a point cloud, compute metrics using an area-based approach, compute digital canopy models, thin lidar data, manage a catalog of dataset, automatically extract ground inventories, process a set of tiles in multicore, classify data from shapefiles and provides other tools to manipulate LiDAR data. lidR package is designed mainly for research purposes using an area-based approach.

lidR provides an open-source and R-based implementation of several classical functions used in softwares dedicated to LiDAR data manipulation. lidR is flexible because it allows the user to program their own tools and manipulate their own object in R rather rely on a set of predefined tools without any control. However it does NOT replace existing tools like FUSION or LAStools. The aims are not the same. The lidR package aims to easily manipulate data to create new thing/ideas but is does not provide memory optimized powerfull algorithms to export professtionnal product.

Please contact the author for any bug or feature request (on github is the best). I'll enjoy to implement new stuff.

1. [Features](#features)
2. [Install lidR from github](#install-lidr-from-github)
3. [Some examples](#some-examples)
4. [Changelog](#changelog)

# Features (not exhaustive)

- [Read write .las and .laz files](http://jean-romain.github.io/lidR/loadLidar.html)
- [Retrieve indiviual pulses](http://jean-romain.github.io/lidR/loadLidar.html#dynamically-computed-fields)
- [Retrieve individual flightlines](http://jean-romain.github.io/lidR/loadLidar.html#dynamically-computed-fields)
- [Compute any set of metrics on a cloud of points](http://jean-romain.github.io/lidR/gridmetrics.html#cloudmetrics)
- [Rasterize and to compute a set of metrics using an area based approach](http://jean-romain.github.io/lidR/gridmetrics.html)
- [Classify and clip data from geographic shapefiles](http://jean-romain.github.io/lidR/classify_from_shapefile.html)
- [Thin a cloud of points to reach an homogeneous pulse density](http://jean-romain.github.io/lidR/thin.html)
- [Filter cloud of points based on any condition test](http://jean-romain.github.io/lidR/lasfilter.html)
- [Clip data bases on discs, rectangles or polygons](http://jean-romain.github.io/lidR/clip.html)
- [Manage a catalog of `.las` tiles](http://jean-romain.github.io/lidR/catalog.html)
- [Extract automatically a set of ground plot inventories (even plots falling between two or more tiles)](http://jean-romain.github.io/lidR/catalog.html#extract-a-ground-inventory)
- [Analyse a full set of tiles in parallel computing](http://jean-romain.github.io/lidR/catalog.html#process-all-the-file-of-a-catalog)
- Compute a digital terrain model (DTM).
- Normalize a point cloud substracting a DTM (computed or read from a file).
- [Compute a digital canopy model](http://jean-romain.github.io/lidR/canopy.html)
- [Plot 3D LiDAR data](http://jean-romain.github.io/lidR/plotLidar.html)
- [plot metrics in 2D and 3D](http://jean-romain.github.io/lidR/gridmetrics.html)

# Install lidR from github

Since version 1.1.0 the package contains C++ code. The process to install the package from github for Windows users is more complex than before as you need developpement tools to be able to compile C++ code. Windows users can download and install a [binary version of the package](https://github.com/Jean-Romain/lidR/tree/gh-pages/win-bin/) already compiled to skip this step (but not necesseraly up-to-date).
    
## Install development tools

### Linux users

Install R development package `sudo apt-get install r-base-dev`

### Windows users

Install [Rtools](https://cran.r-project.org/bin/windows/Rtools/)

### Mac users

Install the [Xcode command line tools](https://developer.apple.com/downloads)

## Install from github with devtools

````r
devtools::install_github("Jean-Romain/lidR", dependencies=TRUE)
````
    
# Some examples
     
## Plot data

````r
lidar = readLAS("myfile.las")
plot(lidar)
````

![](https://github.com/Jean-Romain/lidR/blob/gh-pages/images/plot3d_1.jpg)

## Compute a simple metric

````r
metric = grid_metrics(lidar, 20, mean(Z))
plot(metric)
````

![](https://github.com/Jean-Romain/lidR/blob/gh-pages/images/gridMetrics-mean.jpg)

# Changelog

## Changelog v1.2.0

**Note**: This version comes with a lot of incompatibilities with the previous versions. I tried to harmonize naming before a first submission to the CRAN.

- Fix: `readLAS` returns the NumberOfReturns (forgot since v1.1.0)
- Fix: Correct computation of color when reading RBG. R G and B are coded on 16 bits. (issue [#13](https://github.com/Jean-Romain/lidR/issues/13)).
- Fix: Propage and update header when manipulation of lidar data (issue [#12](https://github.com/Jean-Romain/lidR/issues/12)).
- Remove: `getData()`. Useless since acessor `$`. Use `object$data`.
- Add: an overloaded operator minus `-` very convenient to normalize a dataset `lidar - dtm` is synonyme to `normalize(lidar, dtm)`
- Add: `normalize()` enable to substract a digital terrain model to the LiDAR data.
- Add: `grid_terrain()` enable to compute digital terrain model.
- Add: `LAS` object have now an accessor `$` enabling to access to slots or columns in `@data` or fields in `@header`.
- Change: `roi_query()` can extract data from rectangular queries (issue [#11](https://github.com/Jean-Romain/lidR/issues/11)).
- Change: `process_parallel()` works both on Unix (GNU/linux and Mac) and Windows platform (issue [#10](https://github.com/Jean-Romain/lidR/issues/10)).
- Change: `roi_index()` can create index from rectangular queries (issue [#11](https://github.com/Jean-Romain/lidR/issues/11)).
- Rename: `convexHull()`to `convex_hull()` to harmonize function names style.
- Rename: `pointInPolygon()`to `point_in_polygon()` to harmonize function names style.
- Rename: `selectTiles()`to `tiles_select()` to harmonize function names style.
- Rename: `fractal.dimension()`to `fractal_dimension()` to harmonize function names style.
- Rename: `extract()`to `lasfilter()` to not override `extract()` from `magrittr` or `rgdal`. `filter()` is already used by `dplyr`.
- Rename: `canopyClosure()` to `canopy_closure()` to harmonize function names style.
- Rename: `canopyMatrix()` to `local_maximum()` for a better meaning.
- Rename: `canopyModel()`to `grid_canopy()` to harmonize function names style.
- Rename: `pulseDensity()`to `grid_density()` to harmonize function names style.
- Rename: `gridMetrics()`to `grid_metrics()` to harmonize function names style.
- Rename: `cloudMetrics()`to `cloud_metrics()` to harmonize function names style.
- Rename: `selectArea()`to `roi_select()` to harmonize function names style.
- Rename: `extractGroundInventory()` to `roi_query()`for a wider meaning.
- Rename: `retrieveInventoryTile()` to `roi_index()` for a wider meaning.
- Rename: `gapFractionProfile()` to `gap_fraction_profile()` to harmonize function names style.
- Rename: `classifyFromShapefile()` to `classify_from_shapefile()` to harmonize function names style.
- Rename: `processParallel()` to `process_parallel()` to harmonize function names style.

### Changelog v1.1.2

- Change: remove dependence to `reshape2` (issue [#8](https://github.com/Jean-Romain/lidR/issues/8))
- Add: `.onAttach()` function to provide github link when package is loaded
- Fix: In `gapFractionProfile()` enable more flexible binning (issue [#7](https://github.com/Jean-Romain/lidR/issues/7))
- Fix: In `readLAS()` accept uppercase files extensions LAS and LAZ (not LaS or lAs; pure uppercase is bad enought)
- Fix: In `Catalog()` read only las, laz, LAS and LAZ files extensions (updated regex) (issue [#6](https://github.com/Jean-Romain/lidR/issues/6))
- Fix: bug when reading to many files because of non closed connections (issue [#5](https://github.com/Jean-Romain/lidR/issues/5))

### Changelog v1.1.1

- Fix: `readLAS()` can read file from a `Catalog` object again.
- Fix: bug when building a `Catalog` from a folder containing not only las or laz files. Add a regular expression.
- Fix: bug when building a `Catalog` since version 1.1.0. Works again.

## Changelog v1.1.0

- Update: function `classifyFromShapefile()` is, at least, 3 times faster. Parts of the function have been rewritten in C++. The new column is added by reference
- Add: include the [Martin Isenburg](https://rapidlasso.com/) source code of `LASlib` and `LASzip`.
- Update: function `readLAS()` have been rewritten in C++ using `LASlib`. It is 2 times faster and it's safer.
- Add: function `writeLAS()` using `LASlib`.
- Add: support of compressed `.laz` format in `readLAS` and `writeLAS()` thanks to `LASlib` and `LASzip`.
- Update: function `readLAS()` replace `loadLidar()`. `loadLidar()` does not exist anymore.
- Update: objects `Lidar` do not exist anymore. They are called `LAS`. It does not change anything for users.
