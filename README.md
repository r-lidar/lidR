![Version](http://img.shields.io/Version/1.0.0%20beta.png)  ![licence](https://img.shields.io/badge/Licence-GPL--3-blue.svg) ![CRAN](https://img.shields.io/badge/CRAN-not%20yet-lightgray.svg) [![Travis-CI Build Status](https://travis-ci.org/Jean-Romain/lidR.svg?branch=devel)](https://travis-ci.org/Jean-Romain/lidR)

**Announcement for old users:** *I will submit the package to CRAN soon. The previous version was v1.2.0. The current version is v1.0.0. to be logical and get the foundation right. This version is entirely non-compatible with the old v1.2.0 because all the function names have changed for improved coherence. All the previous work on this package was trial and error. This version is set in stone.*

R package for Airborne LiDAR Data Manipulation and Visualization for Forestry Applications

The lidR package provides functions to read and write `.las` and `.laz` files, plot a point cloud, compute metrics using an area-based approach, compute digital canopy models, thin lidar data, manage a catalog of datasets, automatically extract ground inventories, process a set of tiles in multicore, classify data from shapefiles, and provides other tools to manipulate LiDAR data. The lidR package is designed mainly for research purposes using an area-based approach.

lidR provides an open-source and R-based implementation of several classical functions used in software dedicated to LiDAR data manipulation. lidR is flexible because it allows the user to program their own tools and manipulate their own objects in R rather than rely on a set of predefined tools.

Please contact the author for bug reports or feature requests (on github, preferably). I enjoy implementing new features!

1. [Features](#features)
2. [Install lidR from github](#install-lidr-from-github)
3. [Some examples](#some-examples)
4. [Changelog](#changelog)

# Features (not exhaustive)

- [Read write .las and .laz files](https://github.com/Jean-Romain/lidR/wiki/readLAS)
- [Retrieve indiviual pulses and flightlines](https://github.com/Jean-Romain/lidR/wiki/readLAS#dynamically-computed-fields)
- [Compute any set of metrics on a cloud of points](https://github.com/Jean-Romain/lidR/wiki/cloud_metrics)
- [Rasterize and to compute a set of metrics using an area based approach](https://github.com/Jean-Romain/lidR/wiki/grid_metrics)
- [Classify and clip data from geographic shapefiles](https://github.com/Jean-Romain/lidR/wiki/lasclassify)
- [Thin a cloud of points to reach a homogeneous pulse density](https://github.com/Jean-Romain/lidR/wiki/lasdecimate)
- [Filter a cloud of points based on any condition test](https://github.com/Jean-Romain/lidR/wiki/lasfilter)
- [Clip data based on discs, rectangles or polygons](https://github.com/Jean-Romain/lidR/wiki/lasclip)
- [Manage a catalog of `.las` tiles](https://github.com/Jean-Romain/lidR/wiki/catalog)
- [Automatically extract a set of ground plot inventories (even plots falling between two or more tiles)](https://github.com/Jean-Romain/lidR/wiki/catalog_queries)
- [Analyse a full set of tiles in parallel computing](https://github.com/Jean-Romain/lidR/wiki/catalog_#process-all-the-file-of-a-catalog_apply)
- [Compute a digital terrain model (DTM).](https://github.com/Jean-Romain/lidR/wiki/grid_terrain)
- [Normalize a point cloud substracting a DTM (computed or read from a file).](https://github.com/Jean-Romain/lidR/wiki/lasnormalize)
- [Compute a digital canopy model](https://github.com/Jean-Romain/lidR/wiki/grid_canopy)
- [Plot 3D LiDAR data](https://github.com/Jean-Romain/lidR/wiki/lasplot)

# Install lidR from github

The package contains C++ code. To install the package from github you need development tools to be able to compile C++ code.

Install R development tools:

| OS      | Install
|---------|-------------------------------------------------------------------|
| Linux   | `sudo apt-get install r-base-dev`                                 |
| Windows | [Rtools.exe](https://cran.r-project.org/bin/windows/Rtools/)      |
| Mac     | [Xcode command line tools](https://developer.apple.com/downloads) |

And install devtools: `install.packages("devtools")`.

## Install from github with devtools

````r
devtools::install_github("Jean-Romain/lidR", dependencies=TRUE)
````
    
# Some examples

<table>
  <tr>
    <th>Plot data</th>
    <th>Compute a simple metric</th>
  </tr>
  <tr>
    <td valign="top">
<pre>lidar = readLAS("myfile.las")
plot(lidar)</pre>
<img src="https://raw.githubusercontent.com/Jean-Romain/lidR/gh-pages/images/plot3d_1.jpg" alt="" style="max-width:100%;">
    </td>
    <td valign="top">
<pre>metric = grid_metrics(lidar, 20, mean(Z))
plot(metric)</pre>
<img src="https://raw.githubusercontent.com/Jean-Romain/lidR/gh-pages/images/gridMetrics-mean.jpg" alt="" style="max-width:100%;">
    </td>
  </tr>
    <tr>
    <th>Manage a catalog</th>
    <th>Deal with DTM</th>
  </tr>
  <tr>
    <td valign="top">
<pre>catalog = Catalog("folder of .las files")
plot(catalog)</pre>
<img src="https://raw.githubusercontent.com/Jean-Romain/lidR/gh-pages/images/catalog.png" alt="" style="max-width:100%;">
    </td>
    <td valign="top">
<pre>dtm = grid_terrain(lidar)
plot(dtm)</pre>
<img src="https://raw.githubusercontent.com/Jean-Romain/lidR/gh-pages/images/dtm.jpg" alt="" style="max-width:100%;">
    </td>
  </tr>
</table>

# Changelog

no changelog
