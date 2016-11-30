![Version](http://img.shields.io/Version/1.0.0%20beta.png)  ![licence](https://img.shields.io/badge/Licence-GPL--3-blue.svg) ![CRAN](https://img.shields.io/badge/CRAN-not%20yet-lightgray.svg)

<div style="background:#ffc3c3;padding:8px;border:solid red;margin:10px;border-radius:10px">**Advertissement for old users: **: I will sumbmit the package to the CRAN soon. The previous version was v1.2.0. The current version is v1.0.0. to be logical and get the foundation right. This version is entirely non compatible with the old v1.2.0 because all the function names changed to get a better coherance. All my privious work was try and error. This version is fixed in the stone.
</div>

R package for airborne LiDAR data manipulation and visualisation for forest applications. 

lidR package provides functions to read and write `.las` and `.laz` files, plot a point cloud, compute metrics using an area-based approach, compute digital canopy models, thin lidar data, manage a catalog of dataset, automatically extract ground inventories, process a set of tiles in multicore, classify data from shapefiles and provides other tools to manipulate LiDAR data. lidR package is designed mainly for research purposes using an area-based approach.

lidR provides an open-source and R-based implementation of several classical functions used in softwares dedicated to LiDAR data manipulation. lidR is flexible because it allows the user to program their own tools and manipulate their own object in R rather rely on a set of predefined tools.

Please contact the author for any bug or feature request (on github is the best). I'll enjoy to implement new stuffs.

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

The package contains C++ code. To install the package from github you need developpement tools to be able to compile C++ code.

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
