![](https://raw.githubusercontent.com/Jean-Romain/lidR/master/readme.img/lidr-ban.png)<br/>

![CRAN](https://img.shields.io/badge/CRAN-1.4.1-brightgreen.svg)  ![Github](https://img.shields.io/badge/Github-1.5.0-green.svg) ![licence](https://img.shields.io/badge/Licence-GPL--3-blue.svg) 

R package for Airborne LiDAR Data Manipulation and Visualization for Forestry Applications

The lidR package provides functions to read and write `.las` and `.laz` files, plot a point cloud, compute metrics using an area-based approach, compute digital canopy models, thin lidar data, manage a catalog of datasets, automatically extract ground inventories, process a set of tiles in multicore, individual tree segmentation, classify data from geographic data, and provides other tools to manipulate LiDAR data in a research and development context.

Development of the lidR package between 2015 and 2018 was made possible thanks to the financial support of the [AWARE project  (NSERC CRDPJ 462973-14)](http://aware.forestry.ubc.ca/); grantee [Prof Nicholas Coops](http://profiles.forestry.ubc.ca/person/nicholas-coops/).

# Content

1. [Key features](#key-features)
2. [Some examples](#some-examples)
3. [Install lidR](#install-lidr)
4. [Changelog](#changelog)

# Key features

- [Read and write .las and .laz files](https://github.com/Jean-Romain/lidR/wiki/readLAS)
- [Plot 3D LiDAR data](https://github.com/Jean-Romain/lidR/wiki/lasplot)
- [Area based approach using any set of metrics](https://github.com/Jean-Romain/lidR/wiki/grid_metrics)
- [Individual tree segmentation](https://github.com/Jean-Romain/lidR/wiki/Tree-segmentation-from-A-to-Z)
- [Classify and clip data from geographic shapefiles](https://github.com/Jean-Romain/lidR/wiki/lasclassify)
- [Manage a catalog of tiles](https://github.com/Jean-Romain/lidR/wiki/catalog)
- [Automatically extract a set of ground plot inventories](https://github.com/Jean-Romain/lidR/wiki/catalog_queries)
- [Analyse a full set of tiles in parallel computing](https://github.com/Jean-Romain/lidR/wiki/catalog_apply)
- [Compute a digital canopy model (DCM)](https://github.com/Jean-Romain/lidR/wiki/Rasterizing-perfect-canopy-height-models)
- [Compute a digital terrain model (DTM)](https://github.com/Jean-Romain/lidR/wiki/grid_terrain)
- [Normalize a point cloud substracting a DTM](https://github.com/Jean-Romain/lidR/wiki/lasnormalize)
    
# Some examples

<img align="right" src="https://raw.githubusercontent.com/Jean-Romain/lidR/master/readme.img/rotating-pointcloud.gif">

### Read and display a las file

In the R-fashion style the function `plot` based on `rgl` enable the user to display, rotate and zoom on a point cloud. Because `rgl` have limited capabilities regarding large datasets we also made a package [PointCloudViewer](https://github.com/Jean-Romain/PointCloudViewer) with greater display capabilites.

```r
las = readLAS("<file.las>")
plot(las)
```

### Compute a canopy height model

<img align="left" src="https://raw.githubusercontent.com/Jean-Romain/lidR/master/readme.img/chm.png">

`lidR` have several algorithms from the litterature to compute canopy height models either point-to-raster based (`grid_canopy`) or triangulation based (`grid_tincanopy`). This allows to test and compare some methods relying on a CHM such as individual tree segmentation or the computation of a canopy roughtness index.

```r
las = readLAS("<file.las>")

# Khosravipour et al. pitfree algorithm
th = c(0,2,5,10,15)
edge = c(0, 1.5)
chm = grid_tincanopy(las, thresholds = th, max_edge = edge)

plot(chm)
```

### Read and display a catalog of las files

<img align="right" src="https://raw.githubusercontent.com/Jean-Romain/lidR/master/readme.img/catalog-demo.gif">

`lidR` enables the user to manage, use and process a catalog of `las` files. The function `catalog` build a `LAScatalog` object from a folder. A function `plot` displays this catalog on an interactive map using the `mapview` package.

```r
ctg = catalog("<folder/>")
ctg@crs = sp::CRS("+proj=utm +zone=17")

# CRS set: will be display on an interactive map
plot(ctg)
```

From a `LAScatalog` the user can (for example) extract some regions of interest (ROI) with `lasclip` or `catalog_queries`. Using a catalog for the extraction of the ROI guarantes fast and memory efficient clip. `LAScatalog` object allows many other manipulations that are usually done with multicore if possible.

### Individual tree segmentation

<img align="left" src="https://raw.githubusercontent.com/Jean-Romain/lidR/master/readme.img/rotating-treeseg.gif" margin-right="5px">

`lastrees` function have several algorithms from the litterature for individual tree segmentation either based on the digital model of canopy or on the point-cloud. Each algorithm has been coded from the original article being as close as possible to what was written in the peer-reviwed papers. Our goal is to make usable/testable/comparable what has been published.

```r
las = readLAS("<file.las>")

lastrees(las, algorithm = "li2012")

col = random.colors(200)
plot(las, color = "treeID", colorPalette = col)
```

### Other tools

`lidR` have plenty of other tools and is a continuouly improved package. If it does not exist in `lidR` ask us for a new feature and depending on the feasability we will be glad to implement such feature.

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

# Changelog

[See changelogs on NEW.md](https://github.com/Jean-Romain/lidR/blob/master/NEWS.md)
