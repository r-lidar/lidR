![](https://raw.githubusercontent.com/Jean-Romain/lidR/master/others/README/lidr-ban.png)<br/>

![Github](https://img.shields.io/badge/Github-2.0.0-green.svg) ![licence](https://img.shields.io/badge/Licence-GPL--3-blue.svg) 

R package for Airborne LiDAR Data Manipulation and Visualization for Forestry Applications

The lidR package provides functions to read and write `.las` and `.laz` files, plot point clouds, compute metrics using an area-based approach, compute digital canopy models, thin lidar data, manage a catalog of datasets, automatically extract ground inventories, process a set of tiles using multicore processing, individual tree segmentation, classify data from geographic data, and provides other tools to manipulate LiDAR data in a research and development context.

* Development of the `lidR` package between 2015 and 2018 was made possible thanks to the financial support of the [AWARE project  (NSERC CRDPJ 462973-14)](http://aware.forestry.ubc.ca/); grantee [Prof Nicholas Coops](http://profiles.forestry.ubc.ca/person/nicholas-coops/).
* Development of the `lidR` package between 2018 and 2019 was made possible thanks to the financial support of the Ministère des Forêts, de la Faune et des Parcs of Québec.

# Content

1. [Key features](#key-features)
2. [Some examples](#some-examples)
3. [Install lidR](#install-lidr)
4. [Changelog](#changelog)

# Key features

- [Read and write .las and .laz files](https://github.com/Jean-Romain/lidR/wiki/readLAS)
- [Plot 3D LiDAR data](https://github.com/Jean-Romain/lidR/wiki/lasplot)
- [Area-based approach using any set of metrics](https://github.com/Jean-Romain/lidR/wiki/grid_metrics)
- [Individual tree segmentation](https://github.com/Jean-Romain/lidR/wiki/Tree-segmentation-from-A-to-Z)
- [Classify and clip data from geographic shapefiles](https://github.com/Jean-Romain/lidR/wiki/lasclassify)
- [Manage a catalog of tiles](https://github.com/Jean-Romain/lidR/wiki/catalog)
- [Automatically extract a set of ground plot inventories](https://github.com/Jean-Romain/lidR/wiki/catalog_queries)
- [Analyse a full set of tiles in parallel computing](https://github.com/Jean-Romain/lidR/wiki/catalog_apply)
- [Compute a digital canopy model (DCM)](https://github.com/Jean-Romain/lidR/wiki/Rasterizing-perfect-canopy-height-models)
- [Compute a digital terrain model (DTM)](https://github.com/Jean-Romain/lidR/wiki/grid_terrain)
- [Normalize a point cloud by substracting a DTM](https://github.com/Jean-Romain/lidR/wiki/lasnormalize)
    
# Some examples

<img align="right" src="https://raw.githubusercontent.com/Jean-Romain/lidR/master/others/README/point-cloud-rotating.gif">

### Read and display a las file

In R-fashion style the function `plot`, based on `rgl`, enables the user to display, rotate and zoom a point cloud. Because `rgl` has limited capabilities with respect to large datasets, we also made a package [PointCloudViewer](https://github.com/Jean-Romain/PointCloudViewer) with greater display capabilites.

```r
las <- readLAS("<file.las>")
plot(las)
```

### Compute a canopy height model

<img align="left" src="https://raw.githubusercontent.com/Jean-Romain/lidR/master/others/README/chm-Khosravipour.png">

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

<img align="right" src="https://raw.githubusercontent.com/Jean-Romain/lidR/master/others/README/catalog-plot_interactive.gif">

`lidR` enables the user to manage, use and process a catalog of `las` files. The function `catalog` builds a `LAScatalog` object from a folder. The function `plot` displays this catalog on an interactive map using the `mapview` package (if installed).

```r
ctg <- catalog("<folder/>")
projection(ctg) <- sp::CRS("+proj=utm +zone=17")
plot(ctg)
```

From a `LAScatalog` object the user can (for example) extract some regions of interest (ROI) with `lasclip`. Using a catalog for the extraction of the ROI guarantees fast and memory-efficient clipping. `LAScatalog` objects allow many other manipulations that can be done with multicore processing, where possible.

### Individual tree segmentation

<img align="left" src="https://raw.githubusercontent.com/Jean-Romain/lidR/master/others/README/its-rotating-tree-segmented.gif" margin-right="5px">

The `lastrees` function has several algorithms from the literature for individual tree segmentation, based either on the digital canopy model or on the point-cloud. Each algorithm has been coded from the source article to be as close as possible to what was written in the peer-reviwed papers. Our goal is to make published algorithms usable, testable and comparable.

```r
las <- readLAS("<file.las>")

las <- lastrees(las, li2012())
col <- random.colors(200)
plot(las, color = "treeID", colorPalette = col)
```

### Other tools

`lidR` has many other tools and is a continuouly improved package. If it does not exist in `lidR` please ask us for a new feature, and depending on the feasability we will be glad to implement your requested feature.

# Install `lidR`

* The latest released version from CRAN with

```r
install.packages("lidR")
```

* The latest stable development version from github with


```r
devtools::install_github("Jean-Romain/rlas", ref="devel")
devtools::install_github("Jean-Romain/lidR", ref="devel")
```

To install the package from github make sure you have a working development environment.

* **Windows**: Install [Rtools.exe](https://cran.r-project.org/bin/windows/Rtools/).  
* **Mac**: Install `Xcode` from the Mac App Store.
* **Linux**: Install the R development package, usually called `r-devel` or `r-base-dev`

# Changelog

[See changelogs on NEW.md](https://github.com/Jean-Romain/lidR/blob/master/NEWS.md)
