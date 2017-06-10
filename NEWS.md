### lidR v1.2.1 (in development)

#### NEW FEATURES

* new function `tree_metrics`.
* new function `stdtreemetrics`.
* `grid_tincanopy` gain a parameter `subcircle` like `grid_canopy`
* new function `rumple_index` for measuring roughness of a digital model (terrain or canopy)
* global options avaible with `lidr_options()`

#### BUG FIXES

* Installation failed if package sp was missing.
* Memory leak in the QuadTree. Memory is now free when a QuadTree is deleted.
* In dalponte's algorithm there was a bug due to the use of std::abs which work with intergers instead of std::fabs which works with doubles.
* In `grid_tincanopy` a `x > 0` instead of `x >= 0` leaded to weird canopy height models
* Boudaries were ommited during the rasterization of the delaunay triangulation

#### OTHER CHANGES

* `lastrees` Li et al. algorithm for tree segmentation is dozen to thouthand times faster than in v1.2.0
*  `grid_terrain`, the interpolation is done only within the convex hull of the point cloud
* `grid_tincanopy` makes the triangulation only for highest return per grid cell.
* `grid_tincanopy` and `grid_terrain` using Delaunay triangulation are dozen to hundred times faster than in v1.2.0
* `as.raster` relies on `sp` and is more flexible
* `as.raster` automatically return a `RasterStack` if no information is provided.
* `plot.lasmetrics` inherit of `as.raster` changes and can display a `RasterStack`

### lidR v1.2.0 (Release date: 2017-03-26)

#### NEW FEATURES

* new function `lasground` for ground segmentation.
* new function `grid_tincanopy`. Canopy height model using Khosravipour et al. pit-free algorithm.
* new function `grid_hexametrics`. Area-based approach in hexagonal cells.
* `lasnormalize` allows for "non-discretized" normalization i.e interpolating each point instead of using a raster.
* internally `lascheck` performs more tests to check if the header is in accordance with the data.

#### BUG FIXES

* [[#48](https://github.com/Jean-Romain/lidR/pull/48)] `gap_fraction_profile()` bug with negative values (thanks to Florian de Boissieu)
* [[#49](https://github.com/Jean-Romain/lidR/pull/49)] typo error leading to the wrong metric in `stdmetric_i` 
* [[#50](https://github.com/Jean-Romain/lidR/pull/50)] typo error leading to the wrong metric in `stdmetric` 
* Fix bug in `stdmetric_z` when `max(Z) = 0`
* [[#54](https://github.com/Jean-Romain/lidR/pull/54)] better recomputation of the header of LAS objects.

#### OTHER CHANGES

* Slightly faster point classification from shapefiles.
* [[#51](https://github.com/Jean-Romain/lidR/pull/51)] in `grid_terrain`, forcing the lowest point to be retained is now an option `keep_lowest = FALSE`

### lidR v1.1.0 (Release date: 2017-02-05)

#### NEW FEATURES

* `lastree()` for individual tree segmentation
* `readLAS()` gains a parameter `filter` from `rlas (>= 1.1.0)`
* `catalog_queries()` relies on `rlas (>= 1.1.0)`. It saves a lot of memory, is 2 to 6 times faster and supports .lax files.

#### OTHER CHANGES

* `colorPalette` parameter in `plot.LAS()` now expects a list of colors instead of a function. Use `height.colors(50)` instead of `height.colors`
* The header of a LAS object is now an S4 class object called `LASheader`
* The spatial interpolation method `akima` is now called `delaunay` because it corresponds to what is actually computed.
* The spatial interpolation method `akima` lost its parameter `linear`.
* The spatial interpolation method `kriging` now performs a KNN kriging.
* `catalog_queries()` lost the parameter `...` all the fields are loaded by default.
* Removed `lasterrain()` which was not consistent with other functions and not useful.

#### BUG FIXES

* The header of LAS objects automatically updates `Number of point records` and `Number of nth return`.
* `lasnormalize()` updates the header and returns warnings for some behaviors
* [[#39](https://github.com/Jean-Romain/lidR/issues/39)] interpolation with duplicated ground points


### lidR v1.0.2 (Release date: 2016-12-31)

Third submission

* Change: explain LiDAR in the Description - requested by Kurt Hornik.

### lidR v1.0.1 (Release date: 2016-12-30)

Second submission - rejected

* Change: split the package in two parts. 'lidR' relies on 'rlas' to read binary files.

### lidR v1.0.0 (Release date: 201-12-16)

First submission - rejected
