### lidR v1.3.0 (in development)

This version is dedicated to extend functions to an process entiere catalogs in a continuous way.
Major changes are mainly:
* How `catalog_apply` works. More powerful but no longer compatible with previous releases
* Former existing functions which now natively support a ` Catalog` 
* Management of buffered areas

#### NEW FEATURES

* `catalog_apply` has been entierily re-designed. It is more flexible, more user-friendly and enable for loading buffered data.
* `catalog_queries` has now an argument `...` to pass any argument of `readLAS`.
* `catalog_queries` has now an argument `buffer` to load extra buffered points around the region of interest.
* `grid_metrics` accept a catalog as input. It enables to grid an entiere catalog in a continuous way.
* `grid_density` inherit of this new feature as well
* `grid_terrain` inherit of this new feature as well
* `grid_canopy` inherit of this new feature as well
* `grid_tincanopy` inherit of this new feature as well
* `grid_metrics` has now a an argument `filter` for streaming filters when used with a catalog
* New function `catalog_reshape`

#### OTHER CHANGES

* `catalog_queries` is no longer an exported function. It is now an internal function.
* `plot.Catalog` accept for usual `plot` arguments
* `catalog_queries` and `catalog_apply` do not expect a parameter `mc.cores`. This is now driven by global options in `catalog_options()`.
* `grid_metrics` and `lasmetrics` do not expect a parameter `debug`. This is now driven by global options in `lidr_options`.
* `as.raster` relies both on the fast former code from v1.2.0 to return a RasterLayer and the new code from v1.2.1 to return a RasterStack. This change enable for a faster results in most cases
* `catalog` can build a catalog from a set of paths to files instead a path to a folder.
* removed dependencie to `RANN` package using internal k-nearest neigbourg search (2 to 3 times faster)

#### BUG FIXES

* Computer precision errors lead to holes in raster computed from a Delaunay triangulation.
* Message in `writeLAS` for skipped fields when no field is skipped is know correct.

### lidR v1.2.1 (Release data: 2017-06-12)

#### NEW FEATURES

* new function `tree_metrics`.
* new function `stdtreemetrics`.
* `grid_tincanopy()` gains a parameter `subcircle` like `grid_canopy()`
* new function `rumple_index` for measuring roughness of a digital model (terrain or canopy)
* global options to parameterize the package - avaible with `lidr_options()`

#### BUG FIXES

* Installation fails if package sp is missing.
* Memory leak in QuadTree algorithm. Memory is now free after QuadTree deletion.
* Dalponte's algorithm had a bug due to the use of std::abs which works with intergers. Replaced by std::fabs which works with doubles.
* In `grid_tincanopy` `x > 0` was replaced by `x >= 0` to avoid errors in the canopy height models
* Triangle boudaries are now taken into account in the rasterization of the Delaunay triangulation

#### OTHER CHANGES

* `lastrees` Li et al. algorithm for tree segmentation is now ten to a thousand of times faster than in v1.2.0
* `grid_terrain`, the interpolation is now done only within the convex hull of the point cloud
* `grid_tincanopy` makes the triangulation only for highest return per grid cell.
* `grid_tincanopy` and `grid_terrain` using Delaunay triangulation is now ten to a hundred times faster than in v1.2.0
* `as.raster` now relies on `sp` and is more flexible
* `as.raster` automatically returns a `RasterStack` if no layer is provided.
* `plot.lasmetrics` inherits `as.raster` changes and can display a `RasterStack`

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
