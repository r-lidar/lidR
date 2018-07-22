## lidR v1.6.0 (2018-07-20)

#### NEW FEATURE

* New function `tree_hulls` that computes a convex or concave hull for each segmented tree.
* New option `stop_early` that enables processing of an entire catolog or stops if an error occurs.
* New function `catalog_retile` supersedes the function `catalog_reshape` and performs the same task while adding much more functionality.

#### ENHANCEMENTS

* When processing a `LAScatalog`, error handling has been seriouly improved. A process can now run until the end even with errors. In this case clusters with errors are skipped.
* When processing  a `LAScatalog`, the graphical progress now uses 3 colors. green: ok, red: error, gray: null.
* `as.spatial()` for `LAS` object preserves the CRS.
* All the functions now have strong assertions to check user inputs.
* `plot.LAScatalog` always displays the catalog with `mapview` by default even if the CRS is empty.
* In `lastrees_dalponte` the matching between the seeds and the canopy is more tolerant. Rasters can have different resolution and/or extent.
* `lasground` uses (as an option) only the last and single returns to perform the segmentation.

#### OTHER CHANGES

* `catalog()` displays a message when finding overlaps between files.
* The LAScatalog class is more thoroughly documented.
* Clusters now align on (0,0) by default when processing a `LAScatalog` by cluster.

#### BUG FIXES

* `lasscanline()` did not compute the scanline because the conditional statement that checked if the field was properly populated was incorrect.
* [[#146](https://github.com/Jean-Romain/lidR/issues/146)] Fix matching between tree tops, raster and canopy raster.
* `tree_detection` when used with a point cloud was not properly coded and tended to miss some trees.
* In `lasclip*` if `ofile` was non empty, the function wrote properly the file but returned a non-expected error.
* [[#155](https://github.com/Jean-Romain/lidR/issues/155)] user supplied function was being analysed by `future` and some function were missing. User supplied function is now manually analysed.
* [[#156](https://github.com/Jean-Romain/lidR/pull/156)] Fix error when `lasclip` was used with a `SpatialPolygonDataFrame`.

## lidR v1.5.1 (2018-06-14)

#### BUG FIXES

* The area of a `LAScatalog` was wrongly computed for non square tiles because of a bad copy/paste in the code.
* [[#135](https://github.com/Jean-Romain/lidR/issues/135)] Fix `NULL` class objects returned by `grid_*` functions when processing a `LAScatalog` if the first cluster is empty.
* [[#143](https://github.com/Jean-Romain/lidR/issues/143)] `rumple_index` returns `NA` if not computable.

## lidR v1.5.0 (2018-05-13)

#### SIGNIFICANT CHANGES

* `catalog_options()` is formally deprecated. Use `LAScatalog` properties instead (see `?catalog`).
* The package `magrittr` is no longer loaded with `lidR`. Thus, piping operators are no longer usable by default. To use piping operators use `library(magrittr)`.

#### NEW FEATURES

* New `lassmooth` function. A point cloud-based smoothing function.
* New `lasfiltersurfacepoints` function to filter surface points.
* New `grid_catalog` function is a simplified and more powerful function like `catalog_apply` but specifically dedicated to `grid_*` outputs.
* New functions `lasadddata`, `lasaddextrabyte` and `lasaddextrabyte_manual` to add new data in a `LAS` object.
* `lasclip` can clip a `SpatialPolygonsDataFrame`
* `lasclipRectangle` and `lasclipCircle` can clip multiple selections (non-documented feature).
* The `treeID` computed with `lastrees_*` functions can now be written in a `las/laz` file by default.

#### OTHER CHANGES

* `LAScatalog` objects are processed with a single core by default.
* `lasdecimate` is formally deprecated. Use `lasfilterdecimate`
* `grid_density` now returns both the point and the pulse density, where possible.
* The option `P` is no longer set by default in `readLAS`.
* The documentation of `lastrees` has been split into several pages.
* When a catalog is processed using several cores, if an error is raised the process triggers an early signal to stop the loop. In previous releases the entire process was run and the error was raised at the end when the futures were evaluated.

#### BUG FIXES

* `grid_metrics(lidar, stdmetrics_i(Intensity))` returned and empty `data.table`
* [[#128](https://github.com/Jean-Romain/lidR/issues/128)] Fix raster data extraction using the slower and memory-greedy, but safer `raster::extract` function.
* [[#126](https://github.com/Jean-Romain/lidR/issues/126)] propagate the CRS in filter functions.
* [[#116](https://github.com/Jean-Romain/lidR/issues/116)] Fix clash between function `area` from `lidR` and from `raster`.
* [[#110](https://github.com/Jean-Romain/lidR/issues/110)] Fix out-of-bounds rasterization.

## lidR v1.4.2 (2018-04-19)

#### BUG FIXES

* [[#103](https://github.com/Jean-Romain/lidR/issues/103)] fix user-defined function not exported in clusters on Windows
* [[#104](https://github.com/Jean-Romain/lidR/pull/104)] fix potential bin exclusion in `entropy` function
* [[#106](https://github.com/Jean-Romain/lidR/issues/106)] fix wrong count of points below 0
* Fix wrong type attribution in `lasclassify` when using the shapefile's table of attributes as data.
* Fix column addition when `field = NULL` in `lasclassify`.
* Fix `NA` return in entropy when negative value are found.

#### NEW FEATURES

* Li et al algorithm has a new parameter Zu (see reference) that is no longer hard coded.

## lidR v1.4.1 (2018-02-01)

####  OTHER CHANGES

* Removed examples and unit tests that imply the watershed segmentation to make CRAN check happy with the new rules relative to bioconductor packages.

#### NEW FEATURES

* Parameter `start` has been enabled in `grid_metrics` with `catalogs`.

## lidR v1.4.0 (2018-01-24)

#### NEW FEATURES

* `lasclip` and `lasclip*` can extract from a catalog.
* `lasclip` supports `sp::Polygon` objects.
* `lastrees` gains a new algorithm from Silva et al. (2016).
* `lastrees` with the Li et al. (2012) algorithm gains a new parameter to prevent over-segmentation.
* new function `lassnags` for classifying points as snag points or for segmenting snags.
* new function `tree_detection` to detect individual trees. This feature has been extracted from `lastrees`'s algorithms and it is now up to the users to use `lidR`'s algos or other input sources.
* `plot` supports natively the `PointCloudViewer` package available on github.

#### BUG FIXES

* Fix missing pixel in DTM that made normalization impossible.
* [[#80](https://github.com/Jean-Romain/lidR/issues/80)] fix segfault.
* [[#84](https://github.com/Jean-Romain/lidR/issues/84)] fix bug in `lasscanline`.

#### ENHANCEMENTS
* `lastrees` with the Li et al. (2012) algorithm is now 5-6 times faster and much more memory efficient.
* `lastrees` with the Li et al. (2012) algorithm no longer sorts the original point cloud.
* `lastrees` with the Dalponte et al (2016) algorithm is now computed in linear time and is therefore hundreds to millions times faster.
* `catalog_reshape()` streams the data and uses virtually zero memory to run.
* `grid_canopy()` has been rewritten entirely in C++ and is now 10 to 20 times faster both with the option `subcircle` or without it.
* `grid_canopy()` with the option `subcircle` uses only 16 bytes of extra memory to run, while this feature previously required the equivalent of several copies of the point cloud (several hundreds of MB).
* `as.raster()` is now three times faster.
* `lasclassify` now uses a QuadTree and is therefore faster. This enables several algorithms to run faster, such as `lastrees` with Silva's algo.

#### OTHER CHANGES

* `lasground` with the PMF algorithm now accepts user-defined sequences.
* `lasground` with the PMF algorithm has simplified parameter names to make them easier to type and understand, and to prepare the package for new algorithms.
* `lasground` documentation is more explicit about the actual algorithm used.
* `lasground` now computes the windows size more closely in line with the original Zhang paper.
* `lastrees` when used with raster-based methods now accepts a missing las object. In that case `extra` is turned to `true`.
*  new parameter `p` (for power) added to functions that enable spatial interpolation with IDW.

## lidR v1.3.1 (Release date: 2017-09-20)

#### BUG FIXES

* Fix a bug of computer precision leading to non interpolated pixels at the boundaries of the QuadTree.

## lidR v1.3.0 (Release date: 2017-09-16)

This version is dedicated to extending functions and processes to entire catalogs in a continuous way.
Major changes are:

* How `catalog_apply` works. More powerful but no longer compatible with previous releases
* Former existing functions that now natively support a `Catalog` 
* Management of buffered areas

#### NEW FEATURES

* `catalog_apply` has been entirely re-designed. It is more flexible, more user-friendly and enables loading of buffered data.
* `catalog_queries` has now an argument `...` to pass any argument of `readLAS`.
* `catalog_queries` has now an argument `buffer` to load extra buffered points around the region of interest.
* `grid_metrics` accepts a catalog as input. It allows users to grid an entire catalog in a continuous way.
* `grid_density` also inherits this new feature
* `grid_terrain` also inherits this new feature
* `grid_canopy` also inherits this new feature
* `grid_tincanopy` also inherits this new feature
* `grid_metrics` has now has an argument `filter` for streaming filters when used with a catalog
* New function `catalog_reshape`

#### OTHER CHANGES

* `lasnormalize` updates the point cloud by reference and avoids making deep copies. An option `copy = TRUE` is available for compatibility with former versions.
* `readLAS` arguments changed. The new syntax is simpler. The previous syntax is still supported.
* `catalog_index` is no longer an exported function. It is now an internal function.
* `plot.Catalog` accepts the usual `plot` arguments
* `catalog_queries` and `catalog_apply` do not expect a parameter `mc.cores`. This is now driven by global options in `catalog_options()`.
* `grid_metrics` and `lasmetrics` do not expect a parameter `debug`. This is now driven by global options in `lidr_options`.
* `catalog` can build a catalog from a set of paths to files instead of a path to a folder.
* removed `$` access to LAS attribute (incredibly slow)
* `catalog_select` is more pleasant an more interactive to use.
* S3 `Catalog` class is now a S4 `LAScatalog` class
* `LAS` and `LAScatalog` class gain a slot `crs` automatically filled with a proj4 string
* `plot.LAScatalog` display a google map background if the catalog has a CRS.
* `plot.LAScatalog` gains an argument `y` to display a either a terrain, road, satellite map.
* `lasarea` is deprecated. Use the more generic function `area`

#### BUG FIXES

* Computer precision errors lead to holes in raster computed from a Delaunay triangulation.
* Message in `writeLAS` for skipped fields when no field is skipped is now correct.

#### ENHANCEMENTS

* `grid_terrain` with delaunay allocates less memory, makes fewer deep copies and is 2 to 3 times faster
* `grid_terrain` with knnidw allocates less memory, makes fewer deep copies and is 2 to 3 times faster
* `lasnormalize` and `lasclassify` no longer rely on `raster::extract` but on internal `fast_extract`, which is memory efficient and more than 15 times faster.
* `catalog` enables a `LAScatalog` to be built 8 times faster than previously.
* removed dependencies to `RANN` package using internal k-nearest neighbor search (2 to 3 times faster)

## lidR v1.2.1 (Release date: 2017-06-12)

#### NEW FEATURES

* new function `tree_metrics`.
* new function `stdtreemetrics`.
* `grid_tincanopy()` gains a parameter `subcircle` like `grid_canopy()`
* new function `rumple_index` for measuring roughness of a digital model (terrain or canopy)
* global options to parameterize the package - available with `lidr_options()`

#### BUG FIXES

* Installation fails if package sp is missing.
* Memory leak in QuadTree algorithm. Memory is now free after QuadTree deletion.
* Dalponte's algorithm had a bug due to the use of std::abs which works with integers. Replaced by std::fabs which works with doubles.
* In `grid_tincanopy` `x > 0` was replaced by `x >= 0` to avoid errors in the canopy height models
* Triangle boundaries are now taken into account in the rasterization of the Delaunay triangulation

#### OTHER CHANGES

* `lastrees` Li et al. algorithm for tree segmentation is now ten to a thousand of times faster than in v1.2.0
* `grid_terrain`, the interpolation is now done only within the convex hull of the point cloud
* `grid_tincanopy` makes the triangulation only for highest return per grid cell.
* `grid_tincanopy` and `grid_terrain` using Delaunay triangulation is now ten to a hundred times faster than in v1.2.0
* `as.raster` now relies on `sp` and is more flexible
* `as.raster` automatically returns a `RasterStack` if no layer is provided.
* `plot.lasmetrics` inherits `as.raster` changes and can display a `RasterStack`

## lidR v1.2.0 (Release date: 2017-03-26)

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
* [[#54](https://github.com/Jean-Romain/lidR/pull/54)] better re-computation of the header of LAS objects.

#### OTHER CHANGES

* Slightly faster point classification from shapefiles.
* [[#51](https://github.com/Jean-Romain/lidR/pull/51)] in `grid_terrain`, forcing the lowest point to be retained is now an option `keep_lowest = FALSE`

## lidR v1.1.0 (Release date: 2017-02-05)

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


## lidR v1.0.2 (Release date: 2016-12-31)

Third submission

* Change: explain LiDAR in the Description - requested by Kurt Hornik.

## lidR v1.0.1 (Release date: 2016-12-30)

Second submission - rejected

* Change: split the package in two parts. 'lidR' relies on 'rlas' to read binary files.

## lidR v1.0.0 (Release date: 201-12-16)

First submission - rejected
