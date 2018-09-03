## lidR v2.0.0 (in development)

# Main visible changes

**`LAScatalog` processing engine**

**Algorithm dispatch**

**Third party extensions**

**Consistency improvement**

# Complete description of visible changes

**Exemple files**

* New: the three exemple files are now georeference with an EPSG code that is read and converted to a `proj4string`.
* New: the exemple file `MixedConifers.laz` contains the segmented trees in extra bytes 0.

**LAS class**

* Change: the `LAS` class **is** now a `Spatial` object or more technically it inherits a `Spatial` object.
* Change: being a `Spatial` a `LAS` does not have a slot `@crs` anymore. It has now a slot `@proj4string`.
* New: being a `Spatial` object a `LAS` object inherit of mutilple function from `raster`and `sp` such as `$` and `[[` accessors or `raster::extent`, `sp::bbox`, `raster::projection` and so on. However the replacement method `$<-`, `[[<-` have restricted capabilities to ensure the a `LAS` cannot be modified in a way that implie to loose the properties of the LAS specifications
* New: empty `LAS` with 0 points are now allowed. This have repercussions on several functions including `lasfilter`, `lasclip`, `readLAS` that do not return `NULL` for empty data but a `LAS` objects with 0 point. This new behavior has been introduced to fixes the old unconsitant behavior of function that return either `LAS` or `NULL`. `LAS` is always returned.

**LAScatalog class**

* Change: the `LAScatalog` class **is** now a `SpatialPolygonsDataFrame` or more technically it inherits a `SpatialPolygonsDataFrame`.
* Change: being a `SpatialPolygonsDataFrame` a `LAScatalog` does not have a slot `@crs` anymore. It has now a slot `@proj4string`.
* Change: there are no longer any slot `@cores`, `@by_file`, `@buffer` and so on. They are replaced by more generic and scalable slots `@processing_options`, `@output_options`, `@clustering_options` and `@input_options` that are list of options classified by their main role.
* Change: documentation has been entierly rewritten to explain the whole potential of the class.
* Change: functions `by_file`, `progress`, `tiling_size`, `buffer` were replaced by `set_buffer`, `get_buffer`, `set_progress`, `get_progress` and so on. These allows to get a consitant set of functions that do not overlap with functions from `raster`.

**readLAS**

* Change: `readLAS` no longer supports option `PFC`. User must use function `laspulse`, `lasflightlines` and `lascolors`.

**lasclip**

* New: `lasclip` works now both with a `LAS` object and a `LAScatalog`  in a seamless way. There is no longer any difference between the capabilities of the `LAS` version on the `LAScatalog` one. 
* New: `lasclip` support many geometries including multipart polygons and polygons with holes both with a `LAS` object and a `LAScatalog`.
* Change: The option `inside` has been removed for consitency because it cannot be safely supported both on `LAS` and `LAScatalog`.
* Change: The option `ofile` has been removed for consitency and this option in now managed by `LAScatalog` internal processing engine. For example one can extract ground inventories and write them in `laz` files automatically named after their center coordinates like that:

```r
ctg = catalog(folder)
output_files(ctg) <- "path/to/a/file_{XCENTER}_{YCENTER}"
laz_compression(ctg) <- TRUE
new_ctg = lasclipCircle(ctg, xc,yc, r)
```

* Change: documentation has been reviewed and extented
* Change: `lasclip` does not return `NULL` for empty queries but an empty `LAS`.
* Fix: `lasclipRectangle` returns the same output both with a `LAS` and a `LAScatalog`. With a `LAS` the rectangle is now closed on the bottom and the left and open on the right and the top.

**catalog_queries**

* Change: `catalog_queries` have been removed because it is superseded by `lasclip`

**lasnormalize**

* Change: remove the old option `copy = TRUE`.
* Change: `lasnormalize` relies now on "lidR algorithms dispatch". lidR algorithms are function factories that allow to use one or another algorithm within a given function. (see also the main new features above).
* New: `lasnormalize` can be applied on a `LAScatalog` to write a new catalog.

**lasclassify**

* Fix: the classification, when made with a `RasterLayer` preserve the data type of the `RasterLayer`. This also fixes the fact that `lastrees` classified the tree with `double` instead of `int`.

**tree_detection**

* Change: `tree_detection` relies now on "lidR algorithms dispatch". lidR algorithms are function factories that allow to use one or another algorithm within a given function. (see also the main new features above).
* New: algorithm `lmf` have a user-defined variable size search windows and have two possible search windows shapes (square of disc).
* New: introduction of the `manual` algorithm for manual correction of tree detections.
* New: introduction of the `multichm` algorithm using a multi chm as published in a paper (see reference).
* New: introduction of the `ptrees` algorithm as ublished in a paper (see reference) 
* New: `tree_detection` algorithm (except `manual`) are seamlessly useable with a `LAScatalog`. Thus, the following just works:

```r
ctg = catalog(folder)
ttop = tree_detection(ctg, lmf(5))
```

* Change: the `lmf` algorithm, when used with a `RasterLayer` as input, expect parameters given in the units of the map and no longer in pixels.
* Change: `tree_detection` function return constistently a `SpatialPointsDataFrame` whatever the algorithm.
* Change: `tree_detection` function based on a CHM no longer support a `lasmetric` object as input. Anyway this class no longer exists.

**tree_metrics**

* Change: `tree_metrics` returns a `SpatialPointsDataFrame`.
* Change: `tree_metrics` is seamlessly useable with a `LAScatalog`. Thus, this just works if the las file have and extra bytes attributes that stores the tree ids:

```r
ctg = catalog(folder)
metrics = tree_metrics(ctg, list(`Mean I` = mean(Intensity)))
```

**lastrees**

* Change: `lastrees` relies now on "lidR algorithms dispatch". lidR algorithms are function factories that allow to use one or another algorithm within a given function. (see also the main new features above).
* New: introduction of the `ptrees` algorithm with Vega et al. (2014) algorithm.
* New: introduction of the `hamraz2016` algorithm Hamraz et al. (2016) algorithm.
* New: introduction of the `mcwatershed` algorithm that implements a marker-controlled watershed.

**grid_metrics**

* Change: `grid_metrics` as well as other `grid_*` functions return consitantly a `RasterLayer` or a `RasterBrick` instead of a `data.table`.
* Change: option `splitlines` has been removed. `grid_metrics` used to return a `data.table` because of the `splitlines` option and all `lidR` was built on top of that feature from the very begining. Now `lidR` uses consistantly `sp` and `raster` and this option is no longer supported.

**grid_terrain**

* Change: `grid_terrain` relies now on "lidR algorithms dispatch". lidR algorithms are function factories that allow to use one or another algorithm within a given function. (see also the main new features above).
* Change: `grid_terrain` returns consitantly a `RasterLayer` instead of a `data.table` whatever the algorithm used.

**grid_canopy**

* Change: `grid_canopy` relies now on "lidR algorithms dispatch". lidR algorithms are function factories that allow to use one or another algorithm within a given function. (see also the main new features above).
* Change: `grid_canopy` returns consitantly a `RasterLayer` instead of a `data.table` whatever the algorithm used.
* Change: `grid_canopy` no longer fills NAs with the parameters `na.fill`.

**grid_tincanopy**

* Change: `grid_tincanopy` has been removed. Digital Surface Model are constistanly driven by the function `grid_canopy` and the lidR algorithm dispatch engine. The algorithms that replaced `grid_tincanopy` are `dsmtin` and `pitfree`

**grid_hexametrics**

* Change: as for `grid_metrics` parameter `splitlines` have been removed as well as parameter `debug`.
* Change: the function returns an `hexbin` object or a list of `hexbin` objects and no longer any `data.table`.

**grid_catalog**

* Change: `grid_catalog` has been remove. New LAScatalog processing engine implies that this function is no longuer useful.

**class lasmetrics**

* `data.table` with a class `lasmetrics` no longer exists. It has been consitantly replaced by `RasterLayer` and `RasterBrick` everywhere.
* `as.raster` no longer exists because it used to convert `lasmetrics` to `RasterLayer` and `RasterStack`.
* `as.spatial` no longer convert `lasmetrics` to `SpatialPixelsDataFrame` but still converts `LAS` to `SpatialPointsDataFrame`. 
* `plot.lasmetrics` have been removed obviously.

**lasroi**

* Change: `lasoi` has been removed. It was not useful and bugged. It will maybe be reintroduced later in `lasclipManual`.

**lasfilterdecimate**

* Change: relies now on "lidR algorithms dispatch". lidR algorithms are function factories that allow to use one or another algorithm within a given function. (see also the main new features above).
* New: introduction of the algorithm `highest` available in `lasfilterdecimate` and that supersedes the fonction `lasfiltersurfacepoints`.

**lassnags**

* Change: relies now on "lidR algorithms dispatch". lidR algorithms are function factories that allow to use one or another algorithm within a given function. (see also the main new features above).

**lidr_options**

* Change: `lidr_option` has been removed. the options are now managed by regular R base options with function `options`. Availables options are named `lidR.*`.


# Internal major changes that are not directly visible

* Change: the code that drives the point algorithm relies on `boost` and drastically simplyfies the former code of `lasclassify`

## lidR v1.6.1 (2018-08-21)

#### BUG FIXES

* [[#161](https://github.com/Jean-Romain/lidR/pull/161)] Fix tree ID matching.
* Fix undefined variable in cluster_apply on mac and linux if multicore processing is used.
* Fix rare case of unit test failure due to the random nature of the test dataset using seeds.


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
