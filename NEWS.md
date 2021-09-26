If you are viewing this file on CRAN, please check [the latest news on GitHub](https://github.com/Jean-Romain/lidR/blob/master/NEWS.md) where the formatting is also better

## lidR v3.2.0 (Release date: ...)

#### ANNOUCEMENT

`rgdal` and `rgeos` will be retired on Jan 1st 2024. `raster` and `sp` are based on `rgdal`/`rgeos`. `lidR` is based on `raster` and `sp` because it was created before `sf`, `terra` and `stars`. This means that sooner or later `lidR` will run into trouble (actually it has already started to be the case). So, it is time to fully embrace `sf`, `terra`/`stars` and to leave `sp` and `raster`. This will require an in-depth rebase of `lidR`. We have started the work and we plan to release `lidR` 4.0.0 that will no longer have any internal code that uses `sp` and `raster`. This version already no longer uses `rgdal`. We hope make these changes with minimal breakage in backward compatibility by maintaining the conversion to `sp`/`raster` for functions from v < 4.0.0, but some backward incompatibilities will necessarily arise. In particular, `LAS` will no longer inherit the `sp::Spatial` class and will no longer contain a `sp::CRS` but a `sf::crs` and `LAScatalog` will no longer be `sp::SpatialPolygonDataFrame`. Our plan is (hopefully) to rebase `lidR` in such a way that nobody will notice the changes expect users who dig a little deeper into the objects.

#### CHANGES

1. `hexbin_metrics()` was an unused function and has been removed from `lidR`. It can be retrieved in [`lidRplugins`](https://github.com/Jean-Romain/lidRplugins)

2. Functions using the former namespace such as `lassomething()` that were renamed into `verb_noun()` in version 3.0.0 now throw a warning. In v3.0.0 they were still usable for backward compatibility but not documented. In v3.1.0 they printed a message saying to move on to the new namespace. Now in 3.2.0 they throw a formal warning saying to move on to the new namespace. They will throw an error in the next version.

#### NEW FEATURES

1. `classify_poi()`. New function capable of attributing a class of choice to any points that meet a logical criterion (e.g. Z > 2) and/or a spatial criterion (e.g. inside a polygon). For example, the following will attribute the class "high vegetation" to each non-ground point that is not in the lake polygon.
    ```r
    las <- classify_poi(las, LASHIGHVEGETATION, poi = ~Classification != 2, roi = lakes, inverse = TRUE)
    ```

2. `LAScatalog`
   - New function `rbind()` for `LAScatalog`.
   - New functions `projection()<-` and `crs()<-` for `LAScatalog`. Those two functions were already working in previous versions but in absence of dedicated functions in lidR the functions that were actually called were `raster::projection()` and `raster::crs()` thanks to class inheritance. However the functions from `raster` do not support `crs` from `sf` or numbers as input. Adding a dedicated function in lidR brings consistency between `LAS` and `LAScatalog` ([#405](https://github.com/Jean-Romain/lidR/issues/405)):
      ```r
      projection(ctg) <- st_crs(3625)
      # or
      projection(ctg) <- 3625
      ```
   - The processing engine has a new option to drop some chunks under `ctg@chunk_options$drop`. This generates regions that won't be processed. This option accepts a vector of chunk IDs that are dropped and is thus versatile, but its main role is to allow restarting a computation that failed. We consequently introduced the function `opt_restart()`. Let's assume that the computation failed after few hours at 80% in chunk number 800. Users get a partial output for the first 799 chunks but chunk 800 has a problem that can be solved. It is now possible to restart at 800 and get the second part of the output without restarting from 0:
      ```r
      output <-    catlog_apply(ctg, myfun, param)
      # Failed after 80%, 'output' contains a partial output
      # Fix the trouble
   
      opt_restart(ctg) <- 800
      output2 <- catlog_apply(ctg, myfun, param)
   
      # Merge 'output' and 'output2'
      ```
   - The vignette `LAScatalog engine` and the manual `LAScatalog-class` were updated to reflect these features

3. `LASheader`
   - The function `LASheader()` can now create a `LASheader` object from a `data.frame`. This addition aims to facilitate the creation of valid `LAS` objects from external data.
   - `las_check()` can now check a standalone `LASheader`
       ```r
       las_check(las@header)
       ```
   
4. `LAS`
   - The function `LAS` now automatically fixes the font case of attributes names to match the naming convention of the `rlas` package. This simplifies the creation of compatible objects from non-LAS file sources.
       ```r
       data <- data.frame(x = runif(10), Y = runif(10), z = runif(10), pointsourceid = 1:10)
       las <- LAS(data)
       #> Attribute 'x' renamed 'X' to match with default attribute names.
       #> Attribute 'z' renamed 'Z' to match with default attribute names.
       #> Attribute 'pointsourceid' renamed 'PointSourceID' to match with default attribute names.
       las$PointSourceID
       #> [1]  1  2  3  4  5  6  7  8  9 10
       ```
   
5. Full waveform: with most recent versions of the `rlas` package, full waveform (FWF) can be read and `lidR` provides some compatible functions. However the support of FWF is still a work in progress in the `rlas` package. How it is read, interpreted and represented in R may change. Consequently, tools provided by `lidR` may also change until the support of FWF becomes mature and stable in `rlas`.
   - New function `interpret_waveform()` to transform waveform into a regular point cloud
   - New supported flag `W` for parameter `select` in `readLAS()`
   - New automatic colouring scheme for attribute `Amplitude` in `plot(las, color = "Amplitude")` that aims to be used with FWF.
   
6. `catalog_intersect()` now supports `sf`, `sfc`, `Extent` and `bbox` objects

7. Concave hull: lidR now includes its own C++ code to compute concave hulls using [concaveman-cpp](https://github.com/sadaszewski/concaveman-cpp).
   - New function `concaveman()` to compute concave hulls
   - `delineate_crowns()` using concave hulls is now between 10 to 50 times faster.
        ```r
        LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
        las = readLAS(LASfile, select = "xyz0")
        concave_hulls <- delineate_crowns(las, "concave")
        # Before v3.2.0: 7.1 seconds
        # From v3.2.0  : 0.2 seconds
        ```
   - `grid_terrain()` with `is_concave = TRUE` should also be faster.
    
8. New function `catalog_boundary()` to compute the actual shape of the point-cloud

9. In `find_trees()` and `segment_trees()` the `bitmerge` strategy to generate robust unique IDs was not actually a valid and robust procedure. It had the advantage of generating integers but was not 100% unique. The probability to generate duplicates was low but we changed the strategy to use a true bit-merging procedure anyway. The new IDs thus generated are weird decimal number such as 5.001120e-310 but are guaranteed to be unique. The documentation has been updated to explain the method.

10. New algorithm `random_per_voxel()` for `decimate_points` that keep n points per voxel ([#406](https://github.com/Jean-Romain/lidR/issues/406)).

11. 3D rendering: 
    - `plot()` gains a new parameter `voxels = TRUE` or `voxels = 0.5` to render a point cloud with voxels. This is useful to render the output of `voxelize_points()` or `voxel_metrics()`, for example. This is computationally demanding and takes time so it should be reserved to small scenes with 30,000 or 40,000 voxels maximum, but note that there is no hard coded limit.
      ```r
      vm <- voxel_metrics(las, ~list(N = length(Z)), 8)
      plot(vm, color = "V1", voxels = T)
      ```
    - specular reflections are now disable in `plot()`.
    
12. New function `plot_metrics()` that wraps several other functions into one seamless function that extracts ground inventory plots, computes metrics for each plot and returns a ready to use `data.frame` for statistical modelling.

13. New function `point_eigenvalue()` that is equivalent to `point_metrics(las, .stdshapemetrics)` but specialized, optimized and parallelized to be 10 times faster.

14. `grid_metrics()` gains a new parameters `by_echo` allowing users to compute the metrics for different types of echos independently. It is now possible to map e.g. `mean(Intensity)` for first returns only + multiple return only + single return only. All metrics are computed in a single run and returned in a raster stack.

15. `merge_spatial()` supports `sfc`

#### ENHANCEMENTS

1. `grid_density()` is 10 times faster

#### FIXES

1. Fix: `quantize()` now preserves `NaN` values instead of converting them into minus infinity ([#460](https://github.com/Jean-Romain/lidR/issues/460)).
2. Fix: `stdmetrics_i()` now fails with an informative message when the sum of intensities is greater than `.Machine$integer.max` and becomes `double` ([#463](https://github.com/Jean-Romain/lidR/issues/463))
3. Fix: `find_localmaxima()` respects the `filter` argument. It was previously not considered.
   
#### MISCELLANEOUS

1. Remove `crayon` and `hexbin` dependencies
2. Packages `RCSF` and `rgeos` are now only suggested and they are consequently no longer installed by default with lidR
3. Change: `rgdal` will be retired in 2024. Code using `rgdal` internally has been removed. In many cases this will not change anything for users but in some cases it may fail when assigning an EPSG code to the LAS file. Also, old versions of `rgdal` built with old versions of `gdal` and `proj` are no longer supported ([#466](https://github.com/Jean-Romain/lidR/issues/466))

## lidR v3.1.4 (Release date: 2021-06-22)

- Change: `manual()` now uses the middle button to perform the selection. Historically the button was "right" but later the right button was added in lidR and attributed to the dragging action. By using "right" in this function this disabled the possibility to drag the scene. Consequently we changed the default to use the middle button. ([#442](https://github.com/Jean-Romain/lidR/issues/442)).
- Change: `manual()` now removes all apices in the selection rectangle when removing some false positive ([#445](https://github.com/Jean-Romain/lidR/issues/445)).
- Doc: fix some code block rendering in `catalog_apply` man page
- Fix: fix catalog processing engine edge case when the last chunks fail ([#435](https://github.com/Jean-Romain/lidR/issues/435)).
- Fix: `voxel_metrics()` with `all_voxels = TRUE` did not work as expected. The insertion of empty voxels corrupted some of the real voxels. This bug lead to invalid output and some floating points precision errors lead to supernumerary voxels ([#437](https://github.com/Jean-Romain/lidR/issues/437), [#439](https://github.com/Jean-Romain/lidR/issues/439)).
- Fix: `grid_terrain()` used with a `LAScatalog` no longer propagated the options. For example when using `use_class = c(2L, 8L, 9L, 10L)` this was not propagated and the option was actually the default one i.e. `use_class = c(2L, 9L)`. This bug was introduced in 3.1.0
- Fix: `delineate_crowns()` now returns `NULL` if the input point-cloud has only points with treeID = NA. It also triggers a warning. ([#438](https://github.com/Jean-Romain/lidR/issues/438)).
- Fix: `manual()` the function that allow for finding the trees manually was no longer working probably because of some slight modifications in the `rgl` package.
- Enhance: the `plot` function used to display the output of `voxel_metrics()` now internally uses the same function than `LAS` objects. This enhances the rendering using the `clear_artifact` option by default and allows for a lot more flexibility in the rendering.
- Enhance: new parameter `button` in `manual()` to choose which button to use.
- Enhance: `segment_trees()` now print a message if all points are `NA` to suggest to use other parameters

## lidR v3.1.3 (Release date: 2021-05-20)

- Fix: `las_check(..., deep = TRUE)` was not working in parallel ([#411](https://github.com/Jean-Romain/lidR/issues/411)).
- Fix: the LAScatalog processing engine printed the outputs twice for rare functions that print something like `las_check()`  ([#414](https://github.com/Jean-Romain/lidR/issues/414))
- Fix: the internal way lidR is checking for nested parallelism has been reworked in depth fixing some bugs and allowing to support more strategies thanks to @Lenostatos  ([#418](https://github.com/Jean-Romain/lidR/issues/418), [#421](https://github.com/Jean-Romain/lidR/issues/421))
- Fix: `merge_spatial()` did not work with `sf` objects.
- New: `las_check()` introduces a new type of message called "message". Some message previously classified as "warning" are now classified as "message". Warnings are now displayed in orange and messages in yellow. The output of `las_check()` has now 3 items instead of 2.
- New: `stdmetrics_z` gains a new parameter `zmin = 0` to control the lower bound of the integration for metrics `zpcumx` ([#424](https://github.com/Jean-Romain/lidR/issues/424)).
- Enhance: `max_cr_factor` in `silva2019()` is now allowed to be in [0, inf[ instead of [0,1] ([#417](https://github.com/Jean-Romain/lidR/issues/417))
- Enhance: added a workaround to avoid `sp` printing `proj_create: crs not found` for non recognized EPSG codes and avoid throwing warning `Discarded datum [...] in Proj4 definition`
- Enhance: `readLAScatalog()` throws a more informative error when attempting to read an non-existing folder.
- Enhance: `readXXXLAS()` now throws an error for `LAScluster` ([#430](https://github.com/Jean-Romain/lidR/issues/430)).
- Doc: Updates and clarifications in the doc of `stdmetrics`.
- Misc: removed `LazyData` in `DESCRIPTION`

## lidR v3.1.2 (Release date: 2021-03-11)

- New: the class `LASheader` has a new slot `@EVLR` for the extended variable length records. `print()` has been extended to display EVLR. While this change is compatible with `rlas <= 1.3.9` it is only used with version of `rlas >= 1.4.0`. 
- New: algorithm `lowest()` for `decimate_points()`
- Fix: usban outside the range of representable values of type 'char' for spatial indexes built with 0 point.
- Fix: build failure with GCC 4.x
- Fix: `catalog_apply()` now works with cluster plan `plan(cluster)` meaning that it can be used on HPC e.g. with MDPI. We took advantage of this bug to better detect the parallel strategy used and disable or not OpenMP. When `lidR` is not able to figure out if the strategy involves multiple machines or multiple cores of a single machine, then a warning is thrown and OpenMP is disabled by security.
    ```
    The parallel evaluation strategy was no recognized and lidR does not know if OpenMP should be disabled.
    OpenMP has been disabled by security. 
    Use options(lidR.check.nested.parallelism = FALSE) and set_lidr_threads() for a fine control of parallelism.
    ```
- Fix: incorrect offset computation in `spTransform()` have for consequences to make the function failing with error: `Non quantizable value outside the range of representable values of type 'int'`.
- Fix: attribution of a WKT string with `projection()` when using an epsg code as input (`projection(las) <- 12345`).
- Fix: partial processing mode now respects the raster alignment when processed by file
- Fix: `readLAScatalog()` now reads the WKT CRS of LAS files format 1.4. To support both EPSG and WKT the table of attribute of a `LAScatalog` now has a column named `CRS` that replace former column `EPSG`.
- Fix: `print()` for a `LAScatalog` now prints the CRS exactly like `print` for `LAS`.
- Doc: documentation of `options(lidR.check.nested.parallelism = FALSE)` was missing. Information can now be found in `?lidR-package` and `?lidR-parallelism`
- Enhance: in `catalog_apply()` if `lidR.check.nested.parallelism = FALSE` it now respects the input of `set_lidr_thread()` instead of the output of `get_lidr_threads()`. For example if `set_lidr_thread(0)` it now propagates the information 0 (all cores) instead of the output of `get_lidr_thread()` which might be e.g. 4 on the master worker but might be different on the slave workers. Similarly `set_lidr_thread(20)` will request 20 cores to the workers even if `get_lidr_thread()` returns 4 on the local machine.
- Enhance: `set_lidr_thread()` accepts inputs < 1 such as 0.5 or 0.25 to mean 'half' or 'quarter' of available cores.
- Enhance: `grid_density()` now returns 0 for pixels with 0 points instead of `NA` which make more sense and corresponds to what should be expected.

## lidR v3.1.1 (Release date: 2021-01-22)

- Fix usban issue: outside the range of representable values of type 'int' for spatial indexes built with 0 point.
- Fix usban issue: outside the range of representable values of type 'int' when quantizing or counting non quantized values that are not quantizable according the the given scale and offset.
- Remove lax files in example data.

## lidR v3.1.0 (Release date: 2021-01-15)

#### MAJOR NEW FEATURES

The release of `lidR` 3.1.0 comes with major internal modifications enabling users to chose different kinds of spatial indexes to process the point-clouds, including Quadtrees and Octrees, plus others. Previous releases were optimized to process ALS data but were suboptimal for TLS data (for example) because the spatial index in use was specialized for ALS. With 3 new spatial indexes, version 3.1.0 brings the capability to process TLS (but not only) data more efficiently. For the time being, however, `lidR` is still mainly focused on ALS and does not include many functions for TLS processing, but the existing functions that be used on all kinds of point-cloud, such as `point_metrics()`, `detect_shape()`, and `classify_noise()` are already much faster for TLS data.

1. The class `LAS` has a new slot `@index` that registers the source of the point cloud (e.g. ALS, TLS, UAV, DAP) and the spatial index that must be used (e.g. grid partition, voxel partition, quadtree, octree). See `help("lidR-spatial-index")`.
2. This comes with several new `read*LAS()` functions, such as `readTLSLAS()`, which registers the point-cloud type and a default spatial index. Registering the correct point type improves the performance of some functions. This is particularly visible in functions that perform 3D knn searches, such as `point_metrics()`. Computing `point_metrics()` on a TLS point-cloud tagged as TLS is much faster than if it is not tagged. If performance is not improved in this release the future versions of the package may bring enhancements transparently.
3. New functions `index()` and `sensor()` to manually modify the spatial indexing-related information. `help("lidR-spatial-index")`.
4. New C++ API: the C++ classes for spatial indexing are header-only and stored in `inst/include`, meaning that other packages can link to `lidR` to uses the spatial index at C++ level. The classes are not documented yet but the source code is simple and commented, and the [lidR book](https://jean-romain.github.io/lidRbook/) contains (or will contain) a chapter on spatial indexing.

#### CHANGES

1. The use of old deprecated namespaces (such as `lassomething()`) now triggers a message inviting users to move on the new namespace.
2. The construction of a `LAS` object with `LAS()` now triggers warnings with incorrectly quantized coordinates according to the information in the header.
3. `grid_terrain()` now has a parameter `...` after `algorithm` that invalidates code that uses too many parameters without naming them. This no longer works:
```r
grid_terrain(las, 1, tin(), TRUE, TRUE, 8)
# Use instead
grid_terrain(las, 1, tin(), keep_lowest = TRUE, full_raster = TRUE, use_class = 8)
```
4. `opt_cores()` and `opt_cores<-()` are now defunct. These functions did not have any effect because they only throw a warning to alert about deprecation since v2.1.0 (July 2019).
5.  The `LAS*` classes have a new slot `@index` (see above). This should not break anything expect when a `LAS*` object is saved in an `Rds` file and loaded as an R object instead of being read with `readLAS`.
 
#### NEW FEATURES

1. `classify_noise()`
    - New function `classify_noise()` to classify the outliers of a point-cloud according to ASPRS standard
    - New algorithm `sor()` (statistical outlier removal) for noise classification
    - New algorithm `ivf()` (isolated voxel filter) for noise classification

2. Quantization of the coordinates. `LAS` objects in `lidR` closely respect the ASPRS standard. When modified manually by users, some inadequate practices may generate invalid LAS objects. We thus decided to export some internal functions to help in creating valid LAS objects and we modified the behavior of the `[[<-` and `$<-` operators to ensure that it is more difficult to create `LAS` objects that are not ASPRS compliant.
    - New functions `las_quantize()`, `quantize()`, `is.quantized()`, `count_not_quantized()` to ensure that coordinates are quantized according to the metadata in the header.
    - New function `las_update()` to update the header (bounding box, number of points, return count and so on) if a LAS object was modified outside a `lidR` functions.
    - Enhanced behaviour of `[[<-` and `$<-` operators. Values are quantized on-the-fly and the header is updated automatically when attributing new values to `X`, `Y` or `Z`.
    ```r
    las$X # Original values
    #> [1] 0.755 0.286 0.100 0.954 0.416 0.455 0.971 0.584 0.962 0.762
    las$X + 5/3 # Many decimals because 5/3 = 1.666666...
    #> [1] 2.421667 1.952667 1.766667 2.620667 2.082667 2.121667 2.637667 2.250667 2.628667 2.428667
    las$X <- las$X + 5/3 # Updates X with these numbers
    las$X # Values were quantized (and header updated)
    #> [1] 2.422 1.953 1.767 2.621 2.083 2.122 2.638 2.251 2.629 2.429
    ```
    - New manual page can be found in `help("las_utilities")`.
    
3. metrics
    - `voxel_metrics()` gained a parameter `all_voxels` to include "empty" voxels (i.e. those with 0 points) in the output [#375](https://github.com/Jean-Romain/lidR/issues/375).
    
4. `grid_terrain()`
    - new parameter `...` after `algorithm` that invalidates code that uses too many parameters without naming them. This no longer works:
    ```r
    grid_terrain(las, 1, tin(), TRUE, TRUE, 8)
    # Use instead
    grid_terrain(las, 1, tin(), keep_lowest = TRUE, full_raster = TRUE, use_class = 8)
    ```
    - new parameter `is_concave` to compute a nicer DTM if the point-cloud boundaries are not convex [#374](https://github.com/Jean-Romain/lidR/issues/374)
     
### FIXES

1. In `clip_transect()` the polygon generated to extract the transect defined by points `p1`, `p2` was created by buffering the line `p1-p2` with a `SQUARE` cap style meaning that the transect was extended beyond points `p1`, `p2`. It now uses a `FLAT` cap style meaning that the transect is no longer extended beyond the limits of the user input.
2. In `segment_trees()` when using a raster-based algorithm, some points may have been misclassified as NAs at the edges of the point cloud instead of getting the correct tree ID found in the raster because of some edge effects. Now, all points are correctly classified and there are no longer false positive NAs.
3. `normalize_intensity()` was previously not working with a `LAScatalog`. Now fixed. See [#388](https://github.com/Jean-Romain/lidR/issues/388)
4. In `grid_*()` functions when a `RasterLayer` is given as layout, the computation was performed for all the cells no matter if the extent of the loaded point-cloud was much smaller than the raster. For large rasters this dramatically increased the workload with redundant computation and saturated the RAM to a point that the computation was no longer possible. 
5. In `track_sensor()` pulse IDs could be wrongly attributed for multi-beam sensors if the number of points is very low. See [#392](https://github.com/Jean-Romain/lidR/issues/392)
6. In `track_sensor()`, if `thin_pulses_with_time = 0` a single pulse was loaded with a `LAScatalog`. However it worked as expected with a `LAS` object. This behavior has been fixed.
7. Fixed some new warnings coming from `future` and related to RNG.
8. `clip_*()` in a region with no points from a `LAScatalog` + an output file no longer fails. See [#400](https://github.com/Jean-Romain/lidR/issues/400).

### ENHANCEMENTS

* Doc: The documentation of `point_metrics()` clarifies how the user-defined function is fed and in which order the points are sorted.
* Doc: The argument `Wdegenerated` in `grid_terrain()` and `normalize_height()` was misleading. A wrong interpretation was that degenerated ground points were discarded from the dataset. The documentation now clarifies the text to avoid misinterpretation.
* Doc: minor fixes and clarifications in the `LAScatalog-class` page of the manual.
* Enhance: `plot_dtm3d()` now enables pan by default, like `plot()` for `LAS` objects.
* Enhance: `track_sensor()` throws a new warning if a swath in the point cloud does not produce any sensor location. This addresses [#391](https://github.com/Jean-Romain/lidR/issues/391).
* Misc: switch to C++14 (see [#402](https://github.com/Jean-Romain/lidR/issues/402))

## lidR v3.0.4 (Release date: 2020-10-08)

* Fix: in `readLAScatalog()` the documentation states that `...` is propagated to `list.files()`, but the argument `pattern` was actually hard coded internally and this prevents it being overwritten. When using `readLAScatalog(..., pattern = "xxx")` this previously triggered an error, `formal argument "pattern" matched by multiple actual arguments`. It now works. See [#368](https://github.com/Jean-Romain/lidR/issues/368).
* Fix: in `spTransform()` the reprojected point cloud now has quantized coordinates and is thus LAS compliant [#369](https://github.com/Jean-Romain/lidR/issues/369).
* Fix: The local maximum filter algorithm more robustly finds local maxima when two or more close points or pixels share the exact same height and are both locally the highest. Previously, if two or more points in a close neighbourhood were both the highest, they may all be missed depending on the order they were processed (which is somewhat random). The fix allows users to retain one local maximum among multiple ones with a precedence to the first one identified as local maximum. The consequences of this fix are that slightly more apices may be found, especially when processing a CHM in `RasterLayer`.
* Fix: `classify_ground()` no longer erases the previous classification when no ground points were recorded but some points are classified with other classes.
* Fix [#365](https://github.com/Jean-Romain/lidR/issues/365). Poor interpolation at the very edge of the Delaunay triangulation in some cases. Triangles with too steep a slope are now removed. This triggers a knnidw interpolation instead.
* Fix [#371](https://github.com/Jean-Romain/lidR/issues/371): `las_reoffset()` may not have caught extremely rare Z coordinate overflow when converting to integers.
* Fix [#372](https://github.com/Jean-Romain/lidR/issues/372). `las_reoffset()` incorrectly converted decimal coordinates to integers using `trunc` instead of `round`.
* Fix: `projection<-()` and `crs<-()` properly attributes `NA` CRS for LAS 1.4 objects
* Change: in `print` the CRS of `LAS` and `LAScatalog` is no longer displayed as a proj4 string but uses the WTK string with `sf` style display. E.g. `NAD83 / UTM zone 17N` is displayed instead of `+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs`. This is part of the migration toward WTK instead of proj4.
* Change: `lidR` now explicitly depends on `rgdal >= 1.5.8`.
* Change: `grid_canopy()` now rounds the values of the pixels for not outputing pixels that with an irrelevant number of decimal digits.
* Enhance: `epsg()` now throws a warning if the LAS is in format 1.4 and CRS is stored as WKT.
* New: `projection()<-` supports `crs` from `sf` and numeric values for espg code: `projection(las) <- 26918`.
* New: in `spTransform()` it is now possible to use a parameter `scale` to change the scale factor after reprojection. This is useful for projecting from lon-lat data `las2 = spTransform(las, crs, scale = 0.01)`.
* Internal: better support in `projection<-` of the current changes with CRS representation in the R spatial ecosystem.
* Doc: new CITATION file. `citation("lidR")`

## lidR v3.0.3 (Release date: 2020-08-05)

* New: `tin()` gains a parameter `extrapolate` to control how the method treats interpolation of points outside the convex hull determined by ground points. This solves [#356](https://github.com/Jean-Romain/lidR/issues/356) 
* Doc: supported processing options in `grid_terrain()` were incorrect especially the buffer that *is* required. 
* Doc: in `Wing2015()` the mention about weak performance was removed since it was not longer true for a while.
* Doc: clarification of the supported templates in man page named `clip`
* Enhance: a more informative error is thrown when using `{ORIGINALFILENAME}` as a template in `clip_*()`.
* Misc: fix C++ error that will happen in next version of `Rcpp` ahead of the release of `Rcpp`. Thanks to @waltersom in [#358](https://github.com/Jean-Romain/lidR/pull/358)

## lidR v3.0.2 (Release date: 2020-07-05)

* Fix: in `grid_metrics()` and `grid_canopy()` when processing a `LAScatalog` the option to process by files without buffer and disabling the wall-to-wall guarantees (processing independant filles) is now repected. [See also](https://gis.stackexchange.com/questions/365686/how-to-exclude-bounding-boxes-of-plots-in-lascatalog). 
* Fix: in `grid_metrics()` NA pixels were zeroed. They are now properly initialized to NA.

## lidR v3.0.1 (Release date: 2020-06-18)

* Fix: in `grid_terrain()` and `normalize_height()` we introduced few releases ago an option `use_class` but we did not removed an internal test consisting in failling in absance of point classified 2. This invalidated the possibility to use e.g. `use_class = 1` in files that do not respect ASPRS standards [#350](https://github.com/Jean-Romain/lidR/issues/350).
* Fix: many troubles introduced in v3.0.0 on CRAN
* Fix: package explicitly depends on sp >= 1.4.2
* Fix: `readLAS(filter = "-help")` was not working but was suggested in the documentation.

## lidR v3.0.0 (Release date: 2020-06-08)

#### MAJOR CHANGES

**Summary** 

In lidR version 3.0.0, 80% of the functions were renamed. Old functions were soft-deprecated, meaning that they still exist so version 3 is fully compatible with version 2, at least for 1 year. Users should start to use the new names. See `?lidR::deprecated` for the list of deprecated functions and their new names. The plan is to remove these functions in 1 year so they will progressively print a message, then throw a warning, then throw an error, after which they will be definitively removed.

**Full explanation**

At the very beginning of the development of lidR we started to name the functions that return a LAS object `lassomething()`. At that point there were 5 functions and ~10 users. As lidR grew up, we kept going with this naming convention but now lidR is used worldwide and this naming convention now overlaps with the LAStools software suite created by Martin Isenburg. This creates confusion for users which is problematic both for Martin and for us. This situation is likely to get worse as more tools are released into LAStools. We discussed the issue with Martin Isenburg and we took the decision to rename the functions in the lidR package so that the overlaps in namespace will progressively disappear.

The new naming convention follows the currently trending `verb_noun` syntax initiated by the `tidyverse`. For example, `lasnormalize()` becomes `normalize_height()`, while `lasground()` becomes `classify_ground()`. The full list of changes can be found in `?lidR::deprecated`.

In efforts to avoid breaking users' scripts version 3 is fully backwards-compatible. For example, the function `lasground()` still exists and can be used without throwing a warning or error message. But this will progressively change with versions 3.1.0, 3.2.0 and 3.3.0. First a message will be displayed to invite users to change to using the new names, then a warning, then finally an error. After a year, maybe 18 months, the function will no longer exist. So users are invited to adopt the new naming convention as soon as possible.

#### NEW FEATURES

1. `readLAScatalog()` has new parameters to tune the processing options at read time without using the functions `opt_*()`.

    ```r
    readLAScatalog("folder/", chunk_buffer = 60, filter = "-drop_z_below 2")
    ```

2. New function `clip_transect()` to extract a transect between two points. The function has the capability to reorient the point cloud to put it on XZ coordinates and easily create some 2D rendering of the transects in e.g. `ggplot2.`

3. New function `readMSLAS()` to read multisprectral data from 3 different files.

    ```r
    readMSLAS("channel1.las", "channel2.las", "channel3.las", filter = "-keep_first")
    ```

4. `delineate_crowns()` (formerly named `tree_hulls()`) now returns 3 metrics: `XTOP`, `YTOP` and `ZTOP`, that contain the coordinates of the apices of the trees.

5. `segment_trees()` (formerly named `lastrees()`) and `find_trees()` (formerly `tree_detection()`) can now perform the computation on a `LAScatalog` using two strategies to ensure that tree IDs are always unique on a coverage and that trees that belong on the edge of two tiles will independently get the same IDs.

6. `point_metrics()` 
    - supports a knn neighborhood search with missing `r` and given `k`
    - supports a spherical neighborhood search with missing `k` and given `r`
    - supports a knn neighborhood + a radius limit with `k` and `r` given
    - default setting is now `xyz = FALSE`
    - if `xyz = FALSE` the the output now contains a column (the first one) named `pointID` that references the point of the original las object. See [#325](https://github.com/Jean-Romain/lidR/issues/325)

7. `normalize_height()` (formerly named `lasnormalize()`)
    - new argument `add_lasattribute`. If `TRUE` the absolute elevation (above sea level) is retained as before, but the header is updated so the absolute elevation becomes an extrabyte attribute writable on a las file. Otherwise the information is discarded at write time.
    - new argument `Wdegenerated`. If `FALSE` the function does not warn about degenerated points. Degenerated points are removed anyway.

8. New function `find_localmaxima()` to find local maxima with different windows. This function is designed for programming purposes, not to find individual trees. This latter task is still performed by `find_trees()` (formerly called `tree_detection()`). Instead, `find_localmaxima()` may help with finding other human-made structures.

9. Internal global variables were exported to help with ASPRS LAS classification standard. Instead of remembering the classification table of the specification it is now possible to use one of `LASNONCLASSIFIED`, `LASUNCLASSIFIED`, `LASGROUND`, `LASLOWVEGETATION`, `LASMEDIUMVEGETATION`, `LASHIGHVEGETATION`, `LASBUILDING`, `LASLOWPOINT`, `LASKEYPOINT`, `LASWATER`, `LASRAIL`, `LASROADSURFACE`, `LASWIREGUARD`, `LASWIRECONDUCTOR`, `LASTRANSMISSIONTOWER`, `LASBRIGDE`, `LASNOISE`. e.g.:

    ```r
    filter_poi(las, !Classification %in% c(LASWIRECONDUCTOR, LASTRANSMISSIONTOWER))
    ```

10. The internal function `catalog_makechunks()` has been exported. It is not actually intended to be used by regular users but might be useful in some specifc cases for debugging purposes.

11. `lasmetrics()`, `grid_metrics3d()`, `grid_hexametrics()` were deprecated in previous versions. They are now defunct.

12. `las_check()` (formerly named `lascheck()`):
    - gains an option `print = FALSE`. 
    - now returns a `list` for further automatic processing/parsing. If `print = TRUE` the list is returned invisibly so the former behavior looks the same.
    ```r
    las_check(las, FALSE)
    #> $warnings
    #> [1] "1 points are duplicated and share XYZ coordinates with other points"                                         
    #> [2] "There were 1 degenerated ground points. Some X Y Z coordinates were repeated."                               
    #> [3] "There were 1 degenerated ground points. Some X Y coordinates were repeated but with different Z coordinates."
    #> 
    #> $errors
    #> [1] "Invalid header: X scale factors should be factor ten of 0.1 or 0.5 or 0.25 not 0.123"                      
    #> [2] "Invalid file: the data contains a 'gpstime' attribute but point data format is not set to 1, 3, 6, 7 or 8."
    ```
    - gains an option `deep = TRUE` with a `LAScatalog` only. In this case it performs a deep inspection of each file reading each point cloud.
    - the coordinates of the points are expected to be given with a resolution e.g. 0.01 meaning a centimetre accuracy. It means we are expecting values like 12345.67 and not like 12345.6712. This is always the case when read from a LAS file but users (or lidR itself) may transform the point cloud and generate LAS objects where this rule is no longer respected. `lidR` always ensures to return `LAS` objects that are stricly valid with respect to ASPRS standard. If not valid this may lead to failure in `lidR` because some functions, such as `tin()`, `dsmtin()`, `pitfree()` work with the integer representation of the coordinates. This is why we introduced a quantization check in `las_check()`.
    - now reports problems for invalid data reported in [#327](https://github.com/Jean-Romain/lidR/issues/327)
    
13. `merge_spatial()` (formerly named `lasmergespatial()`) now supports `sf` POLYGON objects.

14. `plot()` 
    - for LAS object gains an argument `add` to overprint two point clouds with e.g. different color palettes [#325](https://github.com/Jean-Romain/lidR/issues/325).
    ```r
    las = readLAS("classified.las")
    nonveg = filter_poi(las, Classification != LASHIGHVEGETATION)
    veg = filter_poi(las, Classification == LASHIGHVEGETATION)
    x = plot(nonveg, color = "Classification")
    plot(veg, add = x)
    ```
    - for LAScatalog objects gains an argument `overlaps = TRUE` to highlight the overlaps.

15. New function `add_lasrgb()` to add RGB attributes. The function updates the header in such a way that the LAS object has a valid point format that supports RGB.

16. `LAScatalog` processing engine
    - gains a generic option `opt_merge(ctg) <- FALSE` to disable final merging and force the engine to return a list
    - gains a generic option `opt_independent_files(ctg) <- TRUE` to set adequate options to a collection of independent files, for example a set of circular ground inventory plots. It is equivalent to set no buffer, processing by file and no wall-to-wall guarantee.
    - gains an option `autoread = TRUE` in `catalog_apply()`. Not actually intended to be used widely but might be convenient for some use cases.

17. New function `get_range()`.

18. `knnidw()` gains an argument `rmax` to set a maximum radius search in which to find the knn. This fixes computation time issues with non-convex point clouds.

19. `track_sensor()` (formerly `sensor_tracking()`) 
    - now has two available algorithms.
    - supports systems with multiple pulses emission which formerly failed

20. `writeLAS()` gains a parameter `index = TRUE` to automatically write a lax file along with the las/laz file.

#### ENHANCEMENTS

1. `readLAS()` now warns when reading incompatible files. Point coordinates are recomputed on-the-fly as it has always been done but now the user is aware of potential problems or precision loss.

2. A new vignette named **LAScatalog processing engine** has been added and documents in-depth the `catalog_apply()` engine of lidR.

3. In `clip_*()` several lines of codes were removed because they were not used. We suspected these lines covered old cases from lidR v1.x.y that are no longer relevant. If a user encounters problems, please report.

4. The arguments `select` and `filter` from `readLAS()` are not expected to be used with a `LAScluster` when processing a `LAScatalog`. The options are carried by the `LAScatalog` itself with `opt_select()` and `opt_filter()`. If used, a warning is now thrown.

5. Enhancements made here and there to improve the support of the CRS when reading and checking a LAS file.

6. When processing by file with a raster output, automatic chunk extension to match with a raster resolution now performs a tighter extension.

7. Minor modification of `print()` methods to enhance information displayed.

8. All algorithms such as `tin()`, `p2r()`, `knnidw()`, `li2012()`, and so on, now have the classes `c("lidRAlgorithm", "something")` and a dedicated print function. The source code is no longer displayed when printing these objects
    
    ```r
    f = lmf(2)
    f
    #> Object of class lidR algorithm
    #> Algorithm for: individual tree detection 
    #> Designed to be used with: find_trees 
    #> Native C++ parallelization: yes 
    #> Parameters: 
    #>  - circ = TRUE <logical>
    #>  - hmin = 2 <numeric>
    #>  - shape = circular <character>
    #>  - ws = 2 <numeric>
    ```

9. In `grid_metrics()` the `RasterBrick` is built much faster.

#### FIXES

1. In `delineate_crowns()`, formerly named `tree_hull()`, when applied to a `LAScatalog` the buffer was not properly removed. The polygons were simply clipped using the bounding box of the chunk. Now the trees that have an apex in the buffer are removed and the trees that have an apex outside the buffer are retained. Thus, when merging, everything is smooth and continuous.

2. `catalog_retile()` returns a `LAScatalog` with only the newly created files even if the folder contains other las files. It formerly read every las file in the folder leading to an invalid catalog if the folder already contained las files.

3. Previously in automatic filename generation the template `YCENTER` was not actually recognized. However, `XCENTER` was recognized but actually contained the value for `YCENTER`. This was working for `lasclip()` thanks to a previous fix but was still a problem in other functions when processing chunks.

4. Function `wkt()` no longer masks the new function `wkt()` in `sp`.

5.  `merge_spatial()` (formerly named `lasmergespatial()`) no longer fails with a LAS object containing a single point.

## lidR v 2.x.y

lidR v2.x.y changelog has been moved to [NEWS_v2.md](https://github.com/Jean-Romain/lidR/blob/master/NEWS_v2.md)
