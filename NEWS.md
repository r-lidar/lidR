If you are viewing this file on CRAN, please check [the latest news on GitHub](https://github.com/Jean-Romain/lidR/blob/master/NEWS.md) where the formatting is also better

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
