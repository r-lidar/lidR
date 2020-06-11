If you are viewing this file on CRAN, please check [the latest news on GitHub](https://github.com/Jean-Romain/lidR/blob/master/NEWS.md) where the formatting is also better.

## lidR v3.0.1

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

## lidR v2.2.5 (Release date: 2020-05-07)

#### ENHANCEMENTS

1. Clear unrelevant message about OpenMP support when using the LAScatalog processing engine with a version of `lidR` not compiled with OpenMP support (i.e. on MacOS)

## lidR v2.2.4 (Release date: 2020-04-24)

#### FIXES

1. Fix segfault on Windows 64 bits when constructing a proj4 from some specific modern WTK strings using `doCheckCRSArgs =  FALSE`. [#323](https://github.com/Jean-Romain/lidR/issues/323) [sp #75](https://github.com/edzer/sp/issues/75)

2. Fix wrong gpstime matching in `lasrangecorrection()` at the edge of flightlines [#327](https://github.com/Jean-Romain/lidR/issues/327).

3. Fix error when building the clusters with a partial processing and a realignment [#332](https://github.com/Jean-Romain/lidR/issues/332).

4. Fix error in `lasclip()` and `lasmergespatial()` with `sf` objects when the coordinates are not stored in a column named `geometry`. Thank to Michael Koontz in [#335](https://github.com/Jean-Romain/lidR/issues/335).

5. `lasrangecorrection()` no longer mess-up the original sensor data. See [#336](https://github.com/Jean-Romain/lidR/issues/336)

#### ENHANCEMENTS

1. Enhancements made here and there to improve the support of the CRS when reading and checking a LAS file.

2. `crs not found` message is no longer displayed when building a LAS object. This message appeared with an update of `rgdal` or `sp`. It is now gone.

3. `sensor_tracking()` now throws an error for the invalid case reported in [#327](https://github.com/Jean-Romain/lidR/issues/327)

4. `lascheck()` now reports problems for invalid data reported in [#327](https://github.com/Jean-Romain/lidR/issues/327)

5. `grid_metrics()` returns a raster full of NAs instead of failing if a `RasterLayer` is given as a layout but this layer does not encompase the point cloud

6. `opt_output_file()` now applies tilde-expansion to the path.

7. When processing by file with an raster output, automatic chunk extension to match with a raster resolution now perform a tighter extension.

## lidR v2.2.3 (Release date: 2020-03-02)

#### FIXES

1. This fix breaks backward compatibility. In `catalog_apply()` if `automerge = TRUE` and the output contains a `list` of strings the list was expected to be merged into a character vector. But actually, the raw list was returned, which was not the intended behavior. This appends with `Spatial*` and `sf` objects and with `data.frame`. This bug should not have affected too many people.

    ```r
    opt_output_files(ctg) <- paste0(tempdir(), "/{ORIGINALFILENAME}")
    option <- list(automerge = TRUE)
    ret <- catalog_apply(ctg, sptest, .options = option) # now returns a vector
    print(ret) 
    #> "/tmp/RtmpV4CQll/file38f1.txt" "/tmp/RtmpV4CQll/file38g.txt"  "/tmp/RtmpV4CQll/file38h.txt" "/tmp/RtmpV4CQll/file38i.txt"
    ```
    
2. When using a `grid_*` function with a `RasterLayer` used as layout, if the layout was not empty or full of NAs, the values of the layout were transferred to the NA cells of the output [#318](https://github.com/Jean-Romain/lidR/issues/318).

3. `lascheck()` no longer warns about "proj4string found but no CRS in the header". This was a false positive. Overall, CRS are better checked. 

#### ENHANCEMENTS

1. `opt_output_files()` now prints a message when using the `ORIGINALFILENAME` template with a chunk size that is not 0 to inform users that it does not make sense.

    ```r
    opt_chunk_size(ctg) <- 800
    opt_output_files(ctg) <- "{ORIGINALFILENAME}"
    #> ORIGINALFILENAME template has been used but the chunk size is not 0. This template makes sense only when processing by file.
    ```
    
2. Internally when building the chunks an informative error is now thrown when using the `ORIGINALFILENAME` template with a chunk size that is not 0 to inform users that it does not make sense instead of the former uninformative error, `Error in eval(parse(text = text, keep.source = FALSE), envir) : objet 'ORIGINALFILENAME' not found`.

    ```r
    #>  Erreur : The template {ORIGINALFILENAME} makes sense only when processing by file (chunk size = 0). It is undefined otherwise.
    ```

3. When using a "by file" processing strategy + a buffer around each file, up to 9 files may be read. Internally the chunks (`LAScluster`) are now built in such a way that the first file read is the main one (and not one of the "buffer file"). This way, if the 9 files do not have the same scales and the same offsets, the main file has precedence over the other ones when rescaling and re-offsetting on-the-fly. This reduces the risk of incompatibilities and preserves the original pattern when processing a LAScatalog.

4. `grid_metrics()` now constructs a `RasterBrick` in a better way and this reduces the risk of bugs with users' functions that sometimes return 0 length objects. The `RasterBrick` will now be properly filled with `NAs`.

5. `lascheck()` now reports information if some points are flagged 'withheld', 'synthetic' or 'keypoint'.

6. We moved the internal logic of chunk realignment with a raster from `catalog_apply()` to the internal function `catalog_makecluster()`. This simplifies the source code, make it easier to maintain and test and will enable us to provide access, at the user level, to more internal functions in future releases.

## lidR v2.2.2 (Release date: 2020-01-28)

#### FIXES

1. We introduced a bug in v2.2.0 in the catalog processing engine. Empty chunks triggered and error  `i[1] is 1 which is out of range [1,nrow=0]` internally. It now works again.

2. Fix heap-buffer-overflow in `lasrangecorrection()` when throwing an error about invalid range.

3. `lasunormalize()` now update the header.

## lidR v2.2.1  (Release date: 2020-01-21)

#### BREAKING CHANGE

1. `imager` was used to drive the `mcwatershed()` algorithm. `imager` is an orphaned package that generated a warning on CRAN. Consequently `mcwatershed()` has been removed. In attempt to provide an informative message to users, the function still exists but generates an error. Anyway this method was weak and buggy and it was a good reason to remove it...

2. In version 2.2.0 we missed to put the parameter `r` in `point_metrics()`. It is not yet supported but will be.

#### NEW FEATURES

1. LAScatalog processing engine:
    * In `catalog_apply()` the options `automerge` now supports automerging of `sf` and `data.frame` objects.
    * New function `catalog_sapply()` strictly equivalent to `catalog_apply()` but with the option `automerge = TRUE` enforced to simplify the output whenever it is possible.
    
#### ENHANCEMENTS

1. In the catalog processing engine, the graphical progression map is now able to plot the actual shape of the chunks. In the case of `lasclip` it means that discs and polygons are displayed instead of bounding boxes.

2. Multi-layers VRTs are returned as `RasterBrick` instead of `RasterStack` for consistency with in memory raster that are returns as `RasterBrick`.

3. `grid_` functions now try to preserve the layer names when returning a VRT built from files written on disk. This works only with file formats that support to store layer name (e.g. not `GTiff`).

4. There are now more than 900 unit tests for a coverage of 91%.

#### FIXES

1. Fix access to not mapped memory in one unit test (consequentless for users).

2. In `lasclip()` the template `XCENTER` actually gave the Y coordinate. It is now the correct X coordinate of the center of the clipped region.

3. In `lasclip()` the template `YCENTER` was not actually defined. It is now the correct Y coordinate of the center of the clipped region.

4. Fix heap-buffer-overflow in `lasrangecorrection()`. The range was likely to be badly computed for points that have a gpstime later than the last sensor position

## lidR v2.2.0 (Release date: 2020-01-06)

#### NEW FEATURES

1. LAScatalog processing engine:
    *  `catalog_apply()` gains an option `automerge = TRUE`. `catalog_apply()` used to return a `list` that had to be merged by the user. This new option allows for automatic merging. This is a fail-safe feature. In the worst case, if the user-defined function returns a non-supported list of objects that cannot be merged it falls back to the former behavior i.e. it returns a `list`. Thus there is no risk associated with adding the option `automerge = TRUE` but by defaut it is set to `FALSE` for retrocompatibility. This might be switched to `TRUE` in future releases.
    
    * `opt_output_file()` now interprets `*` as `{ORIGINALFILENAME}` for shorter syntax. The following is now accepted: 
    
    ```R
    opt_output_file(ctg) <- "/home/user/data/norm/*_norm"  # {*} is valid as well
    # instead of
    opt_output_file(ctg) <- "/home/user/data/norm/{ORIGINALFILENAME}_norm"
    ```
    * The engine now supports "alternative directories". This is a very specific and undocumented feature useful in a single case of remote computing. More details on the [wiki page](https://github.com/Jean-Romain/lidR/wiki/Make-cheap-High-Performance-Computing-to-process-large-datasets).
    
    ```r
    ctg = readLAScatalog("~/folder/LASfiles/")
    ctg@input_options$alt_dir = c("/home/Alice/data/", "/home/Bob/remote/project1/data/")
    ```
    * LAScatalog modification constraints are now relaxed. It is now possible to add or modify an attribute if this attribute has a name that is not reserved.
    
    ```r
    ctg$newattr <- 1 # is now allowed
    ctg$GUID <- TRUE # is still forbidden
    #> Erreur : LAScatalog data read from standard files cannot be modified 
    ```
    * The engine supports partial processing. It is possible to flag some files that will, or will not, be processed. These files are not removed from the LAScatalog. They are used to load a buffer, if required, for the files that are actually processed. To activate this option a new boolean attribute named `processed` can be added in the catalog. 
    
    ```r
    ctg$processed <- TRUE
    ctg$processed[3:5] <- FALSE
    ```
    
2. 3D rendering:
    * The argument `colorPalette` of the function `plot()` for `LAS` objects is now set to `"auto"` by default. This allows for this argument to not be specified even when plotting an attribute other than Z, and having an appropriate color palette by default. More interestingly, it will automatically apply a nice color scheme to the point cloud with the attribute 'Classification' following the ASPRS specifications. See [#275](https://github.com/Jean-Romain/lidR/issues/275).
    
    ```R
    plot(las)
    plot(las, color = "Intensity")
    plot(las, color = "ReturnNumber")
    plot(las, color = "Classification")
    ```
    * In `plot.lasmetrics3d()` the parameter `trim` is now set to `Inf` by default.
    
3. New function `point_metrics()` - very similar to `grid_metrics()` but at the point level. The 'metrics' family is now complete. `cloud_metrics()` computes user-defined metrics at the point cloud level. `grid_metrics()` and `hexbin_metrics()` compute user-defined metrics at the pixel level. `voxel_metrics` computes user-defined metrics at the voxel level. `point_metrics()` computes user-defined metrics at the point level.

4. `lasnormalize()`:
    * Gains an argument `use_class` to control the points used as ground.
    * By default 'ground point' now includes points classified as water by default. This might be useful in regions with a lot of water because in this case `lasnormalize()` can take forever to run (see [#295](https://github.com/Jean-Romain/lidR/issues/295))).

5. New function `sensor_tracking()` to retrieve the position of the sensor in the sky.

6. New function `lasrangecorrection()` to normalize intensity using the sensor position (range correction)

7. `catalog_select` now also allows files to process to be flagged interactively:

    ```r
    ctg <- catalog_select(ctg, method = "flag_processed")
    ctg <- catalog_select(ctg, method = "flag_unprocessed")
    ```
    
8. `grid_terrain()`
    * Have a new argument `use_class` to control which points are considered as ground points
    * With a `LAScatalog` it now uses the filter `-keep_class` by default respecting the classes given in `use_class`.

#### CHANGES

1. `LAS()` now rounds the values to 2 digits if no header is provided to fit with the default header automatically generated. This ensures that a perfectly valid  `LAS` object is built out of external data. This change is made by reference, meaning that the original dataset is also rounded.

    ```r
    pts <- data.frame(X = runif(10), Y = runif(10), Z = runif(10))
    las <- LAS(pts) # 'las' contains rounded values but 'pts' as well to avoid data copying
    ```

2. `lasmetrics()` is deprecated. All `las*` functions return `LAS` objects except `lasmetrics()`. For consistency across the package `lasmetrics()` becomes `cloud_metrics()`.

3. `grid_metrics3d()` and `grid_hexametrics()` are deprecated. They are renamed `voxel_metrics()` and `hexbin_metrics()` for naming consistency.

4. The example dataset `Topography.laz` is now larger and include attributes gpstime, PointSourceID and some classified lakes.
    
#### ENHANCEMENTS

1. Internally the package used a QuadTree as spatial index in versions <= 2.1.3. Spatial index has been rewritten and changed for a grid partition which is twice as fast as the former QuadTree. This change provides a significant boost (i.e. up to two times faster) to many algorithms of the package that rely on a spatial index. This includes `lmf()`, `shp_*()`, `wing2015()`, `pmf()`, `lassmooth()`, `tin()`, `pitfree()`. Benchmark on a Intel Core i7-5600U CPU @ 2.60GHz × 2.

    ```r
    # 1 x 1 km, 13 pts/m², 13.1 million points
    set_lidr_threads(n)
    tree_detection(las, lmf(3))
    #> v2.1: 1 core: 80s - 4 cores: 38s
    #> v2.2: 1 core: 38s - 4 cores: 20s
   
    # 500 x 500 m, 12 pt/m², 3.2 million points
    lassnags(las, wing2015(neigh_radii = nr, BBPRthrsh_mat = bbpr_th))
    #> v2.1: 1 core: 66s - 4 cores: 33s
    #> v2.2: 1 core: 43s - 4 cores: 21s

    # 250 x 250 m, 12 pt/m², 717.6 thousand points
    lasdetectshape(las3, shp_plane())
    #> v2.1 - 1 cores: 12s - 4 cores: 7s
    #> v2.2 - 1 cores:  6s - 4 cores: 3s
    ```
    
2. Internally the Delaunay triangulation has been rewritten with `boost` instead of relying on the `geometry` package. The Delaunay triangulation and the rasterization of the Delaunay triangulation are now written in C++ providing an important speed-up (up to three times faster) to `tin()`, `dsmtin()` and `pitfree()`. However, for this to work, the point cloud must be converted to integers. This implies that the scale factors and offset in the header must be properly populated, which might not be the case if users have modified these values manually or if using a point cloud coming from a format other than las/laz. Benchmark on an Intel Core i7-5600U CPU @ 2.60GHz × 2.

    ```r
    # 1.7 million ground points
    set_lidr_threads(n)
    grid_terrain(las, 0.5, tin())
    #> v2.1: 1 core: 48s - 4 cores: 37s
    #> v2.2: 1 core: 22s - 4 cores: 20s
    
    # 560 thousand first returns (1.6 pts/m²)
    grid_canopy(las, res = 0.5, dsmtin())
    #> v2.1: 1 core: 8s - 4 cores: 7s
    #> v2.2: 1 core: 3s - 4 cores: 3s
    
    # 560 thousand first returns (1.6 pts/m²)
    grid_canopy(las, res = 0.5, pitfree(c(0,2,5,10,15), c(0, 1.5)))
    #> v2.1: 1 core: 30s - 4 cores: 28s
    #> v2.2: 1 core: 11s - 4 cores: 9s
    ```
    
3. There are more than 100 new unit tests in `testthat`. The coverage increased from 68 to 87%.

4. The vignette named *Speed-up the computations on a LAScatalog* gains a section about the possible additional speed-up using the argument `select` from `readLAS()`.

5. The vignette named *LAScatalog formal class* gains a section about partial processing.

6. Harmonization and review of the sections 'Supported processing options' in the man pages.

#### FIXES

1. Several minor fixes in `lascheck()` for very improbable cases of `LAS` objects likely to have been modified manually.

2. Fix colorization of boolean data when plotting an object of class `lasmetrics3d` (returned by `voxel_metrics()`) [#289](https://github.com/Jean-Romain/lidR/issues/289)

3. The LAScatalog engine now calls `raster::writeRaster()` with `NAflag = -999999` because it seems that the default `-Inf` generates a lot of trouble on windows when building a virtual raster mosaic with `gdalUtils::gdalbuildvrt()`.

4. `plot.LAS()` better handles the case when coloring with an attribute that has only two values: `NA` and one other value.

5. `lasclip()` was not actually able to retrieve the attributes of the `Spatial*DataFrame` or `sf` equivalent when using `opt_output_file(ctg) <- "/dir/{PLOTID}"`.

6. `lasmergespatial()` supports 'on disk' rasters [#285](https://github.com/Jean-Romain/lidR/issues/285) [#306](https://github.com/Jean-Romain/lidR/issues/306)

7. `opt_stop_early()` was not actually working as expected. The processing was aborted without logs. It now prevent the catalog processing engine to stop
even when an error occurs.

8. In `tree_detection()` if no tree is found (e.g. in a lake) the function crashed. It now returns an empty `SpatialPointDataFrame`.

9. The argument `keep_lowest` in `grid_terrain` returned dummy output full of NAs because NAs have the precedence on actual numbers.

## lidR v2.1.4 (Release date: 2019-10-15)

#### NEW FEATURES

1. `grid_terrain()` gains an argument `full_raster = FALSE`.

2. `lasnormalize()` gains an argument `...` to tune `raster::extract()` and use, for example, `method = "bilinear"`.

#### FIXES

1. In `lasground()` if `last_returns = TRUE` and the `LAS` is not properly populated i.e. no last return, the classification was not actually computed. The expected behavior was to use all the points. This is now the case.

2. `lasclip()` is now able to clip into a `LAS` objects using `SpatialPoints` or `sf POINT`. It previously worked only into `LAScatalog` objects.

3. `lasaddextrabyte_manual()` was not actually working because the `type` was not converted to a numeric value according to the LAS specifications.

4. Fix double precision floating point error in `grid_*` function in some specific cases. This fix affect also `highest()` and other raster-based algorithms [#273](https://github.com/Jean-Romain/lidR/issues/273).

5. `lasreoffset()` now checks for integer overflow and throws an error in case of invalid user request [#274](https://github.com/Jean-Romain/lidR/issues/274).

6. Tolerance for internal `point_in_triangle()` have been increased to fix double precision error in rasterization of a triangulation. This fixes some rare `NA`s in `pitfree()`, `dsmtin()` and `tin()`.

7. The NAs are now correctly interpreted when writing a GDAL virtual raster [#283](https://github.com/Jean-Romain/lidR/issues/283).

8. Fix `lasmergespatial()` with 'on disk' rasters [#285](https://github.com/Jean-Romain/lidR/issues/285).

9. Fix `pitfree()` with a single triangle case [#288](https://github.com/Jean-Romain/lidR/issues/288).

#### ENHANCEMENTS

1. `pitfree()` handles more errors and fails more nicely in some specific cases [#286](https://github.com/Jean-Romain/lidR/issues/286).

## lidR v2.1.3 (Release date: 2019-09-10)

#### NEW FEATURES

1. New functions `lasrescale()` and ` lasreoffset()` to modify the scale factors and the offsets. The functions update the header and recompute the coordinates to get the proper rounded values in accordance with the new header.

2. `readLAS()` throw (again) warnings for invalid files such as files with invalid scale factors, invalid bounding box, invalid attributes ReturnNumber and so on.

#### ENHANCEMENT

1. `readLAScatalog()` is 60% faster

2. The progress bar of the LAScatalog processing engine has been removed in non interactive sessions and replaced by regular but more informative prints. This allows to track the state of the computation with a stream redirection to a file when running a script remotely for example.

    ```
    R -f script.R &> log.txt &
    ```

#### FIXES

1. Fix an infinite loop in the knn search when k > number of points. This bug may affect `lasdetectectshape()`, `wing2012()` and other functions that rely on a knn search.

2. Using remote futures now works for any function that supports a `LAScatalog` input. Previously remote evaluation of futures failed because of the presence of `return()` statement in the code [future#333](https://github.com/HenrikBengtsson/future/issues/333)

    ```r
    plan(remote, workers = "132.203.41.25")
    ```

3. `lasclipCircle()` behaves identically for `LAS` and `LAScatalog` object. It now returns the points that are strictly inside the circle. Previously for `LAS` objects it also returned the point belonging on the disc.

4. The bounding box is updated after `lastransform()` [#270](https://github.com/Jean-Romain/lidR/issues/270)

5. The offsets are updated after `lastransform()` to prevent integer overflow when writing the point cloud in `.las` files [#272](https://github.com/Jean-Romain/lidR/issues/272)

6. Removed deprecated C++ functions `std::bind2nd` as requested by CRAN.

#### NOTE

1. All C++ source code has been reworked in a tidy framework to clean-up 4 years of mess. It is almost invisible for regular users but the size of the package has been reduced of several MB and many new tools will now be possible to build.

## lidR v2.1.2 (Release date: 2019-08-07)

#### FIXES

1. Fix a serious issue of uninitialized values in an internal C++ function but this issue is consequentless for the package.

## lidR v2.1.1 (Release date: 2019-08-06)

#### NEW FEATURES

1. [#266](https://github.com/Jean-Romain/lidR/issues/266) `lasmetrics` has now a dispatch to  `LAS` and `LAScluster` cluster objects. It means that `lasmetrics` can be used with `catalog_apply` in some specific cases where it has a meaning (see also [#266](https://github.com/Jean-Romain/lidR/issues/266)):

    ```r
    opt_chunk_buffer(ctg) <- 0
    opt_chunk_size(ctg) <- 0
    opt_filter(ctg) <- "-keep_first"
    opt_output_files(new_ctg) <- ""
    output <- catalog_apply(new_ctg, lasmetrics, func = .stdmetrics)
    output <- data.table::rbindlist(output)
    ```

#### ENHANCEMENT

1. `lastrees()` now uses S3 dispatcher system. When trying to use it with a `LAScatalog` object, user will have a standard R message to state that `LAScatalog` is not supported instead of an uninformative message that state that 'no slot of name "header" for this object of class "LAScatalog"'

2. Internal code has been modified to drastically reduce probability of name intersection in `catalog_apply()`. For example, the use of a function that have a parameter `p` in  `catalog_apply()` failed because of partial matching between the true argument `p` and the internal argument `processing_option`.

3. `lasfilterdecimate()` with algorithm `highest()` is now more than 20 times faster. `lasfiltersurfacepoints()`, being a proxy of this algorithm, had the same speed-up

4. `plot` for `LAS` objects gained the pan capability.

#### FIXES

1. [#267](https://github.com/Jean-Romain/lidR/issues/267). A dummy character was introduced by mistake in a variable name breaking the automatic exportation of user object in `grid_metrics` when used with a parallelized plan (`tree_metrics()` was also affected).


## lidR v2.1.0 (Release date: 2019-07-13)

#### VISIBLE CHANGES

Several algorithms are now natively parallelized at the C++ level with `OpenMP`. This has for consequences for speed-up of some computations by default but implies visible changes for users. For more details see `help("lidR-parallelism")`. The following only explains how to modify code to restore the exact former behavior.

In versions `< 2.1.0` the catalog processing engine has R-based parallelism capabilities using the `future` package. The addition of C++-based parallelism introduced additional complexity. To prevent against nested parallelism and give the user the ability to use either R-based or C++-based parallelism (or a mix of the two), the function `opt_cores()` is no longer supported. If used it generates a message and does nothing. The strategy used to process the tiles in parallel must now be explicitly declared by users. This is anyway how it should have been designed from the beginning! For users, restoring the exact former behavior implies only one change.

In versions `< 2.1.0` the following was correct:

```r
library(lidR)
ctg <- catalog("folder/")
opt_cores(ctg) <- 4L
hmean <- grid_metrics(ctg, mean(Z))
```

In versions `>= 2.1.0` this must be explicitly declared with the `future` package:

```r
library(lidR)
library(future)
plan(multisession)
ctg <- catalog("folder/")
hmean <- grid_metrics(ctg, mean(Z))
```

#### NEW FEATURES

1. `readLAS()`:
    * LAS 1.4 and point formats > 6 are now better supported. `lascheck()` and `print()` were updated to work correctly with these formats ([#204](https://github.com/Jean-Romain/lidR/issues/204))
    * New function `readLASheader()` to read the header of a file in a `LASheader` object.
   
2. Coordinate Reference System:
    * New function `wkt()` to store a WKT CRS in a LAS 1.4 file. This function is the twin of `epsg()` to store CRS. It updates the `proj4string` and the header of the LAS object. This function is not expected to be used by users. Users must prefer the new function `projection()` instead.
    * New function `projection<-` that updates both the slot `proj4string` and the header with an EPSG code or a WKT string from a `proj4string` or a `sp:CRS` object. This function supersedes `epsg()`and `wkt()` that are actually only useful internally and in specific cases. The vignette `LAS-class` has been updated accordingly.

    ```r
    projection(las) <- projection(raster)
    ```

3. LAScatalog processing engine:
    * Progression estimation displayed on a map now handles warnings by coloring the chunks in orange.
    * Progression estimation displayed on a map now colors in blue the chunks that are processing.
    * The engine now returns the partial result in case of a fail.
    * The engine now has a log system to help users reload the chunk that throws an error and try to understand what going wrong with this cluster specifically. If something went wrong a message like the following is displayed:

    ```
    An error occurred when processing the chunk 190. Try to load this chunk with:
    chunk <- readRDS("/tmp/RtmpAlHUux/chunk190.rds")
    las <- readLAS(chunk)
    ```
    
4. `grid_metrics()`:
    * New function `stdshapemetrics()` and lazy coding `.stdshapemetrics` to compute eigenvalue-related features ([#217](https://github.com/Jean-Romain/lidR/issues/217)).
    * New argument `filter` in `grid_metrics()`. This argument enables users to compute metrics on a subset of selected points such as "first returns", for example, without creating a copy of the point cloud. Such an argument is expected to be added later in several other functions.

    ```r
    hmean <- grid_metrics(las, ~mean(Z), 20, filter = ~ReturnNumber == 1)
    ```

5. New functions `lasdetectshape()` for water and human-made structure detection with three algorithms `shp_plane()`, `shp_hplane()`, `shp_line()`.


6. `plot()`:
    * For LAS objects `plot()` gained an argument `axis = TRUE` to display axis.
    * For LAS objects `plot()` gained an argument `legend = TRUE` to display color gradient legend ([#224](https://github.com/Jean-Romain/lidR/issues/224)).

7. `tree_hull()`: 
    * Gained an argument `func` to compute metrics for each tree, like `tree_metrics()`

    ```r
    convhulls <- tree_hulls(las, func = ~list(imean = mean(Intensity)))
    ```

8. Miscellaneous tools:
    * The function `area()` has been extended to `LASheader` objects.
    * New functions `npoints()` and `density()` available for `LAS`, `LASheader` and `LAScatalog` objects that return what users may expect.

    ```r
    las    <- readLAS("file.las", filter = "-keep_first")
    header <- readLASheader(file)
    ctg    <- catalog("folder/")
    
    npoints(las)    #> [1] 55756
    npoints(header) #> [1] 81590
    npoints(ctg)    #> [1] 1257691
    
    density(las)    #> [1] 1.0483
    density(header) #> [1] 1.5355
    density(ctg)    #> [1] 1.5123
    ```

9. Several functions are natively parallelized at the C++ level with OpenMP. See `help("lidR-parallelism")` for more details.

10. New function `catalog_select` for interactive tile selection.

11. `lasground` have lost the argument `last_returns` for a more generic argument `filter`. Retro-compatibility as been preserved by interpreting adding an ellipsis.

#### NOTE

1. `grid_metrics()`, `grid_metrics3d()`, `tree_metrics()`, `tree_hull()`, `grid_hexametrics()` and `lasmetrics()` expect a formula as input. Users should not write `grid_metrics(las, mean(Z))` but `grid_metrics(las, ~mean(Z))`. The first syntax is still valid, for now.

2. The argument named `field` in `tree_metrics()` is now named `attribute` for consistency with all other functions.

3. The documentation of supported options in `tree_*()` functions was incorrect and has been fixed.

4. `readLAScatalog()` replaces `catalog()`. `catalog()` is soft-deprecated.

#### FIX

1. [#264](https://github.com/Jean-Romain/lidR/issues/264) `grid_terrain` now filter degenerated ground points.

2. [#238](https://github.com/Jean-Romain/lidR/issues/228) fix a floating point precision error in `p2r` algorithm.

##### ENHANCEMENT

1. When reading a file that contains extrabytes attributes and these data are not loaded (e.g. `readLAS(f, select = "xyzi")`) the header is updated to remove the non-loaded extrabytes. This fixes the issue [#234](https://github.com/Jean-Romain/lidR/issues/234) and enables LAS objects to be written without updating the header manually.

## lidR v2.0.3 (Release date: 2019-05-02)

- Fix: in `li2012()` the doc states that *If R = 0 all the points are automatically considered as 
local maxima and the search step is skipped (much faster)*. This is now true.
- Fix: in `lasmergespatial` used with a `SpatialPolygonDataFrame` when the bounding boxes do not match the full search was performed uselessly. Now the function exits early without searching anything.
- Fix: [#242](https://github.com/Jean-Romain/lidR/issues/242) on Windows when using multicore options to process a LAScatalog the parameter of the algorithms were not exported to each session.
- Enhance: internally the function `tsearch` that searches in a triangulation is 25% faster giving a small speed-up to `pitfree()` and `tin()` algorithms.
- Enhance: in `lasmergespatial` used with a `SpatialPolygonDataFrame` the function checks the bounding box of the polygon to speed-up the computation with complex polygons.
- Doc: add a `?lidR` page to the manual.
- Fix: in `li2012()` the doc states that *If R = 0 all the points are automatically considered as local maxima and the search step is skipped (much faster)*. This is now true.
- Fix: in `lasmergespatial` with a `SpatialPolygonDataFrame` when the bounding boxes do not match instead of exiting early without searching anything the full search was performed uselessly.

## lidR v2.0.2 (Release date: 2019-03-02)

- Fix: [#222](https://github.com/Jean-Romain/lidR/issues/222) `grid_*()` functions return consistently a `RasterLayer` if there is a single layer. virtual raster mosaic were returned as `RasterStack` no matter the number of layers.
- Fix: [#223](https://github.com/Jean-Romain/lidR/issues/223) `lasmergespatial()` wrongly copied shapefile attributes to each point when the parameter `attribute` was the name of an attribute of the shapefile.
- Fix: [#225](https://github.com/Jean-Romain/lidR/issues/225) `laspulse()`, `lasflightline()`, `lasscanline()` were broken since v2.0.0.
- Fix: [#227](https://github.com/Jean-Romain/lidR/issues/227) When processing a LAScatalog the chunks are better computed. In former version it was possible to have chunks that lie on tile only because of the buffer. These chunks are not build anymore.
- Fix: [#227](https://github.com/Jean-Romain/lidR/issues/227) When processing a LAScatalog some chunks may belong in a file/tile but when actually reading the points in the file the chunks could be empty with points only in the buffer region. In these case an empty point cloud is returned and the computation is be skipped.
- Fix: [#228](https://github.com/Jean-Romain/lidR/issues/228) `lasmergespatial()` and `lasclip()` loose precision when extracting polygons due to missing digits in the WKT string used to rebuild the polygons at C++ level.

## lidR v2.0.1 (Release date: 2019-02-02)

- Change: the function `catalog` has been slightly modified in prevision of the release of the package `rlas 1.3.0` to preserve future compatibility. This is invisible for the users.
- New: `lasnormalize` gained a parameter `na.rm = TRUE`
- Fix: an error occured when plotting a LAScatalog with the option `chunk_pattern = TRUE`: object 'ctg' not found.
- Fix: examples in documentation of `tin()` and `knnidw()` were inverted.
- Fix: [#213](https://github.com/Jean-Romain/lidR/issues/213) bug when using option `keep_lowest` in `grid_terrain`.
- Fix: [#212](https://github.com/Jean-Romain/lidR/issues/212) bug when merging big rasters that exceed the memory allowed by the raster package
- Fix: bug when merging rasters when some of then only have one cell
- Fix: bug when printing a 0 point LAS object

## lidR v2.0.0 (Release date: 2019-01-02)

### Why versions `> 2.0` are incompatible with versions `1.x.y`?

The lidR package versions 1 were mainly built upon "personal R scripts" I wrote 3 years ago. These scripts were written for my own use at a time when the lidR package was much smaller (both in term of code and users). The lidR package became a relatively large framework built on top of an unstructured base so it became impossible to develop it further. Many features and functions were missing because the way lidR was built did not allow them to be written. The new release (lidR version 2) breaks the former code to build a more robust, more consistent and more scalable framework that is intended and expected to continue for years without the need to break anything more in the future.

Old binaries can still be found here for 6 months:

- [Windows R-3.6](https://drive.google.com/open?id=1VHM38ftV71lasTQCcm8BxEChP2qxpV9e)
- [Windows R-3.5](https://drive.google.com/open?id=1E-Iieu0DTKHsDq1TDwq_VUGeiXta2T_Y)
- [Windows R-3.4](https://drive.google.com/open?id=1nRUyH0SaBCflGDCItkZhhSU9NQR0DXqr)
- [Mac OSX R-3.5](https://drive.google.com/open?id=1fmMTi6haFQpJFjAVVqW-vIFTULT_YWak)
- [Mac OSX R-3.4](https://drive.google.com/open?id=1Pw04QHeoF4uE8QPRIASGX_sB17r-sdZm)

### Overview of the main visible changes

**lidR as a GIS tool**

lidR versions 1 was not a GIS tool. For example, rasterization functions such as `grid_metrics()` or `grid_canopy()` returned a `data.frame`. Tree tops extraction with `tree_detection()` also returned a `data.frame`. Tree segmentation with `lastrees()` accepted `RasterLayer` or `data.frame` as input in a very inconsistent way. Moreover, the CRS of the point cloud was useless and never propagated to the outputs because outputs were not spatial objects.

lidR version 2 consistently uses `Raster*` and `Spatial*` objects everywhere. Rasterization functions such as `grid_metrics()` or `grid_canopy()` return `Raster*` objects. Tree tops extraction returns `SpatialPointDataFrame` objects. Tree segmentation methods accept `SpatialPointDataFrame` objects only in a consistent way across functions. The CRS of the point cloud is always propagated to the outputs. `LAS` objects are `Spatial` objects. `LAScatalog` objects are `SpatialPolygonDataFrame` objects. In short, lidR version 2 is now a GIS tool that is fully compatible with the R ecosystem.

**No longer any update by reference**

Several lidR functions used to update objects by reference. In lidR versions 1 the user wrote: `lasnormalize(las)` instead of `las2 <- lasnormalize(las1)`. This used to make sense in R < 3.1 but now the gain is no longer as relevant because R makes shallow copies instead of deep copies. 

To simplfy, let's assume that we have a 1 GB `data.frame` that stores the point cloud. In R < 3.1 `las2` was a copy of `las1` i.e. `las1` + `las2` = 2GB . This is why we made functions that worked by reference that implied no copy at all. This was memory optimized but not common or traditional in R. The question of memory optimization is now less relevant since R >= 3.1. In the previous example `las2` is no longer a deep copy of `las1`, but a shallow copy. Thus lidR now consistently uses the traditional syntax `y <- f(x)`.

**Algorithm dispatch**

The frame of lidR versions 1 was designed at a time when there were fewer algorithms. The increasing number of algorithms led to inconsistent ways to dispatch algorithms. For example:

* `grid_canopy()` implemented one algorithm and a second function `grid_tincanopy()` was created to implement another algorithm. With two functions the switch was possible by using two different names (algorithms dispatched by names).
* `grid_tincanopy()` actually implemented two algorithms in one function. The switch was possible by changing the input parameters in the function (algorithm dispatched by input).
* `lastrees()` had several variants that provided access to several algorithms: `lastrees_li()`, `lastrees_dalpontes()`, `lastrees_watershed()`, and so on. With several functions the switch was possible by using several different names (algorithms dispatched by names).
* `tree_detection` did not have several variants, thus it was impossible to introduce a new algorithm (no dispatch at all).

lidR version 2 comes with a flexible and scalable dispatch method that unifies all the former functions. For example, `grid_canopy()` is the only function to make a CHM. There is no longer the need for a second function `grid_tincanopy()`. `grid_canopy()` unifies the two functions by accepting as input an algorithm for a digital surface model:

```r
chm = grid_canopy(las, res = 1, algo = pitfree())
chm = grid_canopy(las, res = 1, algo = p2r(0.2))
```

The same idea drives several other functions including `lastrees`, `lassnags`, `tree_detection`, `grid_terrain`, `lasnormalize`, and so on. Examples:

```r
ttops = tree_detection(las, algo = lmf(5))
ttops = tree_detection(las, algo = lidRplugins::multichm(1,2))
lastrees(las, algo = li2012(1.5, 2))
lastrees(las, algo = watershed(chm))
lasnormalize(las, algo = tin())
lasnormalize(las, algo = knnidw(k = 10))
```

This allows `lidR` to be extended with new algorithms without any restriction either in lidR or even from third-party tools. Also, how lidR functions are used is now more consistent across the package.

**LAScatalog processing engine**

lidR versions 1 was designed to run algorithms on medium-sized point clouds loaded in memory but not to run algorithms over a set of files covering wide areas. In addition, lidR 1 had a poorly and inconsistently designed engine to process catalogs of las files. For example:

* It was possible to extract a polygon of points from a `LAScatalog` but not multipart-polygons or polygons with holes. This was only possible with `LAS` objects i.e loaded in memory (inconsistent behaviors within a function).
* It was possible to run `grid_metrics()` on a `LAScatalog` i.e. over a wide area not loaded in memory, but not `lasnormalize`, `lasground` or `tree_detection` (inconsistent behavior across the functions).

lidR version 2 comes with a powerful and scalable catalog processing engine. Almost all the lidR functions can be used seamlessly with either `LAS` or `LAScatalog` objects. The following chunks of code are now possible:

```r
ctg = catalog("folfer/to/las/file")
opt_output_file(ctg) <- "folder/to/normalized/las/files/{ORIGINALFILENAME}_normalized"
new_ctg = lasnormalize(ctg, algo = tin())
```

### Complete description of visible changes

**LAS class**

* Change: the `LAS` class **is** now a `Spatial` object or, more technically, it inherits a `Spatial` object.
* Change: being a `Spatial` object, a `LAS` object no longer has a `@crs` slot. It has now a slot `@proj4string` that is accessible with the functions `raster::projection` or `sp::proj4string`
* New: being a `Spatial` object, a `LAS` object inherits multiple functions from `raster` and `sp`, such `$` and `[[` accessors or `raster::extent`, `sp::bbox`, `raster::projection`, and so on. However, the replacement method `$<-`, `[[<-` have restricted capabilities to ensure a `LAS` object cannot be modified in a way that implies loosening the properties of the LAS specifications.
* New: empty `LAS` objects with 0 points are now allowed. This has repercussions for several functions including `lasfilter`, `lasclip`, and `readLAS` that do not return `NULL` for empty data but a `LAS` object with 0 points. This new behavior has been introduced to fix the old inconsistent behavior of functions that return either `LAS` or `NULL` objects. `LAS` objects are always returned.

**LAScatalog class**

* Change: the `LAScatalog` class **is** now a `SpatialPolygonsDataFrame` or, more technically, it inherits a `SpatialPolygonsDataFrame`. 
* Change: being a `SpatialPolygonsDataFrame` object, a `LAScatalog` no longer has a `@crs` slot. It has now a slot `@proj4string` that is accessible with the functions `raster::projection` or `sp::proj4string`.
* Change: being a `SpatialPolygonsDataFrame` a `LAScatalog` can be plotted with `sp::spplot()`.
* Change: there are no longer any slots `@cores`, `@by_file`, `@buffer`, and so on. They are replaced by more generic and scalable slots `@processing_options`, `@output_options`, `@clustering_options` and `@input_options` that are list of options classified by their main roles.
* Change: documentation has been entirely rewritten to explain the whole potential of the class.
* Change: functions `by_file`, `progress`, `tiling_size`, `buffer` were replaced by `opt_chunk_size`, `opt_chunk_buffer`, `opt_progress`, and so on. These allow for a consistent set of functions that do not overlap with functions from `raster` or `sp`.
* Change: standard column names were renamed to make syntactically-valid names and for compatibility with `sp` functions.

**readLAS**

* Change: `readLAS` no longer supports option `PFC`. Users must use the functions `laspulse`, `lasflightlines` manually.

**lasclip**

* New: `lasclip` now works both with a `LAS` object and a `LAScatalog` object in a seamless and consistent way. There are no longer any differences between the capabilities of the `LAS` version or the `LAScatalog` one.
* New: `lasclip` support many geometries including multipart polygons and polygons with holes, both with a `LAS` object and a `LAScatalog` object.
* Change: The option `inside` has been removed for consistency because it cannot be safely supported both on `LAS` and `LAScatalog`.
* Change: The option `ofile` has been removed for consistency and this option in now managed by the `LAScatalog` processing engine. For example, one can extract ground inventories and write them in `laz` files automatically named after their center coordinates like this:

```r
ctg = catalog(folder)
output_files(ctg) <- "path/to/a/file_{XCENTER}_{YCENTER}"
laz_compression(ctg) <- TRUE
new_ctg = lasclipCircle(ctg, xc,yc, r)
```

* Change: documentation has been reviewed and extended
* Change: `lasclip` does not return `NULL` anymore for empty queries but an empty `LAS` object.
* Fix: `lasclipRectangle` returns the same output both with a `LAS` and a `LAScatalog`. With a `LAS` the rectangle is now closed on the bottom and the left and open on the right and the top.

**catalog_queries**

* Change: `catalog_queries` has been removed because it is superseded by `lasclip`.

**lasnormalize**

* Change: `lasnormalize()` no longer updates the original object by reference.
* Change: remove the old option `copy = TRUE` that is now meaningless.
* Change: `lasnormalize()` now relies on lidR algorithms dispatch (see also the main new features above).
* New: `lasnormalize()` can be applied on a `LAScatalog` to write a new normalized catalog using the catalog processing engine (see also the main new features above).

**lasclassify**

* Change: `lasclassify()` is now named `lasmergespatial()` to free the name `lasclassify` that should be reserved for other usage.
* Change: `lasmergespatial()` no longer updates the original object by reference.
* Fix: the classification, when made with a `RasterLayer`, preserves the data type of the `RasterLayer`. This also fixes the fact that `lastrees()` used to classify the tree with `double` instead of `int`.

**tree_detection**

* Change: `tree_detection()` now relies on the new dispatch method (see also the main new features above).
* New: algorithm `lmf` has user-defined variable-sized search windows and two possible search window shapes (square or disc).
* New: introduction of the `manual` algorithm for manual correction of tree detection.
* New: `tree_detection` algorithms are seamlessly useable with a `LAScatalog` object by using the catalog processing engine (see also the main new features above). Thus, the following just works:

```r
ctg  <- catalog(folder)
ttop <- tree_detection(ctg, lmf(5))
```

* Change: the `lmf` algorithm, when used with a `RasterLayer` as input, expects parameters given in the units of the map and no longer in pixels.
* Change: `tree_detection()` function consistently returns a `SpatialPointsDataFrame` whatever the algorithm.
* Change: `tree_detection()` function based on a CHM no longer support a `lasmetric` object as input. Anyway, this class no longer exists.

**tree_metrics**

* Change: `tree_metrics()` returns a `SpatialPointsDataFrame`.
* Change: `tree_metrics()` is seamlessly useable with a `LAScatalog` using the catalog processing engine (see also the main new features above). Thus, this just works if the las file has extra bytes attributes that store the tree ids:

```r
ctg <- catalog(folder)
metrics <- tree_metrics(ctg, list(`Mean I` = mean(Intensity)))
```

**lastrees**

* Change: `lastrees()` now relies on the new algorithms dispatch method (see also the main new features above).
* New: introduction of the `mcwatershed` algorithm that implements a marker-controlled watershed.

**grid_metrics**

* Change: `grid_metrics()` as well as other `grid_*` functions consistently return a `RasterLayer` or a `RasterBrick` instead of a `data.table`.
* Change: option `splitlines` has been removed. `grid_metrics()` used to return a `data.table` because of the `splitlines` option and lidR was built on top of that feature from the very beginning. Now lidR consistently uses`sp` and `raster` and this option is no longer supported.

**grid_terrain**

* Change: `grid_terrain()` now relies on the new algorithms dispatch method (see also the main new features above).
* Change: `grid_terrain()` consistently returns a `RasterLayer` instead of a `data.table`, whatever the algorithm used.

**grid_canopy**

* Change: `grid_canopy()` now relies on the new algorithms dispatch method (see also the main new features above). It unifies the former functions `grid_canopy()` and `grid_tincanopy()`.
* Change: `grid_canopy()` consistently returns a `RasterLayer` instead of a `data.table`, whatever the algorithm used.
* Fix: the pitfree algorithm fails if a layer contains only 1 or 2 points.
* Fix: the p2r algorithm is five times faster with the subcircle tweak.

**grid_tincanopy**

* Change: `grid_tincanopy()` has been removed. Digital Surface Models are consistently driven by the function `grid_canopy()` and the lidR algorithm dispatch engine. The algorithms that replaced `grid_tincanopy()` are `dsmtin` and `pitfree`.

**grid_hexametrics**

* Change: as for `grid_metrics`, the parameter `splitlines` has been removed.
* Change: the function returns a `hexbin` object or a list of `hexbin` objects and no longer `data.table` objects.

**grid_catalog**

* Change: `grid_catalog()` has been removed. The new `LAScatalog` processing engine means that this function is no longer useful.

**class lasmetrics**

* `data.table` with a class `lasmetrics` no longer exists. It has been consistently replaced by `RasterLayer` and `RasterBrick` everywhere.
* `as.raster` no longer exists because it used to convert `lasmetrics` into `RasterLayer` and `RasterStack`.
* `as.spatial` no longer converts `lasmetrics` to `SpatialPixelsDataFrame` but still converts `LAS` to `SpatialPointsDataFrame`. 
* `plot.lasmetrics` has been removed obviously.

**lasroi**

* Change: `lasoi()` has been removed. It was not useful and 'buggy'. It might be reintroduced later in `lasclipManual`.

**lascolor**

* Change: `lascolor()` has been removed. It was one of the first functions of the package and is no longer useful because `plot()` has enhanced capabilities.

**lasfilterdecimate**

* Change: now relies on the new algorithms dispatch method (see also the main new features above).
* New: introduction of the algorithm `highest` available in `lasfilterdecimate()`. This supersedes the function `lasfiltersurfacepoints()`.

**lassnags**

* Change: `lassnags()` now relies on the new algorithms dispatch method (see also the main new features above).
* New: `lasnsnags()` can be applied on a `LAScatalog` to write a new catalog using the catalog processing engine (see also the main new features above).

**lidr_options**

* Change: `lidr_option()` has been removed. The options are now managed by regular R base options with function `options()`. Available lidR options are named with the prefix `lidR`.

**Example files**

* New: the three example files are now georeferenced with an EPSG code that is read and converted to a `proj4string`.
* New: the example file `MixedConifers.laz` contains the segmented trees in extra bytes 0.

**plot**

* New: `plot()` for `LAS` objects supports `RGB` as a color attribute.
* New: option `color` supports lazy evaluation. This syntax is correct: `plot(las, color = Classification)`.
* New: option `clear_artifact = TRUE` shifts the point cloud to (0,0) and reduces the display artifact due to the use of floating point in `rgl`.
* New: new functions `add_treetops3d`, `add_dtm3d` and `plot_dtm3d` add elements in the point cloud.
* Change: `trim` does not trim on a percentile of values but on the values themselves.

**Coordinate reference system**

* New: coordinate reference system is supported everywhere and can be written in las files. See function `epsg()`.
* New: function `lastranform` that returns transformed coordinates of a `LAS` object using the CRS argument.

**New functions**

* New: function `lasfilterduplicates`
* New: function `lascheck`
* New: function `lasvoxelize`

### Other changes that are not directly visible

* Change: the code that drives the `point_in_polygon` algorithm relies on `boost` and drastically simplifies the former code of `lasmergespatial()`
* Change: many memory optimizations
