If you are viewing this file on CRAN, please check [the latest news on GitHub](https://github.com/r-lidar/lidR/blob/master/NEWS.md) where the formatting is also better

## lidR v4.0.1 (Release date: 2022-05-03)

- Fix: `plot(ctg, chunk = TRUE)` does not fail if an invalid output file template is registered [#537](https://github.com/r-lidar/lidR/issues/537)
- Enhance: `locate_trees()` throws an informative error if called with an on-disk raster. The former error was cryptic. If the raster is small enough it is loaded on-the-fly.
- Fix: `merge_spatial()` with RGB and `SpatRaster` was not working properly [#545](https://github.com/r-lidar/lidR/issues/545)
- Enhance: `st_area()` better estimates the area of small point-clouds and is faster
- Fix: [#548](https://github.com/r-lidar/lidR/issues/548)
- Enhance: Scale factors are better estimated in `interpret_waveform`  [#549](https://github.com/r-lidar/lidR/issues/549). 
- Fix: `plot_metrics()` returns NA if 0 points available [#551](https://github.com/r-lidar/lidR/issues/551).
- Fix: floating point accuracy error with `rasterize_canopy` may generate error or messed-up CHM [#552](https://github.com/r-lidar/lidR/issues/552).
- Fix: `print()` and `st_area()` were not working for point cloud with no CRS
- Fix: `track_sensor()` does not fail with a `LAScatalog` when no sensor position is found. It also triggers a warning. [#556](https://github.com/r-lidar/lidR/issues/556).
- Fix: The LAScatalog processing engine works with a single file [#558](https://github.com/r-lidar/lidR/issues/558).
- Fix: `rasterize_terrain()` now works with a `LAScatalog` and `shape = sfc_object` [#558](https://github.com/r-lidar/lidR/issues/558).
- Fix: `catalog_retile()` now works when some tiles are empty [#563](https://github.com/r-lidar/lidR/issues/563).
- Fix: `crown_metrics()` messed up tree IDs with a hull geometry [#554](https://github.com/r-lidar/lidR/issues/554).
- Fix: `merge_spatial()` crops large vectors to the extent of the point cloud before to perform the merge. This has for consequences to sometime transform polygons into multipolygons. When polygons and multipolygons were mixed the functions stopped with an error. It now works.
- Fix: `normalize_height()` now sets the Z offset to 0 [#571](https://github.com/r-lidar/lidR/issues/571).
- Fix: smaller rasters stored on-disk are better handled and loaded if needed

### Changes related to rlas 1.6.0

We are currently developing rlas 1.6.0 that uses the ALTREP framework to load compact representation of non populated attributes. For example `UserData` is usually populated with zeros (not populated). Yet it takes 32 bits per point to store each 0. With rlas 1.6.0 it will only uses 644 bits no matter the number of points loaded for non populated attributes. This applies to each attribute populated with a single repeated value. This allows for saving approximately 30% of memory usage depending on the number of non-populated attributes that are present in the file. rlas 1.6.0 is compatible will all versions of lidR but lidR 4.0.1 introduced some internal optimization, internal fixes and new functions to fully take advantage of rlas 1.6.0. lidR v<= 4.0.0 will work with rlas 1.6.0 but won't take advantage of the new compression feature.

1. the function `LAS()` no longer call `data.table::setDT()` if the input is already a `data.table`. Indeed `data.table::setDT()` materializes the compressed ALTREP vectors and this is not what we want. One consequence of this change is that `readLAS()` now preserve the ALTREPness (i.e. the compression) of the output of `rlas::read.las()`.

2. Subsetting a `LAS` object no longer call `data.table` native subset. We previously used something like `las@data[indx]` to subset the point cloud. Sadly `data.table` tries to materialized the ALTREPed vector whenever it can. We implemented internally a `smart_subset()` function that subset and preserves the compression of the vectors. One consequence of such change is that all `filter_*()` and `clip_*()` functions preserve the compression of the point-cloud if any.

3. `las_check()` has been slightly modified to ensure it does not materialize ALTREPed object. One side effect of `las_check()` was to decompress the point cloud unexpectedly. Such a pity! We also change `las_check()` to print information about the compression.

4. We changed the way `*_metrics()` functions evaluates the user defined expression because we found that it had the side effect of materializing all the attributes instead of materializing only those needed. For example `pixel_metrics(las, mean(Z))` only needs the attribute Z. No need to allocate and copy memory for `Intensity`, `ScanAngle` and so on. In previous version all attributes where inspected with the side effect to materialize all compressed vectors. The `*_metrics()` functions now properly detect which attributes are actually necessary for the evaluation of `func`. Two consequences:  (1)`*_metrics()` functions are 20 to 40% faster, (2) the compression is preserved if no compressed attribute is used in the evaluation and e.g. `pixel_metrics(las, mean(UserData))` uncompresses only `UserData`.

5. New functions `las_is_compressed()` that tells which attributes are compressed and `las_size()` that returns the true size of a `LAS` objects taking into account the compression. `las_size()` should returns something similar to `pryr::object_size()` but different to `object.size()` that is not ALTREP aware. We also changed the `print` function so it uses `las_size()` instead of `object.size()`.

On overall lidR's functions are expected to almost never decompress a LAS object. However other R packages and R functions may do it. For example `data.table::print` do materializes the ALTREP vectors. `base::range()` too but not `base::mean()` or `base::var()`.

```r
las@data                    # Full decompression (print data.table)
range(las$Userdata)         # Decompression of UserData
las@data[2, UserData := 1]  # Decompression of UserData
las@data[1:10]              # Full decompression
```

## lidR v4.0.0 (Release date: 2022-02-17)

`rgdal` and `rgeos` will be retired on Jan 1st 2024. see [twitter](https://twitter.com/RogerBivand/status/1407705212538822656), [youtube](https://www.youtube.com/watch?v=cK08bxUJn5A), or see the respective package descriptions on CRAN. Packages `raster` and `sp` are based on `rgdal`/`rgeos` and `lidR` was based on `raster` and `sp` because it was created before `sf`, `terra` and `stars`. This means that sooner or later `lidR` will run into trouble (actually it is more or less already the case). Consequently, we modernized `lidR` by moving to `sf`, `terra`/`stars` and we are no longer depending on `sp` and `raster` (see also [Older R Spatial Package](https://keen-swartz-3146c4.netlify.app/older.html) for more insight). It is time for everybody to stop using `sp` and `raster` and to embrace `sf` and `stars/terra`.

In version 4 `lidR` now no longer uses `sp`, it uses `sf` and it no longer uses `raster`. It is now raster agnostic and works transparently with rasters from `raster`, `terra` and `stars`. These two changes meant we had to rewrite a large portion of the code base, which implies few backward incompatibilities. The backward incompatibilities are very small compared to the huge internal changes we implemented in the foundations of the code and should not even be visible for most users.

### Backward inconpatibilites

1. `lidR` no longer loads `raster` and `sp`. To manipulate `Raster*` and `Spatial*` objects returned by lidR users need to load `sp` and `raster` with:
    ```r
    library(sp)
    library(raster)
    library(lidR)
    ```

2. The formal class `LAS` no longer inherits the class `Spatial` from `sp`. It means, among other things, that a `LAS` object no longer has a slot `@proj4string` with a `CRS` from `sp`, or a slot `@bbox`. The CRS is now stored in the slot `@crs` in a `crs` object from `sf`. Former functions `crs()` and `projection()` inherited from `raster` are backward compatible and return a `CRS` or a `proj4string` from `sp`. However code that accesses these slots manually are no longer valid (but nobody was supposed to do that anyway because it was the purpose of the function `projection()`):
    ```r
    las@proj4string # No longer works
    las@bbox        # No longer works
    inherits(las, "Spatial") # Now returns FALSE
    ```

3. The formal class `LAScatalog` no longer inherits the class `SpatialPolygonDataFrame` from `sp`. It means, among other things, that a `LAScatalog` object no longer has a slot `@proj4string`, or `@bbox`, or `@polygons`. The slot `@data` is preserved and contains an `sf,data.frame` instead of a `data.frame` allowing backward compatibility of data access to be maintained. The syntax `ctg$attribute` is the way to access data, but statement like `ctg@data$attribute` are backward compatible. However, code that accesses other slots manually is no longer valid, like for the `LAS` class:
    ```r
    ctg@proj4string # No longer works
    ctg@bbox        # No longer works
    ctg@polygons    # No longer works
    inherits(ctg, "Spatial") # Now returns FALSE
    ```

4. `sp::spplot()` no longer works on a `LAScatalog` because a `LAScatalog` is no longer a `SpatialPolygonDataFrame`
    ```r
    spplot(ctg, "Max.Z")
    # becomes
    plot(ctg["Max.Z"])
    ```
    
5. `raster::projection()` no longer works on `LAS*` objects because they no longer inherit `Spatial`. Moreover, `lidR` no longer `Depends` on `raster` which means that `raster::projection()` and `lidR::projection` can mask each other. Users should use `st_crs()` preferentially. To use `projection` users can either load `raster` before `lidR` or call `lidR::projection()` with the explicit namespace.

    ```r
    library(lidR)
    projection(las) # works
    library(raster)
    projection(las) # no longer works
    ```

6. Serialized `LAS/LAScatalog` objects (i.e. stored in `.rds` or `.Rdata` files) saved with `lidR v3.x.y` are no longer compatible with `lidR v4.x.y`. Indeed, the structure of a `LAS/LAScatalog` object is now different mainly because the slot `@crs` replaces the slot `@proj4string`. Users may get errors when using e.g. `readRDS(las.rds)` to load back an R object. However we put safeguards in place so, in practice, it should be backward compatible transparently, and even repaired automatically in some circumstances. Consequently we are not sure it is a backward incompatibility because we handled and fixed all warnings and errors we found. In the worst case it is possible to repair a `LAS` object v3 with:
    ```r
    las <- LAS(las)
    ```

7. `track_sensor()` is not backward compatible because it is a very specific function used by probably just 10 people in the world. We chose not to rename it. It now returns an `sf` object instead of a `SpatialPointsDataFrame`.

### New modern functions

Former functions that return `Spatial*` objects from package `sp` should no longer be used. It is time for everybody to embrace `sf`. However, these functions are still in `lidR` for backward compatibility. They won't be removed except if package `sp` is removed from CRAN. It might happen on Jan 1st 2024, it might happen later. We do not know. New functions return `sf` or `sfc` objects. Old functions are not documented so new users won't be able to use them.

- `tree_metrics()` and `delineate_crowns()` are replaced by a single function `crown_metrics()` that has the same functionality, and more.
- `find_trees()` is replaced by `locate_trees()`.

Older functions that return `Raster*` objects from the `raster` package should no longer be used. It is time for everybody to embrace `terra/stars`. However, these functions are still in `lidR` for backward compatibility.  They won't be removed except if package `raster` is removed from CRAN. New functions return either a `Raster*`, a `SpatRaster`, or a `stars` object, according to user preference.

- `grid_metrics()` is replaced by `pixel_metrics()`
- `grid_terrain()`, `grid_canopy()`, `grid_density()` are replaced by `rasterize_terrain()`, `rasterize_canopy()`, `rasterize_density()` 

### New features

New functions are mostly convenient features that simplify some workflow aspects without introducing a lot of brand new functionality that did not already exist in `lidR` v3.

1. New geometry functions `st_convex_hull()` and `st_concave_hull()` that return `sfc`

2. New modern functions `st_area()`, `st_bbox()`, `st_transform()` and `st_crs()` inherited from `sf` for `LAS*` objects.

3. New convenient functions `nrow()`, `ncol()`, `dim()`, `names()` inherited from `base` for `LAS*` objects

4. New operators `$`,  `[[`, `$<-` and `[[<-` on `LASheader`. The following are now valid statements:
    ```r
    header[["Version Major"]]
    header[["Z scale factor"]] <- 0.001
    ```

5. Operators  `$`,  `[[`, `$<-` and `[[<-` on `LAS` can now access the `LASheader` metadata. The following are now valid statements:
    ```r
    las[["Version Major"]]
    las[["Z scale factor"]] <- 0.001
    ```

6. RStudio now supports auto completion for operator `$` in `LAS` objects. Yay!

7. New functions `template_metrics()`, `hexagon_metrics()`, `polygon_metrics()` that extend the concept of metrics further to any kind of template.

8. Functions that used to accept spatial vector or spatial raster as input now consistently accept any of `Spatial*`, `sf`, `sfc`, `Raster*`, `SpatRaster` and `stars` objects. This include `merge_spatial()`, `normalize_intensity()`, `normalize_height()`, `rasterize_*()`, `segment_trees()`, `plot_dtm3d()` and several others. We plan to support `SpatVector` in future releases.

9. Every function that supports a raster as input now accept an "on-disk" raster from `raster`, `terra` and `stars` i.e. a raster not loaded in memory. This includes rasterization functions, individual tree segmentation functions, `merge_spatial` and others, in particular `plot_dtm3d()` and `add_dtm3d()` that now downsample on-disk rasters on-the-fly to display very large DTMs. On-disk rasters were already generally supported in previous versions but not every function was properly optimized to handle such objects.

10. All the functions that return a raster (`pixel_metrics()` and `rasterize_*()`) are raster agnostic and can return rasters from `raster`, `terra` or `stars`. They have an argument `pkg = "raster|terra|stars"` to choose. The default is `terra` but this can be changed globally using:
    ```r
    options(lidR.raster.default = "stars")
    ```

11. New function `catalog_map()` that simplifies `catalog_apply()` to a large degree. Yet it is not as versatile as `catalog_apply()` but well suits around 80% of use cases. Applying a user-defined function to a collection of LAS files is now as simple as:
    ```r
    my_fun <- function(las, ...) {
      # do something with the point cloud
      return(something)
    }
    res <- catalog_map(ctg, my_fun, param1 = 2, param2 = 5)
    ```

12. Operator `[` on `LAS` object has been overloaded to clip a point-cloud using a `bbox` or a `sfc`
    ```r
    sub <- las[sfc]
    ```
  
13. `rasterize_terrain()` accepts an `sfc` as argument to force interpolation within a defined area.

14. `normalize_height()` now always interpolates all points. It is no longer possible to get an error that some points cannot be interpolated. The problem of interpolating the DTM where there is no data is still present but we opted for a nearest neighbour approach with a warning instead of a failure. This prevents the method from failing after hours of computation for special cases somewhere in the file collection. This also means we removed the `na.rm` option that is no longer relevant.

15. New functions `header()`, `payload()`, `phb()`, `vlr()`, `evlr()` to get the corresponding data from a `LAS` object.

16. New algorithm `shp_hline` and `shp_vline` for `segment_shapes()` [#499](https://github.com/r-lidar/lidR/issues/499)

17. New algorithm `mcc` for ground classification.

### Enhancement

1. The bounding box of the CHM computed with `rastertize_canopy()` or `grid_canopy()` is no longer affected by the `subcircle` tweak. See [#518](https://github.com/r-lidar/lidR/issues/518).

2. `readLAS()` can now read two or more files that do not have the same point format (see [#508](https://github.com/r-lidar/lidR/discussions/508))

3. `plot()` for `LAS` gains arguments `pal`, `breaks` and `nbreaks` similar to `sf`. Arguments `trim` and `colorPalette` are deprecated

### Fix

1. The metric `itot` from `stdmetrics_i` which generates troubles (see [#463](https://github.com/r-lidar/lidR/issues/463) [#514](https://github.com/r-lidar/lidR/issues/514)) is now `double` instead of `int`

### Documentation

- Man pages of `classify_*`, `rasterize_*`, `*_metrics`, `segment_*` and `normalize_*` were grouped.
- The pdf version of the manual contains more documentation (more functions) but is 20 pages shorter, meaning that we tidied and cleaned up the documentation.


