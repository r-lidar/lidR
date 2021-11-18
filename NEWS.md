If you are viewing this file on CRAN, please check [the latest news on GitHub](https://github.com/r-lidar/lidR/blob/master/NEWS.md) where the formatting is also better

## lidR v4.0.0 (Release date: ...)

`rgdal` and `rgeos` will be retired on Jan 1st 2024. see [twitter](https://twitter.com/RogerBivand/status/1407705212538822656), [youtube](https://www.youtube.com/watch?v=cK08bxUJn5A) or see respective package descriptions on CRAN. Packages `raster` and `sp` are based on `rgdal`/`rgeos` and `lidR` was based on `raster` and `sp` because it was created before `sf`, `terra` and `stars`. This means that sooner or later `lidR` will run into trouble (actually it is more or less already the case). Consequently we modernized `lidR` by moving to `sf`, `terra`/`stars` and we are no longer depending on `sp` and `raster` anymore (see also [Older R Spatial Package](https://keen-swartz-3146c4.netlify.app/older.html) for more insight). It is time for everybody to stop using `sp` and `raster` and to embrace `sf` and `stars/terra`.

In version 4 `lidR` now no longer uses `sp`, it uses `sf` and it no longer uses `raster` it is now raster agnostic and works transparently with rasters from `raster`, `terra` and `stars`. These two changes implied to rewrite a large portion of the code base and imply few backward incompatibilities. The backward incompatibilities are very small compared to the huge internal changes we operated in the foundations of the code and should not even be visible for most users.

### Backward inconpatibilites

1. `lidR` no longer loads `raster` and `sp`. To manipulate `Raster*` and `Spatial*` objects returned by lidR users need to load `sp` and `raster` with:
  ```r
  library(lidR)
  library(sp)
  library(raster)
  ```

2. The formal class `LAS` no longer inherits the class `Spatial` from `sp`. It means, among others, that a `LAS` object no longer have a slot `@proj4string` with a `CRS` from `sp` nor a slot `@bbox`. The CRS is now stored in the slot `@crs` in a `crs` object from `sf`. Former functions `crs()` and `projection()` inherited from `raster` are backward compatible and return a `CRS` or a `proj4string` from `sp`. However codes that access these slots manually are no longer valid (but nobody was supposed to do that because ìt was the purpose of the function `projection()`):
  ```r
  las@proj4string # No longer works
  las@bbox        # No longer works
  ```

3. The formal class `LAScatalog` no longer inherits the class `SpatialPolygonDataFrame` from `sp`. It means, among others, that a `LAScatalog` object no longer have a slot `@proj4string` nor `@bbox` nor `@polygons`. The slot `@data` is preserved and contains an `sf,data.frame` instead of a `data.frame` allowing to maintain backward compatibility of data access. The syntax `ctg$attribute` is the way to access data, but statement like `ctg@data$attribute` are backward compatible. However code that accesses others slots manually are no longer valid like for the `LAS` class:
  ```r
  ctg@proj4string # No longer works
  ctg@bbox        # No longer works
  ctg@polygons    # No longer works
  ```

4. `sp::spplot()` no longer works on a `LAScatalog` because a `LAscatalog` is no longer a `SpatialPolygonDataFrame`
  ```r
  spplot(ctg, "Max.Z")
  # becomes
  plot(ctg["Max.Z"])
  ```
  
5. Serialized `LAS/LAscatalog` objects (i.e. stored in `.rds` or `.Rdata` files) saved with `lidR v3.x.y` are no longer compatible with `lidR v4.x.y`. Indeed the structure of a `LAS/LAScatalog` object is now different mainly because the slot `@crs` replaces the slot `@proj4string`. Users may get errors when using e.g. `readRDS(las.rds)` to load back an R object. However we put safeguards so, in practice, it should be backward compatible transparently and even repaired automatically in some circumstances. Consequently we are not sure it is a backward incompatibility because we handled and fixed all warnings and error we found. In the worst case it is possible to repair a `LAS` object v3 with:
  ```r
  las <-  LAS(las)
  ```

6. `track_sensor()` is not backward compatible because it is a very specific function used by probably like 10 peoples in the world. We chose to do not rename it. It now returns an `sf` object instead of a `SpatialPointsDataFrame`.

### New modern functions

Former functions that return `Spatial*` objects from package `sp` should no longer be used. It is time for everybody to embrace `sf`. These functions are still in `lidR` for backward compatibility though. They won't be removed except if package `sp` is removed from CRAN. It might happen on Jan 1st 2024, it might happen later. We do not know. New functions return `sf` or `sfc` objects. Old function are not documented so new users won't be able to use them.

- `tree_metrics()` and `delineate_crowns()` are replaced by a single function `crown_metrics()` that do the same and more.
- `find_trees()` is replaced by `locate_trees()`.

Former functions that return `Raster*` objects from package `raster` should no longer be used. It is time for everybody to embrace `terra/stars`. These functions are still in `lidR` for backward compatibility though.  They won't be removed except if package `raster` is removed from CRAN. New functions return either a `Raster*` a `SpatRaster` or a `stars` as preferred by users.

- `grid_metrics()` is replaced by `pixel_metrics()`
- `grid_terrain()`, `grid_canopy()`, `grid_density()` are replaced by `rasterize_terrain()`, `rasterize_canopy()`, `rasterize_density()` 

### New features

New functions are mostly convenient features that simplify some workflow without introducing a lot of brand new functionality that would not already exist in `lidR` v3.

1. New geometry functions `st_convex_hull()` and `st_concave_hull()` that return `sfc`

2. New modern functions `st_area()`, `st_bbox()` and `st_transform()`, `st_crs()` inherited from `sf` for `LAS*` objects.

3. New convenient functions `nrow()`, `ncol()`, `dim()`, `names()` inherited from `base` for `LAS*` objects

4. New operators `$` and `[[` on `LASheader`. The following are now valid statements:
  ```r
  header[["Version Major"]]
  header[["Z scale factor"]]
  ```

5. Operators `$` and `[[` on `LAS` can now access the `LAsheader` data. The following are now valid statements:
  ```r
  las[["Version Major"]]
  las[["Z scale factor"]]
  ```

6. RStudio now supports auto completion for operator `$` in `LAS` object. Yay!

7. New functions `template_metrics()`, `hexagon_metrics()`, `polygon_metrics()` that extend the concept of metrics further to any kind of template.

8. Functions that used to accept spatial vector or spatial raster in input now consistently accept any of `Spatial*`, `sf`, `sfc`, `Raster*`, `SpatRaster` and `stars` objects. This include `merge_spatial()`, `normalize_intensity()`, `normalize_height()`, `rasterize_*()`, `segment_trees()`, `plot_dtm3d()` and several others.

9. Every function that support a raster as input now accept an "on-disk" raster from `raster`, `terra` and `stars`  i.e. a raster not loaded in memory. This include rasterization functions, individual tree segmentation functions, `merge_spatial` and others in particular `plot_dtm3d()` and `add_dtm3d()` that now downsample on-disk rasters on-the-fly to display very large DTMs. This was already the case in previous versions but not every function was properly optimized to handle such objects.

10. All the functions that return a raster (`pixel_metrics()` and `rasterize_*()`) are raster agnostic and can return rasters from `raster`, `terra` or `stars`. They have an argument `pkg = "raster|terra|stars"` to choose. The default is `stars` but this can be changed globally using:
  ```r
  options(lidR.raster.default = "terra")
  ```

11. New function `catalog_map()` that simplifies a lot `catalog_apply()`. Yet it is not as versatile as `catalog_apply()` but suits well in 80% of use cases. Applying a user-defined function to a collection of LAS files is now as simple as:
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

14. `normalize_height()` now always interpolates every points. It is not longer possible to get an error that some points cannot be interpolated. The problem of interpolating the DTM where there is no data is still present but we opted for a nearest neighbour approach with a warning instead of a failure. This prevent the method from failing after hours of computation for special cases somewhere in the file collection. This also means we removed the `na.rm` option that is no longer relevant.

15. New functions `header()`, `payload()`, `phb()`, `vlr()`, `evlr()` to get the corresponding data from a `LAS` object.

### Documentation

- Man pages of `classify_*`, `rasterize_*`, `*_metrics`, `segment_*` and `normalize_*` were grouped.
- The pdf version of the manual contains more documentation (more function) but is 20 pages shorter meaning that we tidy and cleanup the documentation


