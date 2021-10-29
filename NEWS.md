If you are viewing this file on CRAN, please check [the latest news on GitHub](https://github.com/Jean-Romain/lidR/blob/master/NEWS.md) where the formatting is also better

## lidR v4.0.0 (Release date: ...)

### Context 

`rgdal` and `rgeos` will be retired on Jan 1st 2024. see [twitter](https://twitter.com/RogerBivand/status/1407705212538822656), [youtube](https://www.youtube.com/watch?v=cK08bxUJn5A) or see respective package description on CRAN. `raster` and `sp` are based on `rgdal`/`rgeos` and `lidR` was based on `raster` and `sp` because it was created before `sf`, `terra` and `stars`. This means that sooner or later `lidR` will run into trouble (actually it is more or less already the case). Consequently we had to move to `sf`, `terra`/`stars` without depending on `sp` and `raster` anymore (see also [Older R Spatial Package](https://keen-swartz-3146c4.netlify.app/older.html) for more insight).

`lidR` now no longer uses `sp` it only uses `sf`. `lidR` now no longer uses `raster` it is now raster agnostic and works transparently with rasters from `raster`, `terra` and `stars`. These two changes implied to rewrite a large portion of the code but implies few backward incompatibilities and should not even be visible for most users.

### Backward inconpatibilites

As mentionned the backward incompatibilities are minor and should not be visible in most cases.

1. `lidR` no longer loads `raster` and `sp`. For old code to work one need to load `sp` and `raster` with:
  ```r
  library(lidR)
  library(sp)
  library(raster)
  ```

2. The formal class `LAS` no longer inherits the class `Spatial` from `sp`. It means a `LAS` object no longer have a slot `@proj4string` nor `@bbox`. The CRS is now stored in the slot `@crs` in a `crs` object from `sf`. Former functions `crs()` and `projection()` are backward compatible but code that accesses these slots manually are no longer valid (but nobody was supposed to do that):
  ```r
  las@proj4string # No longer works
  las@bbox        # No longer works
  ```

3. The formal class `LAScatalog` no longer inherits the class `SpatialPolygonDataFrame` from `sp`. It means a `LAS` object no longer have a slot `@proj4string` nor `@bbox` nor `@polygons`. The slot `@data` is preserved and contains an `sf,data.frame`. Few operation should no longer work  (but nobody was supposed to do that):
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

5. `track_sensor()` is not backward compatible because it is a very specific function used by probably 10 peoples in the world. We choose to do not rename it. It now returns an `sf` object instead of a `SpatialPointsDataFrame`

### Replacement function

Former functions that return `Spatial*` objects from `sp` should no longer be used. It is time for everybody to embrace `sf`. These functions are still in `lidR` for backward compatibility though. They won't be removed expect if package `sp` is removed from CRAN. It might happen on Jan 1st 2024, it might happen later. We do no know. New functions return `sf` or `sfc` objects

- `tree_metrics()` and `delineate_crowns()` are replaced by a single function `crown_metrics()` that do the same and more
- `find_trees()` is replaced by `locate_trees()`

Former functions that return `Raster*` objects from `raster` should no longer be used. It is time for everybody to embrace `terra/stars`. These functions are still in `lidR` for backward compatibility though. New functions return either a `Raster*` a `SpatRaster` or a `stars`

- `grid_metrics()` is replaced by `pixel_metrics()`
- `grid_terrain()`, `grid_canopy()`, `grid_density()` are replaced by `rasterize_terrain()`, `rasterize_canopy()`, `rasterize_density()` 

### New feature

New functions are mostly convenient features that simplify some workflow without introducing functionality that would not already exist.

1. New functions `st_convex_hull`()  and `st_concave_hull()` that return `sfc`

2. New functions `st_area()`, `st_bbox()` and `st_transform()` inherited from `sf` for `LAS*` objects

3. New functions `nrow()`, `ncol()`, `dim()`, `names()` inherited from `base` for `LAS*` objects

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

7. New functions `template_metrics()`, `hexagon_metrics()`, `polygon_metrics()` that extend the concept of metrics further to any kind of layout

8. Functions that used to accept spatial vector or spatial raster in input now consistently accept `Spatial`, `sf`, `sfc`, `Raster*`, `SpatRaster` and `stars` objects. This include `merge_spatial()`, `normalize_intensity()`, `normalize_height()`, `rasterize_*()`, `segment_trees()`, `plot_dtm3d()` and several others.

9. `plot_dtm3d()` and `add_dtm3d()` now support on disk rasters from `raster`, `terra` and `stars` and downsample them on-the-fly to display large DTMs

10. All the functions that return a raster (`pixel_metrics()` and `rasterize_*()`) are raster agnostic and can return raster from `raster`, `terra` or `stars`. The default is `stars` but this can be changed
  ```r
  options(lidR.raster.default = "terra")
  ```

11. New function `catalog_map()` which simplifies a lot `catalog_apply()`. Yet it is not as versatile as `catalog_apply()` but suits well in 80% of use cases. Applying a user-defined function to a collection of LAS files is now as simple as:
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

### Documentation

- Man pages of `classify_*`, `rasterize_*`, `*_metrics`, `segment_*` and `normalize_*` were grouped.


