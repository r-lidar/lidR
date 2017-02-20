### lidR v1.2.0 (in development)

#### NEW FEATURES

* new function `lasground` for ground segmentation.
* new function `grid_tincanopy`. Canopy height model using Khosravipour et al. pitfree algorithm.
* new function `grid_hexametrics`. Area based approach in hexagonal cells.
* `lasnormalize` allows for "non-discretized" normalization i.e interpolating each points instead of using a raster.
* internally `lascheck` perform more tests to check if the header is in accordance with the data.

#### BUG FIXES

* [[#48](https://github.com/Jean-Romain/lidR/pull/48)] `gap_fraction_profile()` bug with negative values (thanks to Florian de Boissieu)
* [[#49](https://github.com/Jean-Romain/lidR/pull/49)]] typo error leading to the wrong metric in  `stdmetric_i` 
* [[#50](https://github.com/Jean-Romain/lidR/pull/50)]] typo error leading to the wrong metric in  `stdmetric` 



### lidR v1.1.0 (Release date: 2017-02-05)

#### NEW FEATURES

* `lastree()` for individual tree segmentation
* `readLAS()` gains a parameter `filter` from `rlas (>= 1.1.0)`
* `catalog_queries()` relies on `rlas (>= 1.1.0)`. It saves a lot of memory, it is 2 to 6 times faster and supports .lax files.

#### OTHER CHANGES

* `colorPalette` parameter in `plot.LAS()` now expects a list of colors instead of a function. Use `height.colors(50)` instead of `height.colors`
* The header of a LAS object is now an S4 class called `LASheader`
* The spatial interpolation method called `akima` is now called `delaunay` because it corresponds to what is actually computed.
* The spatial interpolation method called `akima` lost its parameter `linear`.
* The spatial interpolation method called `kriging` now performs a KNN kriging.
* `catalog_queries()` lost the parameter `...` all the fields are loaded by default.
* Removed `lasterrain()` which was not consistent with other functions and not useful.

#### BUG FIXES

* The header of LAS objects automatically updates `Number of point records` and `Number of nth return`.
* `lasnormalize()` updates the header and returns warnings for some behaviors
* [[#39](https://github.com/Jean-Romain/lidR/issues/39)] interpolation with duplicated ground points


### lidR v1.0.2 (Release date: 2016-12-31)

Third submission

* Change: explain LiDAR in the Description on Kurt Hornik's demand

### lidR v1.0.1 (Release date: 2016-12-30)

Second submission - rejected

* Change: split the package in two parts. 'lidR' rely on 'rlas' to read binary files.

### lidR v1.0.0 (Release date: 201-12-16)

First submission - rejected
