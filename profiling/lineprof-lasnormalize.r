library(lidR)
library(profvis)

las = readLAS("~/Documents/Haliburton dataset/Landscape LiDAR/CN_683_5016.las")

dtm = grid_terrain(las, method = "delaunay")
l = profvis(lasnormalize(las, dtm))
l

# v1.2.1
# major issues:
# - raster::extract is slow and allocate a huge amount of memory (800 Mb)
# - as.raster makes a lot of deep copies.

# v1.3.0
# minors issues
# - as.raster is the bottleneck (but function is more than 15 times faster than v1.2.1)

l = profvis(lasnormalize(las, method = "delaunay"))
shine(l)

# v1.2.1
# major issues:
# - see grid_terrain.