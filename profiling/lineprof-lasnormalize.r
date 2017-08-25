library(lidR)
library(lineprof)

las = readLAS("~/Documents/Haliburton dataset/Landscape LiDAR/CN_683_5016.las")

dtm = grid_terrain(las, method = "delaunay")

l = lineprof(lasnormalize(las, dtm))
shine(l)

# v1.2.1
# major issues:
# - raster::extract is slow and allocate a huge amount of memory (800 Mb)
# - as.raster makes a lot of deep copies.

l = lineprof(lasnormalize(las, method = "delaunay"))
shine(l)

# v1.2.1
# major issues:
# - see grid_terrain.