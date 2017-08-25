library(lidR)
library(lineprof)

las = readLAS("~/Documents/Haliburton dataset/Landscape LiDAR/CN_683_5016.las")

l = lineprof(grid_canopy(las))
shine(l)

# v1.2.1
# major issues:

l = lineprof(grid_canopy(las, subcircle = 0.2))
shine(l)

# v1.2.1
# major issues:
# - subcircled uses an insane amount of time (6 sec vs 1.4 sec for the aggregation)
# - subcircled allocate a huge amount of memory.

l = lineprof(grid_tincanopy(las, thresholds = 0))
shine(l)

# v1.2.1
# major issues
# - very slow with big file. delaunay: 6s, tsearch: 17s, tinfo: 1s