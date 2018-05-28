library(lidR)
library(profvis)

las = readLAS("~/Documents/ALS data/Haliburton dataset/Landscape LiDAR/CN_683_5016.las")

profvis(grid_canopy(las))

profvis(grid_canopy(las, subcircle = 0.2))

# v1.2.1
# major issues:
# - subcircled uses an insane amount of time (6 sec vs 1.4 sec for the aggregation)

profvis(grid_tincanopy(las, thresholds = 0))

# v1.2.1
# major issues
# - very slow with big file. delaunay: 23s, tsearch: 64s, tinfo: 5s. total 97 secondes