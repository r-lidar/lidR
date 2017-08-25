library(lidR)
library(lineprof)

las = readLAS("~/Documents/Haliburton dataset/Landscape LiDAR/CN_683_5016.las")

l = lineprof(grid_terrain(las, method = "delaunay"))
shine(l)

# v1.2.1
# major issues:
# - convex hull takes more times to run than the actual interpolation (~2.3 vs ~1.3 sec)
# - convex hull requieres almost the same amount of memory than the actual interpolation (~500 vs ~600 Mb)

# v1.3.0
# major issues:
# - tinfo allocate to much memory. It could be better.
# minor issues
# - make_grid makes too many deep copies. can be improved at the C++ level

l = lineprof(grid_terrain(las, method = "knnidw"))
shine(l)

# v1.2.1
# major issues:
# - convex hull takes more times to run than the actual interpolation (~2.3 vs ~1.3 sec)
# - convex hull requieres almost the same amount of memory than the actual interpolation (~500 vs ~600 Mb)

# v1.3.0
# minor issues:
# - make_grid makes too many deep copies. can be improved at the C++ level
# - can we speed-up knnidw?

LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
las = readLAS(LASfile)

l = lineprof(grid_terrain(las, method = "kriging"))
shine(l)

# v1.2.1
# major issues
# - extremely slow but not because of lidR code. All the computation time is kriging