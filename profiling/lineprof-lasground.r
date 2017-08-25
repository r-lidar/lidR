library(lidR)
library(lineprof)

las = readLAS("~/Documents/Haliburton dataset/Landscape LiDAR/CN_683_5016.las")

LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
las = readLAS(LASfile)

l = lineprof(lasground(las))
shine(l)

# v1.2.1
# major issues
# - very slow with big file but entierely due to the morphological opening
# - can be improved reducing the amount of points