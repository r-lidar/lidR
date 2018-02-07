library(lidR)
library(profvis)

las = readLAS("~/Documents/Haliburton dataset/Landscape LiDAR/CN_683_5016.las")

LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
las = readLAS(LASfile)

ws = seq(3,21, 3)
th = seq(0.1, 2, length.out = length(ws))

l = profvis(lasground(las, "pmf", ws, th))
l

# v1.2.1
# major issues
# - very slow with big file but entierely due to the morphological opening
# - can be improved reducing the amount of points