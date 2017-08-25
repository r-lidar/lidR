library(lidR)
library(profvis)

las = readLAS("~/Documents/Haliburton dataset/Landscape LiDAR/CN_683_5016.las")

l = profvis(lidR:::lascheck(las@data, las@header))
l
