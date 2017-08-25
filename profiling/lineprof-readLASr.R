library(lidR)
library(profvis)

l = profvis(readLAS("~/Documents/Haliburton dataset/Landscape LiDAR/CN_683_5016.las"))
l