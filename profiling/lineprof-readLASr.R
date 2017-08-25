library(lidR)
library(lineprof)

l = lineprof(readLAS("~/Documents/Haliburton dataset/Landscape LiDAR/CN_683_5016.las"))
shine(l)
