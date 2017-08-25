library(lidR)
library(lineprof)

las = readLAS("~/Documents/Haliburton dataset/Landscape LiDAR/CN_683_5016.las")

l = lineprof(lasdecimate(las, 0.5))
shine(l)
