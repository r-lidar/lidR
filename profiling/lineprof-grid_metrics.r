library(lidR)
library(lineprof)

las = readLAS("~/Documents/Haliburton dataset/Landscape LiDAR/CN_683_5016.las")

l = lineprof(grid_metrics(las, mean(Z)))
shine(l)

l = lineprof(grid_metrics(las, .stdmetrics))
shine(l)
