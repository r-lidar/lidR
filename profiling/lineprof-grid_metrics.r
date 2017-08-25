library(lidR)
library(profvis)

las = readLAS("~/Documents/Haliburton dataset/Landscape LiDAR/CN_683_5016.las")

l = profvis(grid_metrics(las, mean(Z)))
l

l = profvis(grid_metrics(las, .stdmetrics))
l
