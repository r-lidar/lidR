library(lidR)

LASfile <- "~/Documents/Haliburton dataset/Megaplot/Megaplot_HD.las"
las = readLAS(LASfile, select = "i")

profvis::profvis(plot(las, backend = "rgl"))

profvis::profvis(plot(las, backend = "pcv"))

# v1.3.0
# major issues:
# - data.table selection takes the most of the time. This is crazy
# - setcolor is much too long
# - rgl requies so much memory

# v1.4.0
# solution to previous issues
# - Better colum selection
# - new package PointCloudViewer