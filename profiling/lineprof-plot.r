library(lidR)

LASfile <- "~/Documents/Haliburton dataset/Megaplot/Megaplot_HD.las"
las = readLAS(LASfile, select = "")

profvis::profvis(plot(las))

plot(las)

