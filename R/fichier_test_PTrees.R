library(data.table)
library(lidR)
library(rlas)

LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
las = readLAS(LASfile, select = "xyz", filter = "-drop_z_below 0")
las = LAS(las@data[las@data$Z > 5])

k_values = c(5,6,7,8,10,12,15,20,25,30,40,60,80,100)
res = lidR:::lastrees_PTrees( las, k_values  )

#treeRes = LAS(las@data[,idTree := res], las@header)
#plot(treeRes, color = "idTree", colorPalette = random.colors(200))





