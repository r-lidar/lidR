---
layout: default
title: Plot and visualize a Lidar object
---

lidR provide a simple `plot` function to plot a in 3D a Lidar object. It is based on `rgl` package. For more information see `?rgl::plot3d`.

## Default plotting

Without parameters it plot `X`,`Y`,`Z` in 3D with a classical color palette.

	plot(lidar)

![](images/plot3d_1.jpg)

## Change the color palette

	plot(lidar, colorPalette = "terrain.colors")

![](images/plot3d_2.jpg)


## Choose the field used to colorize

The parameter `color` expect the name of the field you want to use to colorize the points. Default is `Z`

	plot(lidar, color="Intensity", colorPalette = "heat.colors")

If your file contains RGB data. A field `color` has automatically been added.

	plot(lidar, color="color")


## Outliers

The `trim` parameter enables to trim values when ouliers break the color palette range. For example `Intensity` often contains large ouliers. The range of the palette would be to large and the most a the values will be considered as "very low" and every thing will apear in the same color. The parameter `trim` enable to do not take into account a given percentage of highest values. In the following example the intensity values greater than the 99th percentile are all the same color.

	plot(lidar, color="Intensity", colorPalette = "heat.colors", trim=0.99)

![](images/plot3d_3.jpg)
