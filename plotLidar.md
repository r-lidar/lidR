---
layout: default
title: Plot and visualize a Lidar object
---

lidR provides a simple `plot` function to plot a 3D a Lidar object. It is based on the `rgl` package. For more information see `?rgl::plot3d`.

## Default plotting

Without parameters it plots `X`,`Y`,`Z` in 3D with a classic color palette.

	plot(lidar)

![](images/plot3d_1.jpg)

## Change the color palette

	plot(lidar, colorPalette = "terrain.colors")

![](images/plot3d_2.jpg)


## Choose the field used to colorize

The parameter `color` expects the name of the field you want to use to colorize the points. Default is `Z`

	plot(lidar, color="Intensity", colorPalette = "heat.colors")

If your file contains RGB data, a field `color` is automatically added.

	plot(lidar, color="color")


## Outliers

The `trim` parameter enables trimming of values when outliers break the color palette range. For example `Intensity` often contains large outliers. The palette range would be too large and most of the values will be considered as "very low", so everything will appear in the same color. The parameter `trim` enables a given percentage of highest values to be ignored. In the following example the intensity values greater than the 99th percentile are all the same color.

	plot(lidar, color="Intensity", colorPalette = "heat.colors", trim=0.99)

![](images/plot3d_3.jpg)
