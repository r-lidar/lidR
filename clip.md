---
layout: default
title: Clip LiDAR data
---

The `clip` family functions enable to extract pieces of LiDAR data based on coordinates and shapes : 

    clipCircle(lidar, xcenter, ycenter, radius)
    clipRectangle(lidar, xleft, ybottom, xright, ytop)
    clipPolygon(lidar, x, y)
    
The option parameter `inside` enable to keep the data inside or outside the shape. the fefault behavior in `inside = TRUE`.

![](images/clipCircle.png) ![](images/clipRectangle.png) ![](images/clipPoly.png)
