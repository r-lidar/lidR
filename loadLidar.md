---
layout: default
title: Structure of a Lidar object
---

Read a `.las` file to build a `Lidar` object with the `LoadLidar` function. `lidR` package allows to read only `.las` files.

	library(lidR)
	lidar = LoadLidar("myfile.las")

##  Basic structure

A `Lidar` object contains a `data.table` in the slot `@data` with the data read from the `.las` file. The following names enable to acces the data and correspond to the las specification.

- `X` `Y` `Z` `Intensity` and `gpstime`
- `ReturnNumber`
- `NumberOfReturns`
- `ScanDirectionFlag`
- `EdgeofFlightline`
- `Classification`
- `ScanAngle`
- `UserData`
- `PointSourceID`
- `R` `G` and `B`

To save memory only the really usefull data are loaded. `LoadLidar` can take and extra paramter `fields`. The option `fields` enable to select which fields will be loaded. Removing useless field allows to save memory. The option `minimal` load only `X`, `Y`, `Z`  and `gpstime` (if avaible). The option `standard` load all the fiels minus ̀`UserData`, `EdgeofFlighline` and `PointSourceID`. The option `all` load everinthing.

## Dynamically computed fields

When a `Lidar` object is built other informations are automatically computed:

If the file contains a field `gpstime`

- `pulseID`: a number which identifies each pulse allowing to know from which beam a point comes
- `flightlineID`: a number which identifies the flightline allowing to know from which flighline a point comes

If the file contains the fields `R` `G` and `B`

- `color`: a string containg the hexadecimal names of the RGB colors.

## Other data contained in a Lidar object

A `Lidar` object contains other informations in slots `@area`, `@pointDensity`, `@pulseDensity`:

	summary(lidar)

> Memory : 86 Mb 
>
> area : 350306.8 m^2 <br/>
> points : 805322 points<br/>
> pulses : 561704 pulses<br/> 
> point density : 2.3 points/m^2<br/> 
> pulse density : 1.6 pulses/m^2 <br/>


- `area`: is computed with a convex hull. It is only an approximation if the shape of the data is not convex.
- `points` and `pulse density` are computed with the computed area. Therefore it suffers of the same issue

## Header

A `Lidar` object also contains a slot `@header` containing the header of the `.las` file. See public documentation of `.las` file format for more information
