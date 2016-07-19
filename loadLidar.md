---
layout: default
title: Structure of a Lidar object
---

Read a `.las` file to build a `Lidar` object with the `LoadLidar` function. `lidR` package only allows .las` files to be read.

	library(lidR)
	lidar = LoadLidar("myfile.las")

##  Basic structure

A `Lidar` object contains a `data.table` in the slot `@data` with the data read from the `.las` file. The following names enable access to the data and correspond to the las specification.

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

To save memory only useful data are loaded. `LoadLidar` can take an extra paramter `fields`. The option `fields` allows selection of fields to be loaded. Removing redundant fields saves memory. The option `minimal` loads only `X`, `Y`, `Z` coordinates and `gpstime` (if available). The option `standard` loads all the fields minus ̀`UserData`, `EdgeofFlighline` and `PointSourceID`. The option `all` loads everinthing.

## Dynamically computed fields

When a `Lidar` object is built, other information is automatically computed:

If the file contains a field `gpstime`

- `pulseID`: a number which identifies each pulse allowing the source beam for each point to be known
- `flightlineID`: a number which identifies the flightline allowing the flightline for each point to be known

If the file contains the fields `R` `G` and `B`

- `color`: a string containg the hexadecimal names of the RGB colors.

## Other data contained in a Lidar object

A `Lidar` object contains other information in the slots `@area`, `@pointDensity`, `@pulseDensity`:

	summary(lidar)

> Memory : 86 Mb 
>
> area : 350306.8 m^2 <br/>
> points : 805322 points<br/>
> pulses : 561704 pulses<br/> 
> point density : 2.3 points/m^2<br/> 
> pulse density : 1.6 pulses/m^2 <br/>


- `area`: is computed with a convex hull. It is only an approximation if the shape of the data is not convex.
- `points` and `pulse density` are computed with the computed area. Therefore it suffers from the same approximation issue as `area`.

## Header

A `Lidar` object also contains a slot `@header` containing the header of the `.las` file. See public documentation of `.las` file format for more information.
