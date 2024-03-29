% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/merge_spatial.R
\name{merge_spatial}
\alias{merge_spatial}
\title{Merge a point cloud with a source of spatial data}
\usage{
merge_spatial(las, source, attribute = NULL)
}
\arguments{
\item{las}{An object of class \code{LAS}}

\item{source}{An object of class \verb{SpatialPolygons*} or \code{sf} or \code{sfc} or \code{RasterLayer} or
\code{RasterStack} or \code{RasterBrick} or \code{stars}.}

\item{attribute}{character. The name of an attribute in the table of attributes or
the name of a new column in the LAS object. Not relevant for RGB colorization.}
}
\value{
a \code{LAS} object
}
\description{
Merge a point cloud with a source of spatial data. It adds an attribute along each point based on
a value found in the spatial data. Sources of spatial data can be a \verb{SpatialPolygons*}, an \code{sf}/\code{sfc},
a \verb{Raster*}, a \code{stars}, or a \code{SpatRaster}.\cr
\itemize{
\item{\verb{SpatialPolygons*}, \code{sf} and \code{sfc}: it checks if the points belongs within each polygon. If
the parameter \code{attribute} is the name of an attribute in the table of attributes it assigns
to the points the values of that attribute. Otherwise it classifies the points as boolean.
TRUE if the points are in a polygon, FALSE otherwise.}
\item{\code{RasterLayer}, single band \code{stars} or single layer \code{SpatRaster}: it attributes to each point
the value found in each pixel of the raster}.
\item{\code{RasterStack}, \code{RasterBrick}, multibands \code{stars} or multilayer \code{SpatRaster} must have 3
layers for RGB colors. It colorizes the point cloud with RGB values.}
}
}
\examples{
LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
shp     <- system.file("extdata", "lake_polygons_UTM17.shp", package = "lidR")

las   <- readLAS(LASfile, filter = "-keep_random_fraction 0.1")
lakes <- sf::st_read(shp, quiet = TRUE)

# The attribute "inlake" does not exist in the shapefile.
# Points are classified as TRUE if in a polygon
las    <- merge_spatial(las, lakes, "inlakes")     # New attribute 'inlakes' is added.
names(las)

forest <- filter_poi(las, inlakes == FALSE)
#plot(forest)

# The attribute "LAKENAME_1" exists in the shapefile.
# Points are classified with the values of the polygons
las <- merge_spatial(las, lakes, "LAKENAME_1")     # New column 'LAKENAME_1' is added.
names(las)
}
