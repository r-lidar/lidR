% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_constant.R
\docType{data}
\name{asprs}
\alias{asprs}
\alias{LASNONCLASSIFIED}
\alias{LASUNCLASSIFIED}
\alias{LASGROUND}
\alias{LASLOWVEGETATION}
\alias{LASMEDIUMVEGETATION}
\alias{LASHIGHVEGETATION}
\alias{LASBUILDING}
\alias{LASLOWPOINT}
\alias{LASKEYPOINT}
\alias{LASWATER}
\alias{LASRAIL}
\alias{LASROADSURFACE}
\alias{LASWIREGUARD}
\alias{LASWIRECONDUCTOR}
\alias{LASTRANSMISSIONTOWER}
\alias{LASBRIGDE}
\alias{LASNOISE}
\title{ASPRS LAS Classification}
\format{
An object of class \code{integer} of length 1.

An object of class \code{integer} of length 1.

An object of class \code{integer} of length 1.

An object of class \code{integer} of length 1.

An object of class \code{integer} of length 1.

An object of class \code{integer} of length 1.

An object of class \code{integer} of length 1.

An object of class \code{integer} of length 1.

An object of class \code{integer} of length 1.

An object of class \code{integer} of length 1.

An object of class \code{integer} of length 1.

An object of class \code{integer} of length 1.

An object of class \code{integer} of length 1.

An object of class \code{integer} of length 1.

An object of class \code{integer} of length 1.

An object of class \code{integer} of length 1.

An object of class \code{integer} of length 1.
}
\usage{
LASNONCLASSIFIED

LASUNCLASSIFIED

LASGROUND

LASLOWVEGETATION

LASMEDIUMVEGETATION

LASHIGHVEGETATION

LASBUILDING

LASLOWPOINT

LASKEYPOINT

LASWATER

LASRAIL

LASROADSURFACE

LASWIREGUARD

LASWIRECONDUCTOR

LASTRANSMISSIONTOWER

LASBRIGDE

LASNOISE
}
\description{
A set of global variables corresponding to the point classification defined by the ASPRS for the
LAS format. Instead of remembering the classification table of the specification it is possible
to use one of these global variables.
}
\examples{
LASfile <- system.file("extdata", "example.laz", package="rlas")
las = readLAS(LASfile)
las2 = filter_poi(las, Classification \%in\% c(LASGROUND, LASWATER))

print(LASGROUND)
}
\keyword{datasets}
