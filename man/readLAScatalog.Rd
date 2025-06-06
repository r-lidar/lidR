% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/io_readLAScatalog.R
\name{readLAScatalog}
\alias{readLAScatalog}
\alias{readALScatalog}
\alias{readTLScatalog}
\alias{catalog}
\title{Create an object of class LAScatalog}
\usage{
readLAScatalog(
  folder,
  progress = TRUE,
  select = "*",
  filter = "",
  chunk_size = 0,
  chunk_buffer = 30,
  ...
)

readALScatalog(folder, ...)

readTLScatalog(folder, ...)

catalog(folder, ...)
}
\arguments{
\item{folder}{string. The path of a folder containing a set of las/laz files.
Can also be a vector of file paths.}

\item{progress, select, filter, chunk_size, chunk_buffer}{Easily accessible processing
options tuning. See \link{LAScatalog-class} and \link{engine_options}.}

\item{\dots}{Extra parameters to \link[base:list.files]{list.files}. Typically
\code{recursive = TRUE}. Propagates also to \code{readLAScatalog}}
}
\value{
A \code{LAScatalog} object
}
\description{
Create an object of class \link[=LAScatalog-class]{LAScatalog} from a folder
or a collection of filenames. A LAScatalog is a representation of a collection
of las/laz files. A computer cannot load all the data at once. A \code{LAScatalog}
is a simple way to manage all the files sequentially. Most functions from
\code{lidR} can be used seamlessly with a LAScatalog using the internal
\code{LAScatalog} processing engine. To take advantage of the \code{LAScatalog}
processing engine the user must first adjust some processing options using the
\link[=engine_options]{appropriate functions}. Careful reading of the
\link[=LAScatalog-class]{LAScatalog class documentation} is required to use the
\code{LAScatalog} class correctly.\cr\cr
\code{readLAScatalog} is the original function and always works. Using one of the \code{read*LAScatalog} functions
adds information to the returned object to register a point-cloud type. Registering the correct point
type \strong{may} improve the performance of some functions by enabling users to select an appropriate spatial index.
See \link[=lidR-spatial-index]{spatial indexing}. Notice that by legacy and for backwards-compatibility
reasons \code{readLAScatalog()} and \code{readALSLAScatalog()} are equivalent because lidR was originally designed
for ALS and thus the original function \code{readLAScatalog()} was (supposedly) used for ALS.
}
\examples{
# A single file LAScatalog using data provided with the package
LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
ctg = readLAScatalog(LASfile)
plot(ctg)

\dontrun{
ctg <- readLAScatalog("</path/to/folder/of/las/>")

# Internal engine will sequentially process chunks of size 500 x 500 m
opt_chunk_size(ctg) <- 500

# Internal engine will align the 500 x 500 m chunks on x = 250 and y = 300
opt_alignment(ctg) <- c(250, 300)

# Internal engine will not display a progress estimation
opt_progress(ctg) <- FALSE

# Internal engine will not return results into R.
# Instead it will write results in files.
# Files will be named e.g.
# filename_256000_1.ext
# filename_257000_2.ext
# filename_258000_3.ext
# ...
opt_output_files(ctg) <- "/path/filename_{XBOTTOM}_{ID}"

# More details in the documentation
help("LAScatalog-class", "lidR")
help("engine_options", "lidR")
}
}
