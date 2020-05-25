#' @section Uniqueness:
#'
#' By default the tree IDs are numbered from 1 to n, n being the number of trees found. The problem
#' with such incremental numbering is that, while it ensures a unique ID is assigned for each tree in
#' a given point-cloud, it also guarantees duplication of tree IDs in different tiles or chunks when
#' processing a `LAScatalog`. This is because each file is processed independently of the others and potentially
#' in parallel on different computers. Thus, the index always restarts at 1 on each file or chunk. Worse,
#' in a tree segmentation process, a tree that is located exactly between 2 files will have two different
#' IDs for its two halves.
#'
#' This is why we introduced some uniqueness strategies that are all imperfect and that should be seen
#' as experimental. Please report any troubleshooting. Using a uniqueness-safe strategy ensures that
#' trees from different files will not share the same IDs. Moreover, it also means that two halves of a tree
#' on the edge of a processing chunk will be assigned the same ID.
#'
#' \describe{
#' \item{incremental}{Number from 0 to n. This method **does not** ensure uniqueness of the IDs. This
#' is the legacy method.}
#' \item{gpstime}{This method uses the gpstime of the highest point of a tree (apex) to create a
#' unique ID. This ID is not an integer but a 64-bit decimal number which is suboptimal but at
#' least it is exepected to be unique **if the gpstime attribute is consistent across files**.}
#' If inconsistencies with gpstime are reported (for example gpstime records the week time and was
#' reset to 0 in a coverage that takes more than a week to complete), there is a (low) probability to get
#' ID attribution errors.
#' \item{bitmerge}{This method uses the XY coordinates of the highest point (apex) of a tree to
#' create a single number with a bitwise operation. First, XY coordinates are converted to integers using the
#' scales and offsets of the point-cloud. Then the ID is computed with X * 2^32 + Y to combine twice the 32-bits
#' of information into a 64-bit number. For example, if the apex is at (10.32, 25.64)
#' with a scale factor of 0.01 and an offset of 0, the integer coordinates are X = 1032 and Y = 2564
#' and the ID is 4432406252036. Such methods return a 64-bit integer but because 64-bit integers do
#' not exist in R it is converted to a 64-bit decimal number that is guaranteed to be unique
#' **if all files have the same offsets and scale factors**.}
#' }
#'
#' All the proposed options are suboptimal because they either do not guarantee uniqueness in all cases
#' (inconsistencies in the collection of files), or they imply that IDs are based on non-integers or
#' meaningless numbers. But at the very least we expect this to work for simple cases.
