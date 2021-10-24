#' @section Uniqueness:
#'
#' By default the tree IDs are numbered from 1 to n, n being the number of trees found. The problem
#' with such incremental numbering is that, while it ensures a unique ID is assigned for each tree in
#' a given point-cloud, it also guarantees duplication of tree IDs in different tiles or chunks when
#' processing a `LAScatalog`. This is because each chunk/file is processed independently of the others
#' and potentially in parallel on different computers. Thus, the index always restarts at 1 on each
#' chunk/file or chunk. Worse, in a tree segmentation process, a tree that is located exactly between
#' 2 chunks/files will have two different IDs for its two halves.
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
#' unique ID. This ID is not an integer but a 64-bit decimal number, which is suboptimal but at
#' least it is expected to be unique **if the gpstime attribute is consistent across files**.
#' If inconsistencies with gpstime are reported (for example gpstime records the week time and was
#' reset to 0 in a coverage that takes more than a week to complete), there is a (low) probability of
#' getting ID attribution errors.}
#' \item{bitmerge}{This method uses the XY coordinates of the highest point (apex) of a tree to
#' create a single 64-bit number with a bitwise operation. First, XY coordinates are converted to
#' 32-bit integers using the scales and offsets of the point cloud. For example, if the apex is at
#' (10.32, 25.64) with a scale factor of 0.01 and an offset of 0, the 32-bit integer coordinates are
#' X = 1032 and Y = 2564. Their binary representations are, respectively, (here displayed as 16 bits)
#' 0000010000001000 and 0000101000000100. X is shifted by 32 bits and becomes a 64-bit integer. Y is kept
#' as-is and the binary representations are unionized into a 64-bit integer like (here displayed as 32 bit)
#' 00000100000010000000101000000100 that is guaranteed to be unique. However R
#' does not support 64-bit integers. The previous steps are done at C++ level and the 64-bit binary
#' representation is reinterpreted into a 64-bit decimal number to be returned in R. The IDs thus generated
#' are somewhat weird. For example, the tree ID 00000100000010000000101000000100 which is 67635716 if
#' interpreted as an integer becomes 3.34164837074751323479078607289E-316 if interpreted as a decimal
#' number. This is far from optimal but at least it is guaranteed to be unique  **if all files have
#' the same offsets and scale factors**.}
#' }
#'
#' All the proposed options are suboptimal because they either do not guarantee uniqueness in all cases
#' (inconsistencies in the collection of files), or they imply that IDs are based on non-integers or
#' meaningless numbers. But at least it works and deals with some of the limitations of R.
#' @md
