#' @section Uniqueness:
#'
#' By default the trees IDs are numbered from 1 to n, n being the number of tree found. The problem
#' with such incremental numbering is that, while it ensures to get a unique ID for each tree in
#' a given point-cloud it also gurantees to have duplicated tree IDs in different tiles or chunks when
#' processing a `LAScatalog` because each file is processed independently of the others and potentially
#' in parallel on different computers. Thus the index always restart to 1 on each file or chunk. Worst,
#' in a tree segmentation process, a tree that belongs exactly between 2 files will have two different
#' IDs for its two halfs.
#'
#' This is why we introduced some uniqueness strategies that are all imperfect and that should be seen
#' as experimental. Please report any troubleshooting. Using a uniqueness-safe strategy it ensures that
#' trees from different files will not share the same IDs. Moreover it ensure that two halfs of a trees
#' on the edge of a processing chunk will be attributed with the same ID.
#'
#' \describe{
#' \item{incremental}{Number from 0 to n. This methods **does not** ensure unicity of the IDs. This
#' is the legacy method.}
#' \item{gpstime}{This method uses the gpstime of the highest point of a tree (apex) to create a
#' unique ID. This ID is not a integer but a 64 bits decimal number which is suboptimal but at
#' least it is exepected to be unique  **if the gpstime attribute is consistant accross files**.}
#' If inconsistancies with gpstime are reported (for example gpstime records the week time and was
#' reset to 0 in a coverage that takes more than a week to do) there is a (low) probability to get
#' IDs attribution errors.
#' \item{bitmerge}{This method uses the XY coordinates of the highest point of a tree (apex) to
#' create a single number with a bitwise operation. First XY are converted back to integers using the
#' scales and offsets of point-cloud. Then the ID is computed with X * 2^32 + Y
#' to combine twice 32 bits of information into a 64 bits number. For example if the apex is at (10.32, 25.64)
#' with a scale factor of 0.01 and an offset of 0 the integer coordinates are X = 1032 and Y = 2564
#' and the ID is 4432406252036. Such methods returns a 64 bits integer but because 64 bits integer do
#' not exist in R it is converted to 64 bits decimal number that is guaranteed to be unique
#' **if all files have the same offsets and scale factors**.}
#' }
#'
#' All the proposed options are suboptimal because they either do not guarantee uniqueness in all cases
#' (inconsitancies in the collection of files) and they implies that IDs are based on non integers
#' meaningless numbers. But at least it should work in simple cases.
