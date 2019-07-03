#' Parallel computation in lidR
#'
#' This document explain how to process point clouds taking advantage of parallel processing in the
#' lidR package. The lidR package has two level of parallelism and this is why it is difficult to
#' understand how it works. This page aims to provide user a clear overview of how to take advantage
#' of multicore processing even if user is not confortable with parallelism concept.
#'
#' @section Algorithm-based parallelism:
#' When processing a point cloud we are applying an algorithm on data. This algorithm may or may not be
#' natively parallel. In lidR some algorithm are fully computed in parallel, some are not because they are
#' not parallelisable and some are only partially parallized. It means that some portions of the code
#' are computed in parallel and some are not. When an algorithm is natively parallel in lidR it is alway
#' a C++ based parallelisation with OpenMP. The advantage is that the computation is faster without any
#' consequence for the memory usage because the memory is shared between the workers. In short
#' algorithm-based parallelism provides a significant gain without any cost for your R session and
#' your system (but obviously there is a greater workload for the processors). By default lidR uses
#' the half of your cores but you can control this with \link{set_lidr_threads}. For example the \link{lmf}
#' algorithm is a natively parallel algorithm. The following code is computed in parallel:
#' \preformatted{
#' las  <- readLAS("file.las")
#' tops <- tree_detection(las, lmf(2))
#' }
#' However, as said above, all algorithms are not parallelized or even parallelizable. For example
#' \link{li2012} is not parallelized. The following code is computed in serially:
#' \preformatted{
#' las <- readLAS("file.las")
#' dtm <- lastrees(las, li2012())
#' }
#' To know which algorithm is or is not paralellized user can refer to the documentation or used the
#' function \link{is.parallelised}.
#' \preformatted{
#' is.parallel(lmf(2))   #> TRUE
#' is.parallel(li2012()) #> FALSE
#' }
#' @section chunk-based parallelism:
#' When processing a LAScatalog, the internal engine splits the dataset into chunks and each chunk is
#' read an processed sequentially in a loop. But actually this loop can be parallelised with the
#' \code{future} package. By defaut the chunks are processed sequentially but they can be processed
#' in parallel by registering an evaluation strategy. For example the following code is evaluated
#' sequentially:
#' \preformatted{
#' ctg <- readLAScatalog("folder/")
#' out <- grid_metrics(ctg, mean(Z))
#' }
#' But this one is evaluated in parallel with two cores:
#' \preformatted{
#' library(future)
#' plan(multisession, workers = 2L)
#' ctg <- readLAScatalog("folder/")
#' out <- grid_metrics(ctg, mean(Z))
#' }
#' With a chunk-based parallelism any algorithm can be parallelised by processing several subsets of
#' a dataset.  However there is a strong cost with this type of parallelism. When processing several
#' chunks at a time, the computer needs to load the corresponding point clouds. Assuming user processes
#' one square kilometer chunks in parallel with 4 cores, it means that 4 chunks are loaded in the computer
#' memory. This may be too much and the speed-up is not guaranteed since there are some overhead at
#' reading several files at a time. Once this point is understood, the chunk-based parallelism is very
#' powerful since all the algorithms can be parallelised no matter they are natively parallel or not.
#'
#' @section Nested parallelism - part 1:
#' Previous sections stated that some algorithms are natively parallel such as \link{lmf} and some are
#' not such as \link{li2012}. Anyway user can split the dataset into chunks to process simultaneously
#' these chunks with the LAScatalog processing engine. Let assume that user's computer have four cores,
#' what happens in this case:
#' \preformatted{
#' library(future)
#' plan(multisession, workers = 4L)
#' set_lidr_threads(4L)
#' ctg <- readLAScatalog("folder/")
#' out <- tree_detection(ctg, lmf(2))
#' }
#' Here the catalog will be split into chunks that will be process in parallel. And each computation
#' implies itself a parallelized task. This is a nested parallism task and it is bad! Hopefully the lidR
#' package handle such case and chose by default to give precedence to chunk-based paralellism. In this
#' case chunks will be processed in parallel and the points will be processed serially. The question
#' of nested parallel loop does not matter. The catalog processing engine has precedence rules that
#' guaranteed to avoid nested paralellism. This precedence rule aims to (1) alway work (2) preserve
#' behaviors of lidR version 2.0.y.
#'
#' @section Nested parallelism - part 2:
#' We explained rules of precedence. But actually the user can tune more accuratly the engine. Let
#' define the folllowing function:
#' \preformatted{
#' myfun = function(cluster, ...)
#' {
#'   las <- readLAS(cluster)
#'   if (is.empty(las)) return(NULL)
#'   las  <- lasnormalize(las, tin())
#'   tops <- tree_detection(las, lmf(2))
#'   bbox <- extent(cluster)
#'   tops <- crop(tops, bbox)
#'   return(tops)
#' }
#'
#' out <- catalog_apply(ctg, myfun, ws = 5)
#' }
#' This function used two algorithms, one is partially parallelized (\code{tin}) and one is fully
#' parallelized \code{lmf}. The user can manually use both OpenMP and future. By default the engine
#' will give precedence to chunk-based parallelism because it works in all cases but the user can
#' impose somethingelse. In the following 2 workers are attributed to future and 2 workers are
#' attributed to OpenMP.
#' \preformatted{
#' plan(multisession, workers = 2L)
#' set_lidr_threads(2L)
#' catalog_apply(ctg, myfun, ws = 5)
#' }
#' The rule is simple. If the number of the workers needed is greater than the number of
#' available workers then OpenMP is disabled. Let suppose we have a quadcore machine:
#' \preformatted{
#' # 2 chunks 2 threads: OK
#' plan(multisession, workers = 2L)
#' set_lidr_threads(2L)
#'
#' # 4 chunks 1 threads: OK
#' plan(multisession, workers = 4L)
#' set_lidr_threads(1L)
#'
#' # 1 chunks 4 threads: OK
#' plan(sequential)
#' set_lidr_threads(4L)
#'
#' # 3 chunks 2 threads: NOT OK
#' # Needs 6 workers, OpenMP threads are set to 1 i.e. sequential processing
#' plan(multisession, workers = 3L)
#' set_lidr_threads(2L)
#' }
#'
#' @name lidR-parallelism
NULL