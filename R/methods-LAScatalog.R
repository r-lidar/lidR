
#' @export
#' @rdname Extract
setMethod("$", "LAScatalog", function(x, name) { x@data[[name]] })

#' @export
#' @rdname Extract
setMethod("[[", c("LAScatalog", "ANY", "missing"), function(x, i, j, ...) { x@data[[i]] })


#' @param ... Unused
#' @param drop Unused
#' @export
#' @rdname Extract
setMethod("[", "LAScatalog", function(x, i, j, ..., drop = FALSE) {

  ctgname <- deparse(substitute(x))
  iname   <- deparse(substitute(i))
  nargs   <- nargs()

  if (!missing(i) & !missing(j))
    stop(glue::glue("This action is not allowed for a {class(x)}. j must be missing. Maybe you meant: {ctgname}[{iname}, ]."), call. = FALSE)

  if (missing(i) & !missing(j))
    stop(glue::glue("This action is not allowed for a {class(x)}. i cannot be missing."), call. = FALSE)

  if (!missing(i) & missing(j) & nargs == 2L)
    return(x@data[i])

  x@data <- x@data[i, ]
  return(x)
})

#' @export
#' @rdname Extract
setMethod("[", c("LAScatalog", "logical"),  function(x, i)
{
  x@data <- x@data[i,]
  return(x)
})

#' @export
#' @rdname Extract
setMethod("[", c("LAScatalog", "sf"),  function(x, i)
{
  return(x[sf::st_geometry(i)])
})

#' @export
#' @rdname Extract
setMethod("[", c("LAScatalog", "sfc"),  function(x, i)
{
  return(catalog_intersect(x, i))
})

#' @export
#' @rdname Extract
setMethod("[[<-", "LAScatalog",  function(x, i, j, value)
{
  if (i %in% LASCATALOGATTRIBUTES)
    stop("LAScatalog data read from standard files cannot be modified", call. = FALSE)

  x@data[[i]] = value
  return(x)
})

#' @export
#' @rdname Extract
setMethod("$<-", "LAScatalog", function(x, name, value)
{
  if (name %in% LASCATALOGATTRIBUTES)
    stop("LAScatalog data read from standard files cannot be modified", call. = FALSE)

  x@data[[name]] = value
  return(x)
})
