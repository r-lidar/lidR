stopifnotlas = function(x)
{
  if (!inherits(x, "LAS"))
    stop("Argument is not a LAS object", call. = F)
}

stopif_forbidden_name = function(name)
{
  if (name %in% LASFIELDS)
    stop(paste0(name, " is a forbidden name."), call. = FALSE)
}