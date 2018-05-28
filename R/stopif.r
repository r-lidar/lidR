stopifnotlas = function(x)
{
  if (!inherits(x, "LAS"))
    stop("Argument is not a LAS object", call. = F)
}

stopif_forbidden_name = function(name)
{
  if (name %in% LASFIELDS)
    stop(glue("{name} is a forbidden name."), call. = FALSE)
}