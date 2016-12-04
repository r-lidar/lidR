# Add new variables by reference
#
# mutatebyref adds new columns in a data.table or a LAS object and preserves existing columns as
# dplyr::mutate does, but it does this by reference i.e. without any copy of the memory
#
# @param .las a data.table or a LAS object
# @param \dots Name-value pairs of expressions. Use NULL to drop a variable.
# @return Nothing. Modification made in place
mutatebyref <- function (.data, ...)
{
   UseMethod("mutatebyref", .data)
}

mutatebyref.LAS <- function(.data, ...)
{
  mutatebyref_(.data@data, lazyeval::dots_capture(...))
}

mutatebyref.data.table <- function(.data, ...)
{
  mutatebyref_(.data, lazyeval::dots_capture(...))
}


mutatebyref_ <- function(`_dt`, args)
{
  args <- lazyeval::as_f_list(args)

  for (nm in names(args))
  {
    `_dt`[, (nm) := lazyeval::f_eval(args[[nm]], `_dt`)]
  }

  `_dt`[]

  return(invisible())
}
