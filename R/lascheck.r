lascheck = function(data)
{
  fields = names(data)

  if(sum(c("X", "Y", "Z") %in%fields) != 3)
    stop("No fields named X Y and Z found.", call. = F)

  negvalues = fast_countbelow(data$Z, 0)

  if(negvalues > 0)
    lidRError("LDR2", number = negvalues, behaviour = warning)

  if("Classification" %in% fields)
  {
    class0 = fast_countequal(data$Classification, 0)

    if(class0 > 0)
      lidRError("LDR3", number = class0, behaviour = warning)
  }

  if("ReturnNumber" %in% fields)
  {
    class0 = fast_countequal(data$ReturnNumber, 0)

    if(class0 > 0)
      lidRError("LDR10", number = class0, behaviour = warning)
  }
}