# Test if a function return is valid for the gridMetrics function
#
#
# When a user creates their own function and applies it to the gridMetrics function it can sometimes return
# a conceptual error. This function tests if the return is valid before aggregating the
# data.
.testFuncSignature = function(metrics, func)
{
  funcstring = deparse(func)

  if(is.list(metrics) & !is.data.frame(metrics))
  {
    classes = sapply(metrics, class)
    test = classes %in% c("integer", "numeric", "logical", "character")
    n = names(metrics[!test])
    c = classes[!test]

    if(sum(!test) == 1)
      lidRError("TFS1", expression = funcstring, metric = n, class = c)
    else if(sum(!test) > 1)
      lidRError("TFS2", expression = funcstring, metric = n, class = c)

    size = sapply(metrics, length)
    test = size == 1

    n = names(metrics[!test])
    c = size[!test]

    if(sum(!test) == 1)
      lidRError("TFS3", expression = funcstring, metric = n, number = c)
    else if(sum(!test) > 1)
      lidRError("TFS4", expression = funcstring, metric = n, number = c)
  }
  else if(is.data.frame(metrics))
    lidRError("TFS5", expression = funcstring)
  else if(is.vector(metrics) & length(metrics) > 1)
    lidRError("TFS6", expression = funcstring)
  else
    return(0)
}