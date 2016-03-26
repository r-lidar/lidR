# Test if a function return is valid for the gridMetrics function
#
#
# When user create its own function and apply it into gridMetrics function it can make
# a conceptual error. This function test is the return is valid before to aggregate the
# data.
.testFuncSignature = function(metrics, func)
{
  funcstring = deparse(func)

  if(is.list(metrics) & !is.data.frame(metrics))
  {
    classes = sapply(metrics, class)
    test = classes %in% c("integer", "numeric", "logical")
    n = names(metrics[!test])
    c = classes[!test]

    if(sum(!test) > 0)
      stop(paste("The function '", funcstring, "' returned a list in which all elements are not a single numeric or logical value. The elements: '", capture.output(cat(n)), "' are respectively of classes: '", capture.output(cat(c)), "'", sep=""), call.=F)

    size = sapply(metrics, length)
    test = size == 1

    n = names(metrics[!test])
    c = size[!test]

    if(sum(!test) > 0)
      stop(paste("The function '", funcstring, "' returned a list in which all elements are not a single value. The elements: '", capture.output(cat(n)), "' have respectively a length of: '", capture.output(cat(c)), "'", sep=""), call.=F)
  }
  else if(is.data.frame(metrics))
  {
    stop(paste("The function '", funcstring, "' returned a data.frame. A single number or a list of single number is expected.", sep=""), call.=F)
  }
  else if(is.numeric(metrics))
  {
    if(is.vector(metrics) & length(metrics) > 1)
      stop(paste("The function '", funcstring, "' returned a vector. A single number or a list of single number is expected.", sep=""), call.=F)

  }
  else
    return(0)
}