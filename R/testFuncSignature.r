# Test if a function return is valid for the gridMetrics function
#
#
# When a user creates their own function and applies it to the gridMetrics function it can sometimes return
# a conceptual error. This function tests if the return is valid before aggregating the
# data.
.testFuncSignature = function(metrics, func)
{
  funcstring = deparse(func)

  begin_err = paste("The expression '", funcstring, "' returned a", sep = "")
  advice = "A single number or a list of single number is expected."

  if(is.list(metrics) & !is.data.frame(metrics))
  {
    classes = sapply(metrics, class)
    test = classes %in% c("integer", "numeric", "logical", "character")
    n = names(metrics[!test])
    c = classes[!test]

    if(sum(!test) == 1)
    {
      stop(paste(begin_err,
                 "list in which all elements are not a single numeric or logical value. The metric",
                 n, "is a", c,
                 sep=" "),
           call.=F)
    }
    else if(sum(!test) > 1)
    {
      stop(paste(begin_err,
                 "list in which all elements are not a single numeric or logical value. The metrics",
                 capture.output(cat(n, sep=" and ")),
                 "are respectively",
                 capture.output(cat(c, sep= " and ")),
                 sep=" "),
           call.=F)
    }

    size = sapply(metrics, length)
    test = size == 1

    n = names(metrics[!test])
    c = size[!test]

    if(sum(!test) == 1)
    {
      stop(paste(begin_err,
                 "list in which all elements are not a single value. The metric",
                 n, "has a length of", c,
                 sep=" "),
           call.=F)
    }
    else if(sum(!test) > 1)
    {
      stop(paste(begin_err,
                 "list in which all elements are not a single value. The metrics:",
                 capture.output(cat(n, sep=" and ")),
                 "have respectively a length of:",
                 capture.output(cat(c, sep=" and ")),
                 sep=" "),
           call.=F)
    }
  }
  else if(is.data.frame(metrics))
    stop(paste(begin_err,  "a data.frame", advice, sep=" "), call.=F)
  else if(is.vector(metrics) & length(metrics) > 1)
    stop(paste(begin_err, "a vector of lenght ", length(metrics), advice, sep=" "), call.=F)
  else
    return(0)
}