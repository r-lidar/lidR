lascheck = function(data, header, hard = F)
{
  fields = names(data)

  h = header

  if(sum(c("X", "Y", "Z") %in%fields) != 3)
    stop("No fields named X Y and Z found.", call. = F)

  if(length(h) > 0 & hard)
  {
    if(dim(data)[1] != h["Number of point records"])
      warning(paste0("Invalid file: header states the file contains ", h["Number of point records"], " points but ", dim(data)[1], " were found. Header has been updated."), call. = F)

    if("ReturnNumber" %in% fields)
    {
      number_of <- fast_table(data$ReturnNumber, 5L)

      if(h["Number of 1st return"] != number_of[1])
        warning(paste0("Invalid file: header states the file contains ", h["Number of 1st return"], " 1st returns but ", number_of[1], " were found. Header has been updated."), call. = F)

      if(h["Number of 2nd return"] != number_of[2])
        warning(paste0("Invalid file: header states the file contains ", h["Number of 2nd return"], " 2nd returns but ", number_of[2], " were found. Header has been updated."), call. = F)

      if(h["Number of 3rd return"] != number_of[3])
        warning(paste0("Invalid file: header states the file contains ", h["Number of 3rd return"], " 3rd returns but ", number_of[3], " were found. Header has been updated."), call. = F)

      if(h["Number of 4th return"] != number_of[4])
        warning(paste0("Invalid file: header states the file contains ", h["Number of 4th return"], " 4th returns but ", number_of[4], " were found. Header has been updated."), call. = F)

      if(h["Number of 5th return"] != number_of[5])
        warning(paste0("Invalid file: header states the file contains ", h["Number of 5th return"], " 5th returns but ", number_of[5], " were found. Header has been updated."), call. = F)
    }
  }

  negvalues = fast_countbelow(data$Z, 0)

  if(negvalues > 0)
    warning(paste0(negvalues, " points below 0 found."), call. = F)

  if("Classification" %in% fields)
  {
    class0 = fast_countequal(data$Classification, 0)

    if(class0 > 0)
      message(paste0(class0, " unclassified points found."), call. = F)
  }

  if("ReturnNumber" %in% fields)
  {
    class0 = fast_countequal(data$ReturnNumber, 0)

    if(class0 > 0)
      warning(paste0("Dataset is invalid: ", class0, " points with a return number of 0 found."), call. = F)
  }
}