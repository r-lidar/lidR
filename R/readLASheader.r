#' @importFrom tools file_ext
.loadLASheaders = function(files)
{
  valid = file.exists(files)
  islas = tools::file_ext(files) == "las"

  if( sum(!valid) != 0 )
    lidRError("LAS1", files = files[!valid])

  if( sum(!islas) != 0 )
    lidRError("LAS2", files = files[!islas])

  data = readLASheader(files[1])

  for(file in files[-1])
    data = list(data, readLASheader(file))

  return(data)
}

#' Read a .las file header
#'
#' Methods to read .las file header. This function should not be used by users. Use \link[lidR:LoadLidar]{LoadLidar}.
#'
#' @param LASfile character. Filename of .las file
#' @return A list with the las header fields
#' @seealso \link[lidR:LoadLidar]{LoadLidar}
#' @export readLASheader
#' @importFrom magrittr %<>%
readLASheader = function(LASfile)
{
  hd <- publicHeaderDescription()

  pheader <- vector("list", nrow(hd))
  names(pheader) <- hd$Item

  con <- file(LASfile, open = "rb")
  isLASFbytes <- readBin(con, "raw", size = 1, n = 4, endian = "little")
  pheader[[hd$Item[1]]] <- readBin(isLASFbytes, "character", size = 4, endian = "little")

  if (! pheader[[hd$Item[1]]] == "LASF")
    lidRError("LAS3")

  for (i in 2:nrow(hd))
    pheader[[hd$Item[i]]] <- readBin(con, what = hd$what[i], size = hd$Rsize[i], endian = "little", n = hd$n[i])

  close(con)

  pheader$`Generating Software` %<>% rawToChar
  pheader$`System Identifier` %<>% rawToChar

  pheader$`Version Major` %<>% as.numeric
  pheader$`Version Minor` %<>% as.numeric

  pheader$`Project ID - GUID data 1` = NULL
  pheader$`Project ID - GUID data 2` = NULL
  pheader$`Project ID - GUID data 3` = NULL
  pheader$`Project ID - GUID data 4` = NULL
  pheader$`Number of points by return` = NULL
  pheader$`Point Data Format ID (0-99 for spec)` = NULL

  return(pheader)
}

