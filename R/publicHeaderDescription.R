publicHeaderDescription <- function()
{
  hd <- structure(
    list(Item   = c("File Signature",
                    "File Source ID",
                    "Global Encoding",
                    "Project ID - GUID data 1",
                    "Project ID - GUID data 2",
                    "Project ID - GUID data 3",
                    "Project ID - GUID data 4",
                    "Version Major",
                    "Version Minor",
                    "System Identifier",
                    "Generating Software",
                    "File Creation Day of Year",
                    "File Creation Year",
                    "Header Size",
                    "Offset to point data",
                    "Number of variable length records",
                    "Point Data Format ID (0-99 for spec)",
                    "Point Data Record Length",
                    "Number of point records",
                    "Number of points by return",
                    "X scale factor",
                    "Y scale factor",
                    "Z scale factor",
                    "X offset",
                    "Y offset",
                    "Z offset",
                    "Max X",
                    "Min X",
                    "Max Y",
                    "Min Y",
                    "Max Z",
                    "Min Z"),
         Format = c("char[4]", "unsigned short", "unsigned short",
                    "unsigned long", "unsigned short", "unsigned short",
                    "unsigned char[8]", "unsigned char", "unsigned char",
                    "char[32]", "char[32]", "unsigned short", "unsigned short",
                    "unsigned short", "unsigned long", "unsigned long",
                    "unsigned char", "unsigned short", "unsigned long",
                    "unsigned long[5]", "double", "double", "double", "double",
                    "double", "double", "double", "double", "double", "double",
                    "double", "double"),
         Size   = c("4 bytes", "2 bytes", "2 bytes",
                    "4 bytes", "2 byte", "2 byte", "8 bytes", "1 byte", "1 byte",
                    "32 bytes", "32 bytes", "2 bytes", "2 bytes", "2 bytes", "4 bytes",
                    "4 bytes", "1 byte", "2 bytes", "4 bytes", "20 bytes", "8 bytes",
                    "8 bytes", "8 bytes", "8 bytes", "8 bytes", "8 bytes", "8 bytes",
                    "8 bytes", "8 bytes", "8 bytes", "8 bytes", "8 bytes"),
         Required = c("*", "*", "*", "*", "*", "*", "*", "*", "*", "*", "*", "*", "*",
             "*", "*", "*", "*", "*", "*", "*", "*", "*", "*", "*", "*", "*", "*",
             "*", "*", "*", "*", "*")),
        .Names = c("Item", "Format", "Size", "Required"), row.names = 2:33, class = "data.frame")

  hd$what <- ""
  hd$what[grep("unsigned", hd$Format)] <- "integer"
  hd$what[grep("char", hd$Format)] <- "raw"
  hd$what[grep("short", hd$Format)] <- "integer"
  hd$what[grep("long", hd$Format)] <- "integer"
  hd$what[grep("double", hd$Format)] <- "numeric"

  hd$signed <- TRUE
  hd$signed[grep("unsigned", hd$Format)] <- FALSE

  hd$n <- as.numeric(gsub("[[:alpha:][:punct:]]", "", hd$Format))
  hd$n[hd$what == "character"] <- 1
  hd$n[is.na(hd$n)] <- 1

  hd$Hsize <- as.numeric(gsub("[[:alpha:]]", "", hd$Size))

  hd$Rsize <- hd$Hsize / hd$n
  hd$Rsize[hd$what == "raw"] <- 1

  hd$n[hd$what == "raw"] <- hd$Hsize[hd$what == "raw"]

  return(hd)
}
