context("writeLAS")

LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
i = readLAS(LASfile)
ofile = paste0(tempfile(), ".las")

test_that("Test if I/O are equal", {
  writeLAS(i, ofile)
  o = readLAS(ofile)

  # Because those field are expepected to be different
  i@header@PHB["Generating Software"] <- NULL
  o@header@PHB["Generating Software"] <- NULL

  expect_equal(i@data, o@data)
  expect_equal(i@header, o@header)
})

ebfile <- system.file("extdata", "extra_byte.laz", package="rlas")
las <- readLAS(ebfile, select="*+")

check_EB_header <- function(new, origin){
  if(is.list(new))
    mapply(check_EB_header, new[names(origin)], origin)
  else
    new[1] == origin[1]
}

test_that("Add extra bytes with default description",{
  writeLAS(las, ofile)
  lasw=readLAS(ofile, select = "*+")
  expect_true(all(lasw@data==las@data))
  modified_fileds=c("System Identifier", "Generating Software", "Offset to point data")
  all(unlist(check_EB_header(lasw@header@PHB[!(names(lasw@header@PHB) %in% modified_fileds)],
                             las@header@PHB[!(names(lasw@header@PHB) %in% modified_fileds)])))
})

test_that("Add description to existing extra byte",{
  las@header=extra_byte_desc(las@header, name="pulseID",
                  data_type=1,
                  min=min(las@data$pulseID),
                  max=max(las@data$pulseID),
                  description="Pulse identifier [a.u.]")

  las@header=extra_byte_desc(las@header, name="flightlineID",
                  data_type=1,
                  min=min(las@data$flightlineID),
                  max=max(las@data$flightlineID),
                  description="Flightline identifier [a.u.]")

  writeLAS(las, ofile)
  lasw=readLAS(ofile, select = "*+")

  expect_true(all(lasw@data[,names(las@data), with=F]==las@data))
  EB_header_check = check_EB_header(lasw@header@VLR$Extra_Bytes$`Extra Bytes Description`,
                  las@header@VLR$Extra_Bytes$`Extra Bytes Description`)
})

test_that("Add extra bytes with description",{
  las2w=add.extra_byte(las, data = data.table::data.table(ID=c(1:nrow(las@data)), reverseID=c(nrow(las@data):1)),
                       names=c("ID", "reverseID"),
                       data_type=c(1,9), min=c(1,1),
                       max=c(nrow(las@data), nrow(las@data)),
                       description=c("identifier", "Reverse identifier"))

  writeLAS(las2w, ofile)
  lasw=readLAS(ofile)
  expect(lasw@header@VLR$Extra_Bytes$`Extra Bytes Description`$reverseID$data_type==9)
  expect(las2w@header@VLR$Extra_Bytes$`Extra Bytes Description`$ID$data_type==1)

})