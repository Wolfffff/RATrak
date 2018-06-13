readBinary<- function(fileName, colCount){
  #readBinary
  file <- file(fileName, "rb")
  #Format to matrix
  mat <- matrix(readBin(file, numeric(), n= 999999999999, size=4),ncol = colCount,byrow = TRUE)
  return(mat)
}

