#Utility functions
#readBinary
#loadPackages
#
#
#

readBinary<- function(fileName, colCount, start = 1, end){
  #readBinary
  
  file <- file(fileName, "rb")
  #Format to matrix
  mat <- matrix(readBin(file, numeric(), n= 9999999, size=4),ncol = colCount,byrow = TRUE)
  #Return as data frame
  if(is.null(end)){
    fly <- colCount
  }
  return(as.matrix(mat[,start:end]))
}



loadPackages <- function(names){
  missingPackages <- names[!(names %in% installed.packages()[,"Package"])]
  if(length(missingPackages)){
    install.packages(missingPackages)
  }
}
