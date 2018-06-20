#Utility functions
#readBinary
#readMetadata
#loadPackages
#
#
#

readBinary <- function(fileName, colCount, start = 1, end){
  #readBinary
  
  file <- file(fileName, "rb")
  #Format to matrix
  mat <- matrix(readBin(file, numeric(), n= 9999999, size=4),ncol = colCount,byrow = TRUE)
  #Return as data frame
  if(is.null(end)){
    fly <- colCount
  }
  return(as.data.frame(mat[,start:end]))
}

readMetadata <- function(fileName){
  meta <- read.csv("/Users/Wolf/RareTrombone/Resources/info.csv", sep = ',',header = TRUE)
  meta$Treatment <- as.vector(meta$Treatment)
  return(meta)
}



loadPackages <- function(names){
  missingPackages <- names[!(names %in% installed.packages()[,"Package"])]
  if(length(missingPackages)){
    install.packages(missingPackages)
  }
  for (pkg in names) {
    library(pkg,character.only = TRUE)
  }
}
