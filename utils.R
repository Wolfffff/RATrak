#Utility functions
#readBinary
#readMetadata
#loadPackages
#
#
#

readBinary <- function(fileName, colCount, start = 1, end = colCount){
  file <- file(fileName, "rb")
  mat <- matrix(readBin(file, numeric(), n= 9999999, size=4),ncol = colCount,byrow = TRUE)
  fly <- colCount
  return(as.data.frame(mat[,start:end]))
}

readMetadata <- function(fileName){
  meta <- read.csv("/Users/Wolf/RareTrombone/Resources/info.csv",header = TRUE)
  meta$Treatment <- as.vector(meta$Treatment)
  return(meta)
}



#This script provides a quick way to group load packages - it's not needed in the package but could be useful for end users in other applications
loadPackages <- function(names){
  missingPackages <- names[!(names %in% installed.packages()[,"Package"])]
  if(length(missingPackages)){
    install.packages(missingPackages)
  }
  for (pkg in names) {
    library(pkg,character.only = TRUE)
  }
}
