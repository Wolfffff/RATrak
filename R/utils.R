#Utility functions
#lmp
#readBinary
#readMetadata
#loadPackages
#
#
#
#


lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

.trak <- setClass("trak", slots = c(speed="data.frame", activity="list",metadata = "data.frame", hz = "numeric"))

readInfo <- function(speedBinFileName, metadataFileName, wellCount, start = 1, end = wellCount, hz = 5, inferPhenos = T){
  speed <- readBinary(speedBinFileName, wellCount, start, end)
  metadata <- readMetadata(metadataFileName, start, end)
  data <- .trak(speed=speed,metadata=metadata,hz=hz)
  if(inferPhenos)
    data <- flies.sleepActivity(data)
  return(data)
}

readBinary <- function(fileName, colCount, start = 1, end = colCount){
  file <- file(fileName, "rb")
  mat <- matrix(readBin(file, numeric(), n= 1e8, size=4),ncol = colCount,byrow = TRUE)
  close(file)
  return(as.data.frame(mat[,start:end]))
}

readMetadata <- function(fileName, start = 1, end){
  #Determine field separator
  L <- readLines(fileName, n = 1)
  if (grepl(";", L))
    meta <- read.table(fileName, header = TRUE, sep = ';')
  else if (grepl(",", L))
    meta <- read.table(fileName, header = TRUE, sep = ',')
  else if (grepl("\t", L))
    meta <- read.table(fileName, header = TRUE, sep = '\t')
  else
    stop(paste('Could not determine field separator in', fileName))
  
  meta$Treatment <- as.vector(meta$Treatment)
  data = meta[start:end,]
  return(data)
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
