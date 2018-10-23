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

.trak <- setClass(Class = "trak", slots = c(speed = "data.frame", centroid = "data.frame", activity = "list", metadata = "data.frame", hz = "numeric"), package = 'RATrak')

readInfo <- function(speedBinFileName = NULL, centroidBinFileName = NULL, metadataFileName, wellCount, start = 1, end = wellCount, hz = 5, inferPhenos = T){
  if(is.null(centroidBinFileName) & !is.null(speedBinFileName)){
    speed <- readBinary(speedBinFileName, wellCount, dataType = 'speed')
    centroid <- data.frame()
  }
  else if(!is.null(centroidBinFileName) & !is.null(speedBinFileName)){  
    speed <- readBinary(speedBinFileName, wellCount, dataType = 'speed')
    centroid <- readBinary(centroidBinFileName, wellCount, dataType = 'centroid') 
  }
  else if(!is.null(centroidBinFileName) & is.null(speedBinFileName)){
    print('No speed data provided. Calculating speed from centroid data')
    centroid <- readBinary(centroidBinFileName, wellCount, dataType = 'centroid') 
    speed <- flies.calculateSpeed(as.matrix(centroid), hz = hz)
  }
  else
    stop('Neither speedBinFileName or centroidBinFileName was provided')
  
  metadata <- readMetadata(metadataFileName, start, end)
  data <- .trak(speed=speed, centroid=centroid, metadata=metadata, hz=hz)
  if(inferPhenos)
    data <- flies.sleepActivity(data)
  return(data)
}

readBinary <- function(fileName, colCount, dataType){
  file <- file(fileName, "rb")
  if(dataType == 'speed')
    mat <- matrix(readBin(file, numeric(), n = 1e8, size = 4), ncol = colCount, byrow = TRUE)
  else if(dataType == 'centroid'){
    mat.tmp <- matrix(readBin(file, numeric(), n = 1e8, size = 8), ncol = colCount*2, byrow = TRUE)
    #Reshape matrix
    mat <- matrix(ncol = ncol(mat.tmp), nrow = nrow(mat.tmp))
    xCols <- seq(from = 1, to = ncol(mat.tmp) - 1, by = 2)
    yCols <- seq(from = 2, to = ncol(mat.tmp), by = 2)
    mat[, xCols] <- mat.tmp[, 1:colCount]
    mat[, yCols] <- mat.tmp[, (colCount+1):(colCount*2)]
  }
  close(file)
  return(as.data.frame(mat))
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
