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
  if (class(modelobject) != "lm")
    stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1], f[2], f[3], lower.tail = F)
  attributes(p) <- NULL
  return(p)
}


.trak <-
  setClass(
    Class = "trak",
    slots = c(
      area = "data.frame",
      centroid = "data.frame",
      direction = "data.frame",
      dropped = "data.frame",
      majoraxislength = "data.frame",
      minoraxislength = "data.frame",
      orientation = "data.frame",
      radius = "data.frame",
      speed = "data.frame",
      speed.regressed = "data.frame",
      theta = "data.frame",
      time = 'numeric',
      weightedcentroid = "data.frame",
      activity = "list",
      metadata = "data.frame",
      hz = "numeric"
    ),
    package = 'RATrak'
  )

# Note that this currently only takes speed, centroid, and time. Of course it can be easily modified to accept more.
# Also the function is very cumbersome -- a restructure is probably worthwhile
readInfo <-
  function(speedBinFileName = NULL,
           centroidBinFileName = NULL,
           timeBinFileName = NULL,
           metadataFileName,
           wellCount,
           start = 1,
           end = wellCount,
           hz = 5,
           inferPhenos = F,
           size.centroid = NA_integer_) {
    #WARNING: The precision for the centroid data is not consistent across autotracker versions. If the centroid coordinates make no sense, try changing size.centroid
    #This is passed on as the size argument to readBin()
    time <- numeric()
    centroid <- data.frame()
    speed <- data.frame()
    
    if (!is.null(centroidBinFileName) &
        !is.null(timeBinFileName) & !is.null(speedBinFileName)) {
      speed <- readBinary(speedBinFileName, wellCount, dataType = 'speed')
      centroid <-
        readBinary(centroidBinFileName,
                   wellCount,
                   dataType = 'centroid',
                   size.centroid = size.centroid)
      time <- readBinary(timeBinFileName, dataType = 'time')
    }
    else if (is.null(centroidBinFileName) &
             !is.null(timeBinFileName) &
             !is.null(speedBinFileName)) {
      speed <- readBinary(speedBinFileName, wellCount, dataType = 'speed')
      time <- readBinary(timeBinFileName, dataType = 'time')
    }
    else if (!is.null(centroidBinFileName) &
             is.null(timeBinFileName) &
             !is.null(speedBinFileName)) {
      speed <- readBinary(speedBinFileName, wellCount, dataType = 'speed')
      centroid <-
        readBinary(centroidBinFileName,
                   wellCount,
                   dataType = 'centroid',
                   size.centroid = size.centroid)
    }
    else if (!is.null(centroidBinFileName) &
             !is.null(timeBinFileName) &
             is.null(speedBinFileName)) {
      print('Only centroid and time data provided. Calculating speed')
      centroid <-
        readBinary(centroidBinFileName,
                   wellCount,
                   dataType = 'centroid',
                   size.centroid = size.centroid)
      time <- readBinary(timeBinFileName, dataType = 'time')
      speed <- flies.calculateSpeed(as.matrix(centroid), time)
    }
    else if (is.null(centroidBinFileName) &
             is.null(timeBinFileName) &
             !is.null(speedBinFileName)) {
      speed <- readBinary(speedBinFileName, wellCount, dataType = 'speed')
    }
    else if (is.null(centroidBinFileName) &
             !is.null(timeBinFileName) &
             is.null(speedBinFileName)) {
      warning('Only time data provided')
      time <- readBinary(timeBinFileName, dataType = 'time')
    }
    else if (!is.null(centroidBinFileName) &
             is.null(timeBinFileName) & is.null(speedBinFileName)) {
      print('Only centroid data provided. Calculating speed')
      centroid <-
        readBinary(centroidBinFileName,
                   wellCount,
                   dataType = 'centroid',
                   size.centroid = size.centroid)
      speed <-
        flies.calculateSpeed(as.matrix(centroid)) * hz #Rescale speed to pixel/s
      
    }
    else
      stop('Neither speed, centroid, or time data was provided')
    
    metadata <- readMetadata(metadataFileName, start, end)
    data <-
      .trak(
        speed = speed,
        centroid = centroid,
        metadata = metadata,
        time = time,
        hz = hz
      )
    if (inferPhenos)
      data <- flies.activity(data)
    return(data)
  }



checkIfFile = function(name, base) {
  fileLoc = paste0(base, name)
  if (file_test("-f", paste0(base, name))) {
    return(fileLoc)
  }
  else{
    return(NULL)
  }
}



# readInfo setup for importing margo folderData
# https://www.biorxiv.org/content/10.1101/593046v1

readInfo.margo <-
  function(rawDataFolder,
           metadataFileName = NULL,
           wellCount,
           start = 1,
           end = wellCount,
           hz = 5,
           startFrame = 4,
           inferPhenos = F,
           size.centroid = NA_integer_,
           featuresToIgnore=c("weightedcentroid","majoraxislength","minoraxislength", "direction",
                                 "orientation","radius","theta","dropped","area")) {
    #WARNING: The precision for the centroid data is not consistent across autotracker versions
    #If the centroid coordinates make no sense, try changing size.centroid
    #This is passed on as the size argument to readBin()
    
    files = list.files(rawDataFolder)
    
    #Method for finding all files and assigning according to name
    # Note that we should refine the regex expression
    for (name in files) {
      assignmentName = tolower(strsplit(name, "[_.]")[[1]][3])
      if (assignmentName %in% featuresToIgnore) {
        assign(assignmentName, data.frame())
      } else{
        assign(
          assignmentName,
          readBinary.margo(
            paste0(rawDataFolder, name),
            dataType = assignmentName,
            colCount = wellCount
          )
        )
      }
    }
    
    if(!is.null(metadataFileName))
      metadata <- readMetadata(metadataFileName, start, end)
    else{
      metadata <- data.frame()
      warning('No metadata provided')
    }
    
    data <-
      .trak(
        area = area,
        centroid = centroid,
        direction = direction,
        dropped = dropped,
        majoraxislength = majoraxislength,
        minoraxislength = minoraxislength,
        orientation = orientation,
        radius = radius,
        speed = speed,
        speed.regressed = data.frame(),
        theta = theta,
        time = time,
        weightedcentroid = weightedcentroid,
        metadata = metadata,
        hz = hz
      )
    if (inferPhenos)
      data <- flies.activity(data)
    return(data)
  }


readBinary <-
  function(fileName,
           colCount,
           dataType,
           size.centroid = 4,
           size.speed_time = 4,
           startFrame = 2) {
    file <- file(fileName, "rb")
    if (dataType == 'speed') {
      mat <-
        matrix(
          readBin(file, numeric(), n = 1e9, size = size.speed_time),
          ncol = colCount,
          byrow = TRUE
        )
      mat <-
        mat[startFrame:nrow(mat),] #Discard first few frames if needed
      close(file)
      mat[is.nan(mat)] = 0
      return(as.data.frame(mat))
    }
    else if (dataType == 'centroid') {
      mat.tmp <-
        matrix(
          readBin(file, numeric(), n = 1e9, size = size.centroid),
          ncol = colCount * 2,
          byrow = TRUE
        )
      #Reshape matrix
      mat <- matrix(ncol = ncol(mat.tmp), nrow = nrow(mat.tmp))
      xCols <- seq(from = 1,
                   to = ncol(mat.tmp) - 1,
                   by = 2)
      yCols <- seq(from = 2,
                   to = ncol(mat.tmp),
                   by = 2)
      mat[, xCols] <- mat.tmp[, 1:colCount]
      mat[, yCols] <- mat.tmp[, (colCount + 1):(colCount * 2)]
      mat <-
        mat[startFrame:nrow(mat),] #Shift to correct for margo output
      close(file)
      mat[is.nan(mat)] = 0
      return(as.data.frame(mat))
    }
    else if (dataType == 'time') {
      time <- readBin(file, numeric(), n = 1e9, size = size.speed_time)
      time <-
        time[startFrame:length(time)]
      close(file)
      return(time)
    }
    else
      stop(paste('datatype:',
                 dataType,
                 'was not recognized.'))
  }


#WARNING: The precision for the centroid data has been changed between single and double in different autotracker versions.

readBinary.margo <-
  function(fileName,
           colCount,
           dataType = NULL,
           size.centroid = 4,
           size.default = 4,
           startFrame = 1) {
    file <- file(fileName, "rb")
    
    if (dataType == 'centroid' || dataType == "weightedcentroid") {
      mat <-
        matrix(
          readBin(file, numeric(), n = 1e9, size = size.centroid),
          ncol = colCount * 2,
          byrow = TRUE
        )
      close(file)
      
      #Note memory issue here because of mat and mat.tmp being available together
      #Reshape matrix
      xCols <- seq(from = 1,
                   to = ncol(mat) - 1,
                   by = 2)
      yCols <- seq(from = 2,
                   to = ncol(mat),
                   by = 2)
      mat = mat[,order(c(xCols,yCols))]
      # mat[, xCols] <- mat.tmp[, 1:colCount]
      # mat[, yCols] <- mat.tmp[, (colCount + 1):(colCount * 2)]
      mat =mat[startFrame:nrow(mat),] #Shift to correct for margo output

      mat[is.nan(mat)] = 0
      sort( sapply(ls(),function(x){object.size(get(x))}))

      return(as.data.frame(mat))
    }
    else if (dataType == "dropped") {
      #Be careful of syncing here -- the rounding may cause issues
      # Needs to be fixed
      bits = (rawToBits(readBin(
        file, raw(), n = 1e9, size = 1
      )))
      close(file)
      mat <-
        matrix(bits[1:(DescTools::RoundTo(length(bits), colCount, trunc))], #Round to multiple of well count
               ncol = colCount,
               byrow = TRUE)
      #    mat <- sapply(as.data.frame(mat), as.logical)
      mat = mat[(startFrame):nrow(mat),] #Discard first few frames if needed
      return(as.data.frame(mat))
    }
    else if (dataType == "time") {
      time <- readBin(file, numeric(), n = 1e9, size = size.default)
      time <- time[startFrame:length(time)]
      close(file)
      return(time)
    }
    else{
      mat <-
        matrix(
          readBin(file, numeric(), n = 1e9, size = size.default),
          ncol = colCount,
          byrow = TRUE
        )#Discard first few frames if needed
      close(file)
      mat[is.nan(mat)] = 0
      return(as.data.frame(mat))
    }
  }


readMetadata <- function(fileName, start = 1, end) {
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
  colnames(meta) = tolower(colnames(meta))
  
  data = meta[start:end, ]
  return(data)
}


#This script provides a quick way to group load packages - it's not needed in the package but could be useful for end users in other applications
loadPackages <- function(names) {
  missingPackages <-
    names[!(names %in% installed.packages()[, "Package"])]
  if (length(missingPackages)) {
    install.packages(missingPackages)
  }
  for (pkg in names) {
    library(pkg, character.only = TRUE)
  }
}


#
classApply <- function(x, FUN, ...) {
  cl <- class(x)
  result <- list()
  for (i in propertyNames(cl)) {
    result[[i]] <- FUN(slot(x, i), ...)
  }
  result
}

groupMean <- function(x, l) {
  rowMeans(as.matrix(x[, l == T]))
}

paste_ <- function(x){
  tmp = ""
  for (w in x) {
    if(tmp != "")
      tmp = paste(tmp,w,sep="_")
    else
      tmp= w
  }
  return(tmp)
}
