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
      dropped_frames = "data.frame",
      majoraxislength = "data.frame",
      minoraxislength = "data.frame",
      orientation = "data.frame",
      orientation_continuous = "data.frame",
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
                                 "orientation","radius","theta","area")) {
    #WARNING: The precision for the centroid data is not consistent across autotracker versions
    #If the centroid coordinates make no sense, try changing size.centroid
    #This is passed on as the size argument to readBin()
    
    files = list.files(rawDataFolder)
    features <- c('area', 'centroid', 'direction', 'dropped_frames', 'majoraxislength', 'minoraxislength', 'orientation', 'radius', 'speed', 'theta', 'time', 'weightedcentroid')
    #Load features from the files in rawDataFolder
    for (name in features) {
      if (name %in% featuresToIgnore) { #Ignore this feature
        if(name == 'time')
          assign(name, numeric())
        else
          assign(name, data.frame())
      }
      else if(!any(grepl(pattern = paste0('.*__', name, '.*'), files, ignore.case = T))){ #Feature is not present in rawDataFolder
        if(name == 'time')
          assign(name, numeric())
        else
          assign(name, data.frame())
      }
      else{ #Read feature
        fileName <- grep(pattern = paste0('.*__', name, '.*'), files, value = T, ignore.case = T)
        message('Loading: ', fileName)
        
        assign(
          name,
          readBinary.margo(
            paste0(rawDataFolder, fileName),
            dataType = name,
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
    
    orientation <- orientation * pi / 180
    
    #Continuous phase
    orientation_continuous <-
      as.data.frame(do.call(cbind, mclapply(as.data.frame(orientation, unwrap, mc.cores = 16)))
    data <-
      .trak(
        area = area,
        centroid = centroid,
        direction = direction,
        dropped_frames = dropped_frames,
        majoraxislength = majoraxislength,
        minoraxislength = minoraxislength,
        orientation = orientation,
        orientation_continuous = orientation_continuous,
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
          readBin(file, numeric(), n = 1e10, size = size.speed_time),
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
          readBin(file, numeric(), n = 1e10, size = size.centroid),
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
      time <- readBin(file, numeric(), n = 1e10, size = size.speed_time)
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
          readBin(file, numeric(), n = 1e10, size = size.centroid),
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
      mat <- mat[,order(c(xCols,yCols))] 
      # mat[, xCols] <- mat.tmp[, 1:colCount]
      # mat[, yCols] <- mat.tmp[, (colCount + 1):(colCount * 2)]
      mat <- mat[startFrame:nrow(mat),] #Shift to correct for margo output
      #mat[is.nan(mat)] = 0
      
      
      mat <- apply(mat,2,FUN = function(x){
        base <- x[which(!is.nan(x))[1]]
        for (i in 1:length(x)) {
          if (is.nan(x[i])) {
            if (identical(x[i-1], numeric(0))) {
              x[i] = base
            } else{
              x[i] = x[i-1]
            }
          }
        }
        return(x)
      })

      return(as.data.frame(mat))
    }
    else if (dataType == "dropped_frames") {
      #Be careful of syncing here -- the rounding may cause issues
      # Needs to be fixed
      bits = (rawToBits(readBin(
        file, raw(), n = 1e10, size = 1
      )))
      close(file)
      mat <-
        matrix(bits[1:(DescTools::RoundTo(length(bits), colCount, trunc))], #Round to multiple of well count
               ncol = colCount,
               byrow = TRUE)
      mat = mat==1
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

dtwDistance_parallel_listed <- function(spgeom1) {
  # if second set of lines is not given, calculate pairwise distances within
  # first set of lines
  if (is.null(spgeom2)) {
    
    # prepare empty distance matrix
    n_geoms <- length(spgeom1)
    distmat <- foreach(i=1:(n_geoms - 1),.combine='rbind',.packages = c("sp","dtw","foreach","doParallel")) %dopar% {
      crds1 <- spgeoms[[i]]
      temp <- foreach(j=(i + 1):n_geoms,.packages = c("sp","dtw"),.combine="c") %do% {
        crds2 <- crds1 <- spgeoms[[j]]
        align <- dtw(crds1,crds2)
        align$normalizedDistance  # normalized distance
      }
      temp <- c(rep(0,(n_geoms - length(temp))),temp)
      temp
    }
    distmat <- rbind(distmat,rep(0,n_geoms))
    distmat <- t(as.matrix(distmat))
    # print(dim(distmat))
    # if two sets of lines are given, calculate pairwise distances
  }
  
  ids <- names(spgeom1)
  # print(length(ids))
  
  colnames(distmat) <- ids
  rownames(distmat) <- ids
  return(distmat)
}



generate_video <- function(trak,video_location,fly_number,start,end,width,variables = c("speed","orientation","direction")){
  td <- seconds_to_period(86400)
  time_start = sprintf('%02d:%02d:%02d',td@hour + 24*day(td), minute(td), second(td))
  
  
  y_min <-
    min(trak@centroid[which(trak@centroid[, 2 * fly_number] !=
                              0), 2 * fly_number])
  y_max <-
    max(trak@centroid[which(trak@centroid[, 2 * fly_number] !=
                              0), 2 * fly_number])
  
  x_min <-
    min(trak@centroid[which(trak@centroid[, 2 * fly_number - 1] !=
                              0), 2 * fly_number - 1])
  x_max <-
    max(trak@centroid[which(trak@centroid[, 2 * fly_number - 1] !=
                              0), 2 * fly_number - 1])
  
  start_y = y_min - 10
  start_x = x_min - 10
  vid_width = 120
  name_fly_vid = paste0("temp/Fly_Vid_Frame_%d.png")
  cmd <-
    paste0('ffmpeg -ss ',
           start / trak@hz ,
           " -i ",
           video_location, 
           " -t ",
           (end - start + 1) / trak@hz,
           ' -filter:v "crop=',
           vid_width,
           ':',
           vid_width,
           ':',
           start_x,
           ':',
           start_y,
           '\" ',
           name_fly_vid,
           ''
    )
  #print(cmd)
  system(cmd)
  
  
  list_of_plots <- list()
  for (i in start:end) {
    count <- i - start + 1
    current_name <- paste0("temp/Fly_Vid_Frame_", count,".png")
    temp <- list()
    j = 0
    if ("speed" %in% variables) {
      j <- j + 1
      temp[[j]] <- ggplot(NULL,aes(x=(i-width):(i+width),y=trak@speed[(i-width):(i+width),fly_number])) +
        geom_path() +
        theme_minimal() +
        ylab("Speed") +
        xlab("") +
        geom_vline(xintercept = i,color="red",alpha=0.5) +
        #     ggtitle(paste0("Speed from ", start, " to ", end)) +
        ylim(c(0,100))  +
        scale_x_continuous(breaks = round(seq(min(i-width), (i+width), by = 10),1),limits=c(i-width,i+width)) +
        theme(axis.text.x=element_blank())
    }
    if ("majoraxislength" %in% variables) {
      j <- j + 1
      temp[[j]] <- ggplot(NULL,aes(x=(i-width):(i+width),y=trak@majoraxislength[(i-width):(i+width),fly_number])) +
        geom_path() +
        theme_minimal() +
        ylab("Major Axis") +
        xlab("") +
        geom_vline(xintercept = i,color="red",alpha=0.5) +
        #  ggtitle(paste0("Major Axis Length from ", start, " to ", end)) +
        ylim(c(0,50)) +
        scale_x_continuous(breaks = round(seq(min(i-width), (i+width), by = 10),1),limits=c(i-width,i+width)) +
        theme(axis.text.x=element_blank())
    }
    if ("minoraxislength" %in% variables) {
      j <- j + 1
      temp[[j]] <- ggplot(NULL,aes(x=(i-width):(i+width),y=trak@minoraxislength[(i-width):(i+width),fly_number])) +
        geom_path() +
        theme_minimal() +
        ylab("Minor Axis") +
        xlab("") +
        geom_vline(xintercept = i,color="red",alpha=0.5) +
        #    ggtitle(paste0("Minor Axis Length from ", start, " to ", end)) +
        ylim(c(0,20)) +
        scale_x_continuous(breaks = round(seq(min(i-width), (i+width), by = 10),1),limits=c(i-width,i+width)) +
        theme(axis.text.x=element_blank())
    }
    if ("orientation" %in% variables) {
      j <- j + 1
      temp[[j]] <- ggplot(NULL,aes(x=(i-width):(i+width),y=trak@orientation[(i-width):(i+width),fly_number])) +
        geom_path() +
        theme_minimal() +
        ylab("Orientation") +
        xlab("") +
        geom_vline(xintercept = i,color="red",alpha=0.5) +
        #ggtitle(paste0("Orientation from ", start, " to ", end)) +
        ylim(c(-pi/2,pi/2)) +
        scale_x_continuous(breaks = round(seq(min(i-width), (i+width), by = 10),1),limits=c(i-width,i+width)) +
        theme(axis.text.x=element_blank())
    }
    if ("orientation_continuous" %in% variables) {
      j <- j + 1
      temp[[j]] <- ggplot(NULL,aes(x=(i-width):(i+width),y=trak@orientation_continuous[(i-width):(i+width),fly_number])) +
        geom_path() +
        theme_minimal() +
        ylab("Cont. Orientation") +
        xlab("") +
        geom_vline(xintercept = i,color="red",alpha=0.5) +
        #ggtitle(paste0("Orientation from ", start, " to ", end)) +
        ylim(c(min(trak@orientation_continuous[(start-width):end,fly_number]),max(trak@orientation_continuous[start:end,fly_number]))) +
        scale_x_continuous(breaks = round(seq(min(i-width), (i+width), by = 10),1),limits=c(i-width,i+width)) +
        theme(axis.text.x=element_blank())
    }
    
    if ("area" %in% variables) {
      j <- j + 1
      temp[[j]] <- ggplot(NULL,aes(x=(i-width):(i+width),y=trak@area[(i-width):(i+width),fly_number])) +
        geom_path() +
        theme_minimal() +
        ylab("Area") +
        xlab("") +
        geom_vline(xintercept = i,color="red",alpha=0.5) +
        #ggtitle(paste0("Orientation from ", start, " to ", end)) +
        ylim(c(min(trak@area[(start-width):(end+width),fly_number]),max(trak@area[(start-width):(end+width),fly_number]))) +
        scale_x_continuous(breaks = round(seq(min(i-width), (i+width), by = 10),1),limits=c(i-width,i+width)) +
        theme(axis.text.x=element_blank())
    }
    
    if ("direction" %in% variables) {
      j <- j + 1
      temp[[j]] <- ggplot(NULL,aes(x=(i-width):(i+width),y=trak@direction[(i-width):(i+width),fly_number])) +
        geom_path() +
        theme_minimal() +
        ylab("Direction") +
        xlab(paste0("Frame Number")) +
        geom_vline(xintercept = i,color="red",alpha=0.5) +
        # ggtitle(paste0("Direction from ", start, " to ", end)) +
        ylim(c(-pi,pi )) +
        scale_x_continuous(breaks = round(seq(min(i-width), (i+width), by = 10),1),limits=c(i-width,i+width))
    }
    
    centroid_x <- trak@centroid[i,2*fly_number-1]-start_x
    centroid_y <- trak@centroid[i,2*fly_number]-start_y
    
    vec_start_x <- trak@centroid[i-1,2*fly_number-1]-start_x
    vec_start_y <- trak@centroid[i-1,2*fly_number]-start_y
    
    majoraxislength <- trak@majoraxislength[i,fly_number]
    minoraxislength <- trak@minoraxislength[i,fly_number]
    orientation <- f1s_set1@orientation[i,fly_number]
    current_name <- paste0("temp/Fly_Vid_Frame_", count,".png")
    base <- image_ggplot(image_read(current_name)) + 
      geom_point(aes(x=centroid_x,y=centroid_y),color="red") +
      geom_ellipse(aes(x0=centroid_x,y0=centroid_y,a = majoraxislength/2,b=minoraxislength/2,angle=orientation)) + 
      geom_segment(aes(xend=centroid_x,yend=centroid_y,x=vec_start_x,y=vec_start_y),arrow = arrow(length=unit(0.10,"cm"),type="closed"))
    
    temp_plot <- plot_grid(plotlist = temp,align="v",ncol=1)
    
    output <- plot_grid(temp_plot,base,ncol=2,rel_widths = c(1,1),rel_heights = c(1,1))
    
    
    ggsave(paste0("temp/temp_",i,".png"),output,height = 8,width=16,units = "in",dpi=320)
    
  }
  
  name_plots=paste0("temp/Fly",fly_number,"_Start",start,"_End",end,"_Time",format(Sys.time(), "%Y-%m-%d_%H%M%S"),".mp4")
  merge_cmd <- paste0("ffmpeg -framerate ", trak@hz ," -start_number ",start, " -i 'temp/temp_%d.png' -pix_fmt yuv420p " ,name_plots)
  #print(merge_cmd)
  system(merge_cmd,wait = T)
  
  
  
  #name_merged=paste0("temp/",format(Sys.time(), "%Y-%m-%d_%H%M%S"),"Fly",fly_number,"_Start",start,"_End_",end,"_Merged.mp4")
  
  #cmd_combine <- paste0("ffmpeg -i ",name_plots," -i ", name_fly_vid, ' -filter_complex "[0][1]scale2ref=\'oh*mdar\':\'if(lt(main_h,ih),ih,main_h)\'[0s][1s];[1s][0s]scale2ref=\'oh*mdar\':\'if(lt(main_h,ih),ih,main_h)\'[1s][0s];[0s][1s]hstack,setsar=1"', " -preset ultrafast ", name_merged) 
  #print(cmd_combine)
  #system(cmd_combine,wait=T)
  
  system("rm temp/temp_*")
  system("rm temp/Fly_Vid_Frame_*")
  
}

v_parperp <-function(d){
  p_0 <- d[1,]
  p_1 <- d[2,]
  p_2 <- d[3,]
  
  v_perp <- (((p_1[2] - p_0[2])*(p_2[1]-p_1[1])) - ((p_2[2] - p_1[2])*(p_1[1]-p_0[1])))/(sqrt((p_1[1]-p_0[1])^2 + (p_1[2]-p_0[2])^2))
  
  v_par <- (((p_2[1] - p_1[1])*(p_1[1] - p_0[1])) + ((p_2[2] - p_1[2])*(p_1[2] - p_0[2])))/(sqrt((p_1[1]-p_0[1])^2 + (p_1[2]-p_0[2])^2))
  
  output <-c(v_par,v_perp)
  names(output) <- c("v_par","v_perp")
  
  return(output)
  
}

euc.dist <- function(x1 = c(0,0), x2) {
  sqrt(sum((x1 - x2) ^ 2))
}

normal <- function(window) {
  p_0 = window[1,]
  p_1 = window[2,]
  p_2 = window[3,]
  direction <- c(-(((p_1[2] - p_0[2]) / euc.dist(p_0,p_1)) + ((p_2[2] - p_1[2]) / euc.dist(p_1,p_2))),(((p_1[1] - p_0[1]) / euc.dist(p_0,p_1)) + ((p_2[1] - p_1[1]) / euc.dist(p_1,p_2))))
  normalized <- direction/euc.dist(x2 = direction)
  output <- atan2(normalized[2],normalized[1])
  return(output)
}

get_time_diff <- function(start_time){
  start_time <- as.POSIXct(start_time,format="%m/%d/%Y %H:%M:%S",origin = "1970-01-01",tz="EST")
  start_time <- format(start_time,origin = "1970-01-01", format="%H:%M:%S")
  start_time <- strptime(start_time,format="%H:%M:%S")
  
  section_start <-  as.POSIXct("19:00:00",format="%H:%M:%S",origin = "1970-01-01",tz="EST")
  section_start <- format(section_start, format="%H:%M:%S",origin = "1970-01-01")
  section_start <- strptime(section_start,format="%H:%M:%S")
  
  time_to_section_start <- as.numeric(difftime(section_start,start_time,units = "s"))
  return(time_to_section_start)
}



unwrap <- function(data, tol = pi/1.2, step = pi) 
{
  data_length <- length(data)
  for (a in 1:(data_length - 1)) {
    b <- a + 1
    data_diff <- data[a] - data[b]
    if (data_diff <= (-tol)) {
      for (c in b:data_length) {
        data[c] <- data[c] - step
      }
    }
    if (data_diff >= (tol)) {
      for (c in b:data_length) {
        data[c] <- data[c] + step
      }
    }
  }
  return(data)
}
