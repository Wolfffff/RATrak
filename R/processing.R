#Processing matrix data
#flies.sleepActivity
#flies.avgByGroup
#
#
#


flies.activity <- function(trak, sleepThreshold = 5*60, deathThreshold = 1.5*60^2, mvFracThreshold = .7, mvMinThreshold = 5, mvSpacerThreshold = 3, emptyWellThreshold = 5, errorThreshold = 3*60^2, erroneousSleepDataThreshold = 5, speedUnit = 'dist/s'){
  #sleepThreshold = time of no movement to call sleep (s). Default 5 min
  #deathThreshold = Minimum time of no movement to call dead (s). If no movement > deathThreshold AND nomore movement after that point, call dead. Default 1.5 h
  #mvFracThreshold = threshold to call bout of movement. The fly needs to spend at least this fraction of time moving 
  #mvMinThreshold = threshold to call bout of movement. The fly needs to spend at least this time moving in total
  #mvSpacerThreshold = threshold to call bout of movement. Bouts need to be separated by at least this much non-movement
  #emptyWellThreshold = If the total number of run lengths is < emptyWellThreshold, discard that well as empty/fly dead from the start
  #errorThreshold = threshold after which later movement is logged as warning. Default = 3h
  #erroneous(Sleep/Movement)DataThreshold = cutoff for codensing sleep bouts, if the next bout contains less than (erroneousDataThreshold) of (Movement/Sleep) frames, it is assumed to be erroneous data and processed as part of the prior bout. Default = 5
  sleepMin <- sleepThreshold*trak@hz
  deadMin <- deathThreshold*trak@hz
  mvSpaceMin <- mvSpacerThreshold*trak@hz
  errorMin <- errorThreshold*trak@hz
  
  if(nrow(trak@speed.regressed) > 0){
    print('Using regressed speed')
    speedType <- 'Regressed'
    speed <- trak@speed.regressed
  }
  else if(nrow(trak@speed) > 0){
    print('Using raw speed')
    speedType <- 'Raw'
    speed <- trak@speed
  }
  else
    stop('trak object does not contain speed data')
  
  #Run length encoding of movement > 0, == streaks of movement
  speed.mov <- apply(speed, MARGIN = 2, FUN = function(x){rle(x > 0)})
  
  result <- list()
  for(i in 1:length(speed.mov)){
    movement <- speed.mov[[i]]
    
    ##### Sleep and Death #####
    sleep <- movement$lengths > sleepMin & !movement$values # if streak length > sleepThreshold AND streak value == F (no movement), the fly is sleeping or dead
    if(any(sleep) & length(movement$lengths) > emptyWellThreshold){
      sleepIndexes <- which(sleep)
      
      
      #Checking if any flies moved after being still > errorMin
      for (x in sleepIndexes) {
        if(movement$lengths[x] > errorMin){
          if(!is.na(movement$lengths[x+1])){
            #Setup preferred error logging
            warning(paste("Movement detected after error threshold gap for fly", i))
          }
        }
      }
      
      #Merge sleep bouts that are separated by movement < erroneousSleepDataThreshold
      if(length(sleepIndexes) > 1){
        sleepQC = sapply(
          1:(length(sleepIndexes) - 1),
          FUN = function(x) {
            if (sum(movement$lengths[1:sleepIndexes[x]]) != sum(movement$lengths)) {
              from <- sum(movement$lengths[1:sleepIndexes[x]])
              to <- sum(movement$lengths[1:(sleepIndexes[x + 1] - 1)])
              return(sum(speed[from:to, i], na.rm = T) > erroneousSleepDataThreshold)
            }
          }
        )
        for (s in 1:length(sleepQC)) {
          if (!is.null(sleepQC[s])){
            if(!sleepQC[s]) { #If the movement after the sleep bout < erroneousSleepDataThreshold, merge the sleep bouts
              #Merge bouts
              movement$lengths[sleepIndexes[s]] = sum(movement$lengths[ sleepIndexes[s]:sleepIndexes[s+1] ])
              rmIndexes <- (sleepIndexes[s] + 1):sleepIndexes[s+1]
              movement$lengths = movement$lengths[-rmIndexes]
              movement$values = movement$values[-rmIndexes]
              
              #Update sleepIndexes
              sleepIndexes[(s+1):length(sleepIndexes)] <- sleepIndexes[(s+1):length(sleepIndexes)] - length(rmIndexes)
            }
          }
        }
        #Redefine with updated movement from above code
        sleep <- movement$lengths > sleepMin & !movement$values
        sleepIndexes <- which(sleep)
      }
      
      
      
      lastNoMov <- sleepIndexes[length(sleepIndexes)]
      if(movement$lengths[lastNoMov] > deadMin & all(!movement$values[lastNoMov:length(sleep)])){ #If the last recorded no movement bout is > deadMin AND no movement after that point
        dead <- sum(movement$lengths[1:(lastNoMov - 1)]) #Call dead at the start of the last no movement bout
        
        #Call sleep for the previous no movement bouts
        if(sleepIndexes[1] == 1){ #If the first bout starts at timepoint 1, some tweaking is needed to get the indexing of the rle right
          sleepStartTimes <- sapply(sleepIndexes[2:(length(sleepIndexes) - 1)], function(x){ sum(movement$lengths[1:(x-1)]) } ) #Sum of every run length up to the movement start == movement start frame
          sleepStartTimes <- c(1, sleepStartTimes)
        }
        else{
          sleepStartTimes <- sapply(sleepIndexes[1:(length(sleepIndexes) - 1)], function(x){ sum(movement$lengths[1:(x-1)]) } ) #Sum of every run length up to the sleep start == sleep start frame
        }
        
        sleepLengths <- movement$lengths[sleepIndexes[1:(length(sleepIndexes) - 1)]] #sleep lengths
        sleepNr <- length(sleepIndexes) - 1
      }
      else{
        dead <- NA
        
        if(sleepIndexes[1] == 1){ #If the first bout starts at timepoint 1, some tweaking is needed to get the indexing of the rle right
          if(length(sleepIndexes) == 1){ #The rare case of only one sleep bout, starting at timepoint 1
            sleepStartTimes <- 1
          }
          else{
            sleepStartTimes <- sapply(sleepIndexes[2:length(sleepIndexes)], function(x){ sum(movement$lengths[1:(x-1)]) } ) #Sum of every run length up to the movement start == movement start frame
            sleepStartTimes <- c(1, sleepStartTimes)
          }
        }
        else{
          sleepStartTimes <- sapply(sleepIndexes, function(x){ sum(movement$lengths[1:(x-1)]) } ) #Sum of every run length up to the sleep start == sleep start frame
        }
        sleepLengths <- movement$lengths[sleep] #sleep lengths
        sleepNr <- sum(sleep)
      }
    }
    else{
      dead <- NA
      sleepLengths <- NA
      sleepNr <- 0
      sleepStartTimes <- NA
    }
    
    
    ##### Movement Bouts #####
    # mvBouts <- movement$lengths > mvMin & movement$values
    mvSpacers <- movement$lengths > mvSpaceMin & !movement$values #Regions of non-movement > mvSpaceMin
    
    if(any(mvSpacers)){
      mvSpacers.idx <- which(mvSpacers)
      
      #pre-defining these vectors to their maximum length to avoid dynamically growing them
      mvBouts.startTimes <- rep(NA, length(mvSpacers.idx))
      mvBouts.lengths <- rep(NA, length(mvSpacers.idx))
      mvBouts.mvTime <- rep(NA, length(mvSpacers.idx))
      mvBouts.avgSpeed <- rep(NA, length(mvSpacers.idx))
      
      start.idx <- 1
      k <- 1
      for(j in 1:length(mvSpacers.idx)){ #Are there some clever vectorized solution to this? 
        end.idx <- mvSpacers.idx[j] - 1
        potBout <- list(lengths = movement$lengths[start.idx:end.idx], values = movement$values[start.idx:end.idx]) #Time window between the two non-movement regions == potential mv bout
        potBout.timeMv <- sum(potBout$lengths[potBout$values], na.rm = T) #The time spent moving in the bout
        potBout.timeTot <- sum(potBout$lengths, na.rm = T) #The total length of the bout, including non-movement
        
        if(!is.null(mvFracThreshold) & !is.null(mvMinThreshold))
          pass <- potBout.timeMv/potBout.timeTot > mvFracThreshold & potBout.timeMv > mvMinThreshold*trak@hz
        else if(!is.null(mvFracThreshold))
          pass <- potBout.timeMv/potBout.timeTot > mvFracThreshold
        else if(!is.null(mvMinThreshold))
          pass <- potBout.timeMv > mvMinThreshold*trak@hz
        else
          stop('One or both of mvMinThreshold and mvFracThreshold has to be specified')
        if(pass){
          mvBouts.lengths[k] <- potBout.timeTot 
          mvBouts.mvTime[k] <- potBout.timeMv 
          
          #Start and end of the bout
          start <- sum(movement$lengths[1:(start.idx-1)])
          end <- sum(movement$lengths[1:end.idx])
          mvBouts.startTimes[k] <- start
          
          #Avg speed in the bout
          mvBouts.avgSpeed[k] <- mean(speed[start:end, i], na.rm = T)
          
          k <- k+1
        }
        start.idx <- mvSpacers.idx[j] + 1
      }
      mvBouts.startTimes <- na.omit(mvBouts.startTimes)
      mvBouts.lengths <- na.omit(mvBouts.lengths)
      mvBouts.mvTime <- na.omit(mvBouts.mvTime)
      mvBouts.nr <- length(mvBouts.startTimes)
      mvBouts.avgSpeed <- na.omit(mvBouts.avgSpeed)
      attributes(mvBouts.startTimes) <- attributes(mvBouts.lengths) <- attributes(mvBouts.mvTime) <- attributes(mvBouts.nr) <- attributes(mvBouts.avgSpeed) <- NULL
    }
    else{
      mvBouts.lengths <- NA
      mvBouts.mvTime <- NA
      mvBouts.nr <- 0
      mvBouts.startTimes <- NA
      mvBouts.avgSpeed <- NA
    }
    
    result[[i]] <- list(sleepLengths = sleepLengths, sleepNr = sleepNr, sleepStartTimes = sleepStartTimes,
                        mvBouts.lengths = mvBouts.lengths, mvBouts.mvTime = mvBouts.mvTime, mvBouts.nr = mvBouts.nr, 
                        mvBouts.startTimes = mvBouts.startTimes, mvBouts.avgSpeed = mvBouts.avgSpeed, dead = dead)
  }
  trak@activity <- list(result = result, parameters = list(sleepThreshold = sleepThreshold, deathThreshold = deathThreshold, 
                                                           mvFracThreshold = mvFracThreshold, mvMinThreshold = mvMinThreshold, 
                                                           mvSpacerThreshold = mvSpacerThreshold, emptyWellThreshold = emptyWellThreshold, 
                                                           errorThreshold = errorThreshold, erroneousSleepDataThreshold = erroneousSleepDataThreshold,
                                                           speedType = speedType))
  return(trak)
}



flies.avgByGroup <- function(trak, sex = T, treatments = T) {
  if(sex == T){
    sex <- as.vector(trak@metadata$Male)
  }else{ #For clarity
    sex <- NA
  }
  
  if(treatments == T){
    treatments <- as.vector(trak@metadata$Treatment)
  }else{
    treatments <- NA
  }
  speed <-trak@speed
  
  
  if(is.na(sex) && is.na(treatments)){
    stop("Neither sex nor treatments provided")
  }
  groupedTreatments = c(NA) #First elements to NA to keep NA checks false until populated
  groupedSex <- c(NA)
  if(!is.na(sex[1])){ #Assumes sex = logical vector. T == male
    if(!is.na(treatments[1])){
      
      treatments.uniq <- unique(treatments)
      nTreatments <- length(treatments.uniq)
      speed.groupAvg <- as.data.frame(matrix(nrow = nrow(speed), ncol = 2*nTreatments))
      
      #speed.groupAvg will be organized as:
      # - column 1 TO nTreatments = males in each treatment
      # - column nTreatments + 1 TO 2*nTreatments = females in each treatment
      
      for(i in 1:nTreatments){ #There should be a smarter way of doing this within the data.table indexing framework
        #males
        speed.groupAvg[,i] <- rowMeans(as.matrix(speed[, treatments == treatments.uniq[i] & sex]))
        groupedSex[i] = TRUE
        groupedTreatments[i] = as.character(treatments.uniq[i])
        
        #females
        speed.groupAvg[,i + nTreatments] <- rowMeans(as.matrix(speed[, treatments == treatments.uniq[i] & !sex]))
        groupedSex[i+nTreatments] = FALSE
        groupedTreatments[i+nTreatments] = as.character(treatments.uniq[i])
      }
      
      #Calculate SDs of the above means - Not implemented
      # if(sdShading){
      #   speed.groupSD <- as.data.frame(matrix(nrow = nrow(speed), ncol = 2*nTreatments))
      #
      #   for(i in 1:nTreatments){ #There should be a smarter way of doing this within the data.table indexing framework
      #     speed.groupSD[,i] <- rowSds(as.matrix(speed[, treatments == treatments.uniq[i] & sex])) #males
      #     speed.groupSD[,i + nTreatments] <- rowSds(as.matrix(speed[, treatments == treatments.uniq[i] & !sex])) #females
      #   }
      # }
    }
    #Just sex
    else{
      groupedSex <- c(1,2) #Male & female
      speed.groupAvg <- as.data.frame(matrix(nrow = nrow(speed), ncol = 2))
      
      #Direct grouping by sex metadata as passed in
      speed.groupAvg[,1] <- rowMeans(as.matrix(speed[, sex])) #males
      speed.groupAvg[,2] <- rowMeans(as.matrix(speed[, !sex])) #females
      
    }
  }
  #Just treatments - no sex
  else if(!is.na(treatments[1])) {
    treatments.uniq <- unique(treatments)
    nTreatments <- length(treatments.uniq)
    speed.groupAvg <- matrix(nrow = nrow(speed), ncol = nTreatments)
    #All in treatment group to group
    for(i in 1:nTreatments){ #There should be a smarter way of doing this within the data.table indexing framework
      speed.groupAvg[,i] <- rowMeans(as.matrix(speed[, treatments == treatments.uniq[i]]))
      groupedTreatments[i] = as.character(treatments.uniq[i])    
    }
  }
  class <- setClass("trak", slots = c(speed="data.frame", activity="list",metadata = "data.frame", hz = "numeric"))
  
  output <- class(speed = as.data.frame(speed.groupAvg), metadata = data.frame("Male" = groupedSex, "Treatment" = groupedTreatments),hz = trak@hz)
  output <- flies.sleepActivity(output)
  return(output)
}


flies.extractActivity <- function(trak, start, end, timeScale, returnSpeed = F){
  #Convenience function to extract activity phenotypes within a time window from a trak object
  if(timeScale == "h"){
    timeFactor = trak@hz*60^2
  }else if(timeScale == "m"){
    timeFactor = trak@hz*60
  }else{
    timeFactor = trak@hz
  }
  start <- start*timeFactor
  if(end == 'end')
    end <- nrow(trak@speed) + 1
  else
    end <- end*timeFactor
  
  activity.window <- list()
  for(i in 1:length(trak@activity)){
    if(length(trak@activity[[i]]$sleepStartTimes) != length(trak@activity[[i]]$sleepLengths))
      stop(paste('Inconsistent sleep data for individual ', i, '. length(sleepStartTimes) != length(sleepLengths)', sep = ''))
    mvBoutNrs <- c(length(trak@activity[[i]]$mvStartTimes), length(trak@activity[[i]]$mvLengths), length(trak@activity[[i]]$boutSpeeds))
    if(length(unique(mvBoutNrs)) != 1)
      stop(paste('Inconsistent movement data for individual ', i, '. length(mvStartTimes), length(mvLengths), and length(boutSpeeds) differ', sep = ''))
    
    sleepInWindow <- trak@activity[[i]]$sleepStartTimes > start & trak@activity[[i]]$sleepStartTimes <= end
    mvInWindow <- trak@activity[[i]]$mvStartTimes > start & trak@activity[[i]]$mvStartTimes <= end
    if(!is.na(trak@activity[[i]]$dead) & (trak@activity[[i]]$dead > start & trak@activity[[i]]$dead <= end))
      dead <- trak@activity[[i]]$dead
    else
      dead <- NA
    
    
    activity.window[[i]] <- list(sleepLengths = trak@activity[[i]]$sleepLengths[sleepInWindow], 
                                 sleepNr = sum(sleepInWindow), 
                                 sleepStartTimes = trak@activity[[i]]$sleepStartTimes[sleepInWindow],
                                 mvLengths = trak@activity[[i]]$mvLengths[mvInWindow], 
                                 mvNr = sum(mvInWindow), 
                                 mvStartTimes = trak@activity[[i]]$mvStartTimes[mvInWindow], 
                                 dead = dead, 
                                 boutSpeeds = trak@activity[[i]]$boutSpeeds[mvInWindow])
  }
  trak@activity <- activity.window
  if(!returnSpeed)
    trak@speed <- data.frame()
  else
    trak@speed <- trak@speed[start:end, ]
  return(trak)
}

flies.calculateSpeed <- function(centroid, time = NULL){
  if(ncol(centroid) %% 2 != 0)
    stop('centroid matrix has uneven number of columns. It should contain xy coordinates in separate columns')
  
  speed <- data.frame(matrix(nrow = nrow(centroid)-1, ncol = ncol(centroid)/2))
  samples <- seq(from = 1, to = ncol(centroid) - 1, by = 2)
  j <- 1
  for(i in samples){
    speed[,j] <- sqrt(rowSums( diff(centroid[, i:(i+1)])^2 ))
    j <- j+1
  }
  
  if(is.null(time))
    warning('No time data provided. Returning speed as pixel/frame. This will result in inconsistent estimates if the time between frames are not identical')
  else
    speed <- speed/time
  return(speed)
}

flies.regressSpeed <- function(trak, center = c(664, 524)){
  #This function fits the linear model: speed ~ distance from center of image. The residual speed is added to the trak object
  
  #center = The camera center coordinates. The default (664, 524) correspond to a camera mode with resolution 1048 x 1328. 
  #If tracking was done using a different camera mode, this has to be changed accordingly
  if(nrow(trak@speed) > 50000)
    smpl <- sort(sample(x = 1:nrow(trak@speed), size = 50000))
  else
    smpl <- 1:nrow(trak@speed)
  
  xCols <- seq(from = 1, to = ncol(trak@centroid) - 1, by = 2)
  yCols <- seq(from = 2, to = ncol(trak@centroid), by = 2)
  cam_dist <- sqrt((trak@centroid[smpl, xCols] - center[1])^2 + (trak@centroid[smpl, yCols] - center[2])^2)
  
  #Model
  cam_dist <- as.vector(cam_dist)
  speed <- as.vector(as.matrix(trak@speed[smpl,]))
  filter <- !is.na(speed) & speed != 0
  model <- lm(speed[filter] ~ cam_dist[filter])
  
  if(summary(model)$coefficients[2, 4] < .01){
    cam_dist <- sqrt((trak@centroid[, xCols] - center[1])^2 + (trak@centroid[, yCols] - center[2])^2)
    cam_dist <- as.vector(cam_dist)
    speed <- as.vector(as.matrix(trak@speed))
    
    #Regress out the "distance from camera" effect. I'm not using the intercept, since I don't want to center the speed around zero
    speed.regressed <- speed
    speed.regressed[filter] <- speed[filter] - model$coefficients[2]*cam_dist[filter]
    speed.regressed <- as.data.frame(matrix(speed.regressed, ncol = ncol(trak@speed))) #Reshape
  }
  else{
    cat('No significant \"distance from center\" effect detected')
    speed.regressed <- data.frame()
  }
  
  trak@speed.regressed <- speed.regressed
  return(trak)
}


