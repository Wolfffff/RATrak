#Processing matrix data
#flies.activity
#
#
#
#
#

flies.activity <-
  function(trak,
           sleepThreshold = 5 * 60,
           deathThreshold = 1.5 * 60 ^ 2,
           mvFracThreshold = .7,
           mvMinThreshold = 5,
           mvSpacerThreshold = 3,
           emptyWellThreshold = 5,
           errorThreshold = 3 * 60 ^ 2,
           erroneousSleepDataThreshold = 5,
           speedUnit = 'dist/s',
           noiseLevelThreshold = 1) {
    #sleepThreshold = time of no movement to call sleep (s). Default 5 min
    #deathThreshold = Minimum time of no movement to call dead (s). If no movement > deathThreshold AND nomore movement after that point, call dead. Default 1.5 h
    #mvFracThreshold = threshold to call bout of movement. The fly needs to spend at least this fraction of time moving
    #mvMinThreshold = threshold to call bout of movement. The fly needs to spend at least this time moving in total
    #mvSpacerThreshold = threshold to call bout of movement. Bouts need to be separated by at least this much non-movement
    #emptyWellThreshold = If the total number of run lengths is < emptyWellThreshold, discard that well as empty/fly dead from the start
    #errorThreshold = threshold after which later movement is logged as warning. Default = 3h
    #erroneous(Sleep/Movement)DataThreshold = cutoff for condensing sleep bouts, if the next bout contains less than (erroneousDataThreshold) of (Movement/Sleep) frames, it is assumed to be erroneous data and processed as part of the prior bout. Default = 5
    #noiseLevelThreshold = cutoff to be considered to movement( if movement < noiseLevelThreshold, we assume movement = 0). Default 1
    
    
    sleepMin <- sleepThreshold * trak@hz
    deadMin <- deathThreshold * trak@hz
    mvSpaceMin <- mvSpacerThreshold * trak@hz
    errorMin <- errorThreshold * trak@hz
    
    if (nrow(trak@speed.regressed) > 0) {
      print('Using regressed speed')
      speedType <- 'Regressed'
      speed <- trak@speed.regressed
    }
    else if (nrow(trak@speed) > 0) {
      print('Using raw speed')
      speedType <- 'Raw'
      speed <- trak@speed
    }
    else
      stop('trak object does not contain speed data')
    speed[speed < noiseLevelThreshold] = 0 #Set speeds < noiseLevelThreshold to zero
    
    #Run length encoding of movement > 0, == streaks of movement
    speed.mov <-
      apply(
        speed,
        MARGIN = 2,
        FUN = function(x) {
          rle(x > 0)
        }
      )
    
    result <- list()
    pb <- txtProgressBar(min = 0,
                         max = length(speed.mov),
                         style = 3)
    for (i in 1:length(speed.mov)) {
      movement <- speed.mov[[i]]
      
      ##### Sleep and Death #####
      sleep <-
        movement$lengths > sleepMin &
        !movement$values # if streak length > sleepThreshold AND streak value == F (no movement), the fly is sleeping or dead
      if (any(sleep) &
          length(movement$lengths) > emptyWellThreshold) {
        sleepIndexes <- which(sleep)
        
        
        #Checking if any flies moved after being still > errorMin
        for (x in sleepIndexes) {
          if (movement$lengths[x] > errorMin) {
            if (!is.na(movement$lengths[x + 1])) {
              #Setup preferred error logging
              warning(paste(
                "Movement detected after error threshold gap for fly",
                i
              ))
            }
          }
        }
        
        #Merge sleep bouts that are separated by movement < erroneousSleepDataThreshold
        if (length(sleepIndexes) > 1) {
          sleepQC = sapply(
            1:(length(sleepIndexes) - 1),
            FUN = function(x) {
              if (sum(movement$lengths[1:sleepIndexes[x]]) != sum(movement$lengths)) {
                from <- sum(movement$lengths[1:sleepIndexes[x]])
                to <-
                  sum(movement$lengths[1:(sleepIndexes[x + 1] - 1)])
                return(sum(speed[from:to, i], na.rm = T) > erroneousSleepDataThreshold)
              }
            }
          )
          for (s in 1:length(sleepQC)) {
            if (!is.null(sleepQC[s])) {
              if (!sleepQC[s]) {
                #If the movement after the sleep bout < erroneousSleepDataThreshold, merge the sleep bouts
                #Merge bouts
                movement$lengths[sleepIndexes[s]] = sum(movement$lengths[sleepIndexes[s]:sleepIndexes[s + 1]])
                rmIndexes <-
                  (sleepIndexes[s] + 1):sleepIndexes[s + 1]
                movement$lengths = movement$lengths[-rmIndexes]
                movement$values = movement$values[-rmIndexes]
                
                #Update sleepIndexes
                sleepIndexes[(s + 1):length(sleepIndexes)] <-
                  sleepIndexes[(s + 1):length(sleepIndexes)] - length(rmIndexes)
              }
            }
          }
          #Redefine with updated movement from above code
          sleep <- movement$lengths > sleepMin & !movement$values
          sleepIndexes <- which(sleep)
        }
        
        
        
        lastNoMov <- sleepIndexes[length(sleepIndexes)]
        if (movement$lengths[lastNoMov] > deadMin &
            all(!movement$values[lastNoMov:length(sleep)])) {
          #If the last recorded no movement bout is > deadMin AND no movement after that point
          dead <-
            sum(movement$lengths[1:(lastNoMov - 1)]) #Call dead at the start of the last no movement bout
          
          #Call sleep for the previous no movement bouts
          if (sleepIndexes[1] == 1) {
            #If the first bout starts at timepoint 1, some tweaking is needed to get the indexing of the rle right
            if (length(sleepIndexes) > 2) {
              sleepStartTimes <-
                sapply(2:(length(sleepIndexes) - 1), function(x) {
                  sum(movement$lengths[1:(x - 1)])
                }) #Sum of every run length up to the movement start == movement start frame
              sleepStartTimes <- c(1, sleepStartTimes)
            }
            else
              sleepStartTimes <- 1
          }
          else{
            sleepStartTimes <-
              sapply(sleepIndexes[1:(length(sleepIndexes) - 1)], function(x) {
                sum(movement$lengths[1:(x - 1)])
              }) #Sum of every run length up to the sleep start == sleep start frame
          }
          
          sleepLengths <-
            movement$lengths[sleepIndexes[1:(length(sleepIndexes) - 1)]] #sleep lengths
          sleepNr <- length(sleepIndexes) - 1
        }
        else{
          dead <- NA
          
          if (sleepIndexes[1] == 1) {
            #If the first bout starts at timepoint 1, some tweaking is needed to get the indexing of the rle right
            if (length(sleepIndexes) == 1) {
              #The rare case of only one sleep bout, starting at timepoint 1
              sleepStartTimes <- 1
            }
            else{
              sleepStartTimes <-
                sapply(sleepIndexes[2:length(sleepIndexes)], function(x) {
                  sum(movement$lengths[1:(x - 1)])
                }) #Sum of every run length up to the movement start == movement start frame
              sleepStartTimes <- c(1, sleepStartTimes)
            }
          }
          else{
            sleepStartTimes <-
              sapply(sleepIndexes, function(x) {
                sum(movement$lengths[1:(x - 1)])
              }) #Sum of every run length up to the sleep start == sleep start frame
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
      mvSpacers <-
        movement$lengths > mvSpaceMin &
        !movement$values #Regions of non-movement > mvSpaceMin
      
      if (any(mvSpacers)) {
        mvSpacers.idx <- which(mvSpacers)
        
        #pre-defining these vectors to their maximum length to avoid dynamically growing them
        mvBouts.startTimes <- rep(NA, length(mvSpacers.idx))
        mvBouts.lengths <- rep(NA, length(mvSpacers.idx))
        mvBouts.mvTime <- rep(NA, length(mvSpacers.idx))
        mvBouts.avgSpeed <- rep(NA, length(mvSpacers.idx))
        
        if (movement$values[1])
          #If the first run length is movement, set start idx for first potential mv bout to 1
          start.idx <- 1
        else
          #If it's non-movement, set it to 2
          start.idx <- 2
        k <- 1
        for (j in 1:length(mvSpacers.idx)) {
          #Are there some clever vectorized solution to this?
          end.idx <- mvSpacers.idx[j] - 1
          potBout <-
            list(lengths = movement$lengths[start.idx:end.idx],
                 values = movement$values[start.idx:end.idx]) #Time window between the two non-movement regions == potential mv bout
          potBout.timeMv <-
            sum(potBout$lengths[potBout$values], na.rm = T) #The time spent moving in the bout
          potBout.timeTot <-
            sum(potBout$lengths, na.rm = T) #The total length of the bout, including non-movement
          
          if (!is.null(mvFracThreshold) & !is.null(mvMinThreshold))
            pass <-
            potBout.timeMv / potBout.timeTot > mvFracThreshold &
            potBout.timeMv > mvMinThreshold * trak@hz
          else if (!is.null(mvFracThreshold))
            pass <-
            potBout.timeMv / potBout.timeTot > mvFracThreshold
          else if (!is.null(mvMinThreshold))
            pass <- potBout.timeMv > mvMinThreshold * trak@hz
          else
            stop('One or both of mvMinThreshold and mvFracThreshold has to be specified')
          if (pass) {
            mvBouts.lengths[k] <- potBout.timeTot
            mvBouts.mvTime[k] <- potBout.timeMv
            
            #Start and end of the bout
            start <- sum(movement$lengths[1:(start.idx - 1)])
            end <- sum(movement$lengths[1:end.idx])
            mvBouts.startTimes[k] <- start
            
            #Avg speed in the bout
            mvBouts.avgSpeed[k] <-
              mean(speed[start:end, i], na.rm = T)
            
            k <- k + 1
          }
          start.idx <- mvSpacers.idx[j] + 1
        }
        mvBouts.startTimes <- na.omit(mvBouts.startTimes)
        mvBouts.lengths <- na.omit(mvBouts.lengths)
        mvBouts.mvTime <- na.omit(mvBouts.mvTime)
        mvBouts.nr <- length(mvBouts.startTimes)
        mvBouts.avgSpeed <- na.omit(mvBouts.avgSpeed)
        attributes(mvBouts.startTimes) <-
          attributes(mvBouts.lengths) <-
          attributes(mvBouts.mvTime) <-
          attributes(mvBouts.nr) <-
          attributes(mvBouts.avgSpeed) <- NULL
      }
      else{
        mvBouts.lengths <- NA
        mvBouts.mvTime <- NA
        mvBouts.nr <- 0
        mvBouts.startTimes <- NA
        mvBouts.avgSpeed <- NA
      }
      
      result[[i]] <-
        list(
          sleepLengths = sleepLengths,
          sleepNr = sleepNr,
          sleepStartTimes = sleepStartTimes,
          mvBouts.lengths = mvBouts.lengths,
          mvBouts.mvTime = mvBouts.mvTime,
          mvBouts.nr = mvBouts.nr,
          mvBouts.startTimes = mvBouts.startTimes,
          mvBouts.avgSpeed = mvBouts.avgSpeed,
          dead = dead
        )
      setTxtProgressBar(pb, i)
    }
    close(pb)
    trak@activity <-
      list(
        result = result,
        parameters = list(
          sleepThreshold = sleepThreshold,
          deathThreshold = deathThreshold,
          mvFracThreshold = mvFracThreshold,
          mvMinThreshold = mvMinThreshold,
          mvSpacerThreshold = mvSpacerThreshold,
          emptyWellThreshold = emptyWellThreshold,
          errorThreshold = errorThreshold,
          erroneousSleepDataThreshold = erroneousSleepDataThreshold,
          speedType = speedType
        )
      )
    return(trak)
  }


flies.activityByGroup <- function(trak,
                                  groups = NA,
                                  metricsToAvg = c("sleepNr",
                                                   "sleepLengths",
                                                   "mvBouts.lengths",
                                                   "mvBouts.mvTime",
                                                   "mvBouts.avgSpeed")) {
  if (is.na(groups)) {
    stop("You must provide features to groups(list of indices).")
  }
  
  combinedInfo = NULL
  # https://stackoverflow.com/questions/18538977/combine-merge-lists-by-elements-names
  for (i in 1:length(groups)) {
    combinedInfo[[names(groups)[i]]] = setNames(do.call(mapply, c(
      FUN = c, lapply(trak@activity$result[groups[[i]]], `[`, metricsToAvg)
    )), metricsToAvg)
  }
  return(combinedInfo)
}


flies.avgFeaturesByGroup <- function(trak,
                                     featuresToGroup = NA,
                                     groupBy = NA) {
  if (is.na(groupBy) || is.na(groupBy)) {
    stop("You must provide features to group and identifiers to group by.")
  }
  
  output = .trak(time = trak@time,
                 hz = trak@hz)
  # Assign to appropriate variables -- works around assign and get problem
  for (feature in featuresToGroup) {
    assign(feature, slot(trak, feature))
  }
  
  uniq = NULL
  for (i in 1:length(groupBy)) {
    uniq[i] = unique(trak@metadata[groupBy[i]])
  }
  
  combin = expand.grid(uniq)
  groups = list()
  colnames(combin) = groupBy
  names = apply(combin, 1, paste_)
  for (f in featuresToGroup) {
    tmp = get(f)
    for (row in 1:nrow(combin)) {
      indexes = which(apply(trak@metadata[groupBy], 1, function(x)
        all(x == combin[row, ])))
      tmpMeans = rowMeans(as.matrix(tmp[, indexes]))
      groups[[f]][[toString(names[row])]] = tmpMeans
    }
  }
  output@metadata = combin
  for (n in featuresToGroup) {
    slot(output, n) = as.data.frame(groups[[n]])
    colnames(slot(output, n)) = tolower(names)
  }
  output <- flies.activity(output)
  
  return(output)
}


flies.group <- function(trak, groupBy = NA) {
  if (is.na(groupBy)) {
    stop("You must provide identifiers to group by.")
  }
  uniq = NULL
  for (i in 1:length(groupBy)) {
    uniq[i] = unique(trak@metadata[groupBy[i]])
  }
  
  combin = expand.grid(uniq)
  groups = list()
  colnames(combin) = groupBy
  names = apply(combin, 1, paste_)
  for (row in 1:nrow(combin)) {
    indexes = which(unname(apply(trak@metadata[groupBy], 1, function(x)
      all(x == combin[row,]))))
    groups[[tolower(toString(names[row]))]] = indexes
  }
  return(groups)
}

#Convenience function to extract activity phenotypes within a time window from a trak object
flies.extractTimeWindow <-
  function(trak,
           start,
           end,
           timeScale = 'min',
           features = c(
             "centroid",
             "speed",
             "area",
             "theta",
             "majoraxislength",
             "minoraxislength",
             "direction",
             "orientation",
             "time"
           ),
           returnRawData = T,
           removeSamples = NULL) {
    if (timeScale %in% c('h', 'hour')) {
      timeFactor = trak@hz * 60 ^ 2
    } else if (timeScale %in% c('m', 'min', 'minute')) {
      timeFactor = trak@hz * 60
    } else if (timeScale == 'frames') {
      timeFactor = 1
    } else if (timeScale %in% c('s', 'sec')) {
      timeFactor = trak@hz
    } else{
      stop(paste('Did not recognize timeScale:', timeScale))
    }
    
    start <- start * timeFactor
    if (end == 'end') {
      end <- nrow(trak@speed) + 1
    } else{
      end <- end * timeFactor
    }
    
    activity.window <- list()
    j <- 1
    for (i in 1:length(trak@activity$result)) {
      if (!i %in% removeSamples) {
        if (length(trak@activity$result[[i]]$sleepStartTimes) != length(trak@activity$result[[i]]$sleepLengths))
          stop(
            paste(
              'Inconsistent sleep data for individual ',
              i,
              '. length(sleepStartTimes) != length(sleepLengths)',
              sep = ''
            )
          )
        mvBoutNrs <-
          c(
            length(trak@activity$result[[i]]$mvBouts.startTimes),
            length(trak@activity$result[[i]]$mvBouts.lengths),
            length(trak@activity$result[[i]]$mvBouts.avgSpeed)
          )
        if (length(unique(mvBoutNrs)) != 1)
          stop(
            paste(
              'Inconsistent movement data for individual ',
              i,
              '. length(mvBouts.startTimes), length(mvBouts.lengths), and length(mvBouts.avgSpeed) differ',
              sep = ''
            )
          )
        
        sleepInWindow <-
          trak@activity$result[[i]]$sleepStartTimes > start &
          trak@activity$result[[i]]$sleepStartTimes <= end
        mvInWindow <-
          trak@activity$result[[i]]$mvBouts.startTimes > start &
          trak@activity$result[[i]]$mvBouts.startTimes <= end
        if (!is.na(trak@activity$result[[i]]$dead) &
            (trak@activity$result[[i]]$dead > start &
             trak@activity$result[[i]]$dead <= end))
          dead <- trak@activity$result[[i]]$dead
        else
          dead <- NA
        
        
        activity.window[[j]] <-
          list(
            sleepLengths = trak@activity$result[[i]]$sleepLengths[sleepInWindow],
            sleepNr = sum(sleepInWindow),
            sleepStartTimes = trak@activity$result[[i]]$sleepStartTimes[sleepInWindow],
            mvBouts.lengths = trak@activity$result[[i]]$mvBouts.lengths[mvInWindow],
            mvBouts.mvTime = trak@activity$result[[i]]$mvBouts.mvTime[mvInWindow],
            mvBouts.nr = sum(mvInWindow),
            mvBouts.startTimes = trak@activity$result[[i]]$mvBouts.startTimes[mvInWindow],
            mvBouts.avgSpeed = trak@activity$result[[i]]$mvBouts.avgSpeed[mvInWindow],
            dead = dead
          )
        j <- j + 1
      }
    }
    
    trak@activity$result <- activity.window
    trak@metadata <-
      trak@metadata[!(1:nrow(trak@metadata) %in% removeSamples), ]
    if (returnRawData) {
      for (name in features[!(features == "time" &
                              features != "centroid")]) {
        if (nrow(slot(trak, name)) > 0)
          slot(trak, name) = slot(trak, name)[start:end, !((1:ncol(slot(trak, name))) %in% removeSamples)]
      }
      trak@time = trak@time[start:end]
      
      #Fixes for specfic types - regress and centroid
      if (nrow(trak@speed.regressed) > 0)
        trak@speed.regressed <-
        trak@speed.regressed[start:end, !(1:ncol(trak@speed.regressed) %in% removeSamples)]
      if (nrow(trak@centroid) > 0) {
        cols <-
          c(2 * removeSamples - 1, 2 * removeSamples) #The columns in trak@centroid corresponding to removeSamples
        trak@centroid <-
          trak@centroid[start:end, !(1:ncol(trak@centroid) %in% cols)]
      }
    }
    else{
      for (name in features) {
        slot(trak, name) = NULL
      }
    }
    return(trak)
  }

flies.calculateSpeed <- function(centroid, time = NULL) {
  if (ncol(centroid) %% 2 != 0)
    stop(
      'centroid matrix has uneven number of columns. It should contain xy coordinates in separate columns'
    )
  
  speed <-
    data.frame(matrix(nrow = nrow(centroid) - 1, ncol = ncol(centroid) / 2))
  samples <- seq(from = 1,
                 to = ncol(centroid) - 1,
                 by = 2)
  j <- 1
  for (i in samples) {
    speed[, j] <- sqrt(rowSums(diff(centroid[, i:(i + 1)]) ^ 2))
    j <- j + 1
  }
  
  if (is.null(time))
    warning(
      'No time data provided. Returning speed as pixel/frame. This will result in inconsistent estimates if the time between frames are not identical'
    )
  else
    speed <- speed / time
  return(speed)
}

flies.regressSpeed <-
  function(trak,
           center = c(664, 524),
           subset = NA) {
    #This function fits the linear model: speed ~ distance from center of image. The residual speed is added to the trak object
    
    #center = The camera center coordinates. The default (664, 524) correspond to a camera mode with resolution 1048 x 1328.
    #If tracking was done using a different camera mode, this has to be changed accordingly
    if (!is.na(subset) & nrow(trak@speed) > subset) {
      smpl <- sort(sample(x = 1:nrow(trak@speed), size = subset))
    }
    else{
      smpl <- 1:nrow(trak@speed)
    }
    
    xCols <- seq(from = 1,
                 to = ncol(trak@centroid) - 1,
                 by = 2)
    yCols <- seq(from = 2,
                 to = ncol(trak@centroid),
                 by = 2)
    cam_dist <-
      sqrt((trak@centroid[smpl, xCols] - center[1]) ^ 2 + (trak@centroid[smpl, yCols] - center[2]) ^
             2)
    
    
    cam_dist <-
      as.vector(as.matrix(cam_dist)) # Proper conversion to vector
    speed <- as.vector(as.matrix(trak@speed[smpl,]))
    filter <- !is.na(speed) & speed != 0
    
    model <- lm(speed[filter] ~ cam_dist[filter])
    if (summary(model)$coefficients[2, 4] < .01) {
      cam_dist <-
        sqrt((trak@centroid[, xCols] - center[1]) ^ 2 + (trak@centroid[, yCols] - center[2]) ^
               2)
      cam_dist <- as.vector(as.matrix(cam_dist))
      speed <- as.vector(as.matrix(trak@speed))
      
      #Regress out the "distance from camera" effect. I'm not using the intercept, since I don't want to center the speed around zero
      speed.regressed <- speed
      speed.regressed[filter] <-
        speed[filter] - model$coefficients[2] * cam_dist[filter]
      speed.regressed <-
        as.data.frame(matrix(speed.regressed, ncol = ncol(trak@speed))) #Reshape
    }
    else{
      cat('No significant \"distance from center\" effect detected')
      speed.regressed <- data.frame()
    }
    trak@speed.regressed <- speed.regressed
    return(trak)
  }

flies.extractActivity <- function(trak) {
  #Convenience function to extract averaged phenotypes per fly in data.frame format
  avgMvLength <-
    sapply(trak@activity$result, function(x) {
      mean(x$mvBouts.lengths, na.rm = T)
    })
  avgMvFrac <-
    sapply(trak@activity$result, function(x) {
      mean(x$mvBouts.mvTime / x$mvBouts.lengths, na.rm = T)
    })
  #Avg speed during movement bout
  mvBout.avgSpeed <-
    sapply(trak@activity$result, function(x) {
      mean(x$mvBouts.avgSpeed, na.rm = T)
    })
  avgSpeed <-
    as.vector(mapply(mean, trak@speed[, colnames(trak@speed)]))
  mvNr <- sapply(trak@activity$result, function(x) {
    x$mvBouts.nr
  })
  sleepNr <- sapply(trak@activity$result, function(x) {
    x$sleepNr
  })
  avgSleepLength <-
    sapply(trak@activity$result, function(x) {
      mean(x$sleepLengths, na.rm = T)
    })
  totSleep <-
    sapply(trak@activity$result, function(x) {
      sum(x$sleepLengths, na.rm = T)
    })
  
  if (identical(
    nrow(trak@metadata),
    length(avgMvLength),
    length(avgMvFrac),
    length(avgSpeed),
    length(mvNr),
    length(sleepNr),
    length(avgSleepLength),
    length(totSleep)
  )) {
    out <-
      data.frame(
        avgMvLength = avgMvLength,
        avgMvFrac = avgMvFrac,
        avgSpeed = avgSpeed,
        mvBout.avgSpeed = mvBout.avgSpeed,
        mvNr = mvNr,
        sleepNr = sleepNr,
        avgSleepLength = avgSleepLength,
        totSleep = totSleep,
        trak@metadata
      )
    return(out)
  }
  else if (identical(
    length(avgMvLength),
    length(avgMvFrac),
    length(avgSpeed),
    length(mvNr),
    length(sleepNr),
    length(avgSleepLength),
    length(totSleep)
  )) {
    warning(
      paste(
        'Found activity data for',
        length(avgMvLength),
        'samples but meta data contains',
        nrow(trak@metadata),
        'samples. Returning data.frame without meta data'
      )
    )
    out <-
      data.frame(
        avgMvLength = avgMvLength,
        avgMvFrac = avgMvFrac,
        avgSpeed = avgSpeed,
        mvNr = mvNr,
        sleepNr = sleepNr,
        avgSleepLength = avgSleepLength,
        totSleep = totSleep
      )
    return(out)
  }
  else{
    stop('Inconsistent number of samples in activity data')
  }
}

flies.extractMvBouts <-
  function(trak,
           boutLength,
           boutLength.w = 1,
           flies = NULL,
           makeEgocentric = T,
           timeScale = 'frames',
           start = 1,
           end = NA) {
    if (nrow(trak@centroid) == 0)
      stop('trak object must contain centroid data')
    if (length(trak@activity) == 0)
      stop('trak object must contain phenotypes in the activity slot')
    if (is.null(flies)) {
      flies <- 1:length(trak@metadata$well_orderastracked)
    }
    
    
    #Set time scale
    if (timeScale %in% c('h', 'hour')) {
      timeFactor = trak@hz * 60 ^ 2
    } else if (timeScale %in% c('m', 'min', 'minute')) {
      timeFactor = trak@hz * 60
    } else if (timeScale == 'frames') {
      timeFactor = 1
    } else if (timeScale %in% c('s', 'sec')) {
      timeFactor = trak@hz
    } else{
      stop(paste('Did not recognize timeScale:', timeScale))
    }
    
    if (is.na(end)) {
      endTime = length(trak@time)
    } else{
      endTime = end * timeFactor
    }
    startTime = start * timeFactor
    
    boutLength.w <-
      timeFactor * boutLength.w #Get bouts with length boutLength +- boutLength.w
    boutLength <- timeFactor * boutLength
    bouts.maxLength <-
      boutLength + boutLength.w #time windows of this size, starting at inferred start time, will be extracted from trak@centroid
    
    flies.mvBouts <- list()
    for (i in flies) {
      allBouts.lengths <- trak@activity$result[[i]]$mvBouts.lengths
      
      #Identify bouts with length boutLength +- boutLength.w
      bouts.idx <- which(
        allBouts.lengths > boutLength - boutLength.w
        &
          allBouts.lengths < boutLength + boutLength.w &
          trak@activity$result[[i]]$mvBouts.startTimes >= startTime &
          (
            trak@activity$result[[i]]$mvBouts.startTimes + trak@activity$result[[i]]$mvBouts.lengths - 1
          ) <= endTime
      )
      
      #Starts and lenghts
      bouts.starts <-
        trak@activity$result[[i]]$mvBouts.startTimes[bouts.idx]
      bouts.lengths <-
        trak@activity$result[[i]]$mvBouts.lengths[bouts.idx]
      
      #Check to see if fly has any usable bouts
      if (length(bouts.starts) == 0) {
        flies.mvBouts[[i]] <- NA
        next
      }
      
      #Get centroid data for bouts. There are probably some more efficient way to do this rather than looping, maybe using reshape for instance
      bouts.centroidIdx <-
        sapply(bouts.starts, function(x) {
          x:(x + bouts.maxLength)
        }) #Gives a matrix with one column per bout, with the indices of that bout in trak@centroid note that this creates a static length later on.
      mvBouts <-
        data.frame(matrix(
          nrow = 2 * nrow(bouts.centroidIdx),
          ncol = ncol(bouts.centroidIdx)
        )) #To store bouts
      colnames(mvBouts) <-
        paste0('fly',
               i,
               '_',
               bouts.starts,
               '_',
               bouts.starts + bouts.lengths - 1) #Naming every bout by fly number and bout start_end, where end is the one inferred by flies.activity, rather than the window extracted here
      for (j in 1:length(bouts.idx)) {
        xy <- trak@centroid[bouts.centroidIdx[, j], c(i * 2 - 1, i * 2)]
        #print(dim(xy))
        if (makeEgocentric) {
          xy[, 2] <- -xy[, 2]
          
          #Center & Rotate every bout
          #If I'm interpreting this code correctly, this is simply the start time of the bout
          #print(paste0("Bout Start Time: ", bouts.centroidIdx[1,j]))
          
          #https://github.com/de-Bivort-Lab/margo/wiki/Options-and-Parameters -- see entry on heading
          #This is getting the starting direction in raeds for a given bout -- it may need adjustment as it the first
          
          # This is using the direction(weighted by speed to determine the direction the fly is traveling for the transformation.)
          # Note that we dont actually have any info about the "directionality" other than heading(frame to frame angle) and orientation(major axis of ellipse against x axis)
          if (sum(trak@direction[bouts.centroidIdx[1:5 * trak@hz, j], i] * trak@speed[bouts.centroidIdx[1:trak@hz, j], i]) < 0) {
            orientation <-
              (-median(trak@orientation[bouts.centroidIdx[1:trak@hz, j], i]) + 90) *
              (pi / 180)
            
          } else{
            orientation <-
              (-median(trak@orientation[bouts.centroidIdx[1:trak@hz, j], i]) - 90) *
              (pi / 180)
          }
          rotationMatrix <-
            matrix(
              c(
                cos(orientation),-sin(orientation),
                sin(orientation),
                cos(orientation)
              ),
              nrow = 2,
              ncol = 2,
              byrow = TRUE
            )
          center <- xy[1, 1:2]
          xy <- t(apply(as.matrix(xy), 1, '-', t(center)))
          xy <-
            t(apply(xy, 1, function(x, rotationMatrix) {
              rotationMatrix %*% x
            }, rotationMatrix = rotationMatrix))
          
        }
        
        mvBouts[, j] <- c(xy[, 1], xy[, 2])
      }
      
      #Export
      flies.mvBouts[[i]] <- mvBouts
    }
    return(flies.mvBouts)
  }