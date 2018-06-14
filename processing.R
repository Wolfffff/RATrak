#Processing matrix data
#flies.sleepActivity
#lmp - fit linear model
#
#
#


flies.sleepActivity <- function(centroidDist, sleepThreshold = 5*60, deathThreshold = 1.5*60^2, mvThreshold = 3, hz = 5, emptyWellThreshold = 5, errorThreshold = 3*60^2, erroneousSleepDataThreshold = 5, smoothingFactor = 10){
  #sleepThreshold = time of no movement to call sleep (s). Default 5 min
  #deathThreshold = Minimum time of no movement to call dead (s). If no movement > deathThreshold AND nomore movement after that point, call dead. Default 1.5 h
  #mvThreshold = threshold to call bout of continous movement. Default 2s
  #hz = movement aquisition rate. Default 5 Hz
  #emptyWellThreshold = If the total number of run lengths is < emptyWellThreshold, discard that well as empty/fly dead from the start
  #errorThreshold = threshold after which later movement is logged as warning. Default = 3h
  #erroneous(Sleep/Movement)DataThreshold = cutoff for codensing sleep bouts, if the next bout contains less than (erroneousDataThreshold) of (Movement/Sleep) frames, it is assumed to be erroneous data and processed as part of the prior bout. Default = 5
  sleepMin <- sleepThreshold*hz
  deadMin <- deathThreshold*hz
  mvMin <- mvThreshold*hz
  errorMin <- errorThreshold*hz
  
  #Run length encoding of movement > 0, == streaks of movement
  centroidDist.mov <- apply(centroidDist, MARGIN = 2, FUN = function(x){rle(x > 0)})
  
  result <- list()
  for(i in 1:length(centroidDist.mov)){
    movement <- centroidDist.mov[[i]]
    
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
              return(sum(centroidDist[from:to, i], na.rm = T) > erroneousSleepDataThreshold)
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
          sleepStartTimes <- sapply(sleepIndexes[2:length(sleepIndexes)], function(x){ sum(movement$lengths[1:(x-1)]) } ) #Sum of every run length up to the movement start == movement start frame
          sleepStartTimes <- c(1, sleepStartTimes)
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
    mvBouts <- movement$lengths > mvMin & movement$values
    
    
    if(any(mvBouts)){
      mvBoutsIndexes <- which(mvBouts)
      c <- 1
      
      while(c <= length(mvBoutsIndexes)){
        #iterate forward until we reach a "blocker" - that is a non movement bout of more than (parameter)
        #Rename smoothing factor to smothing more reasonable
        while(!is.na(movement$length[mvBoutsIndexes[c] + 1]) &&( movement$length[mvBoutsIndexes[c] + 1] < smoothingFactor || movement$values[mvBoutsIndexes[c] + 1] == TRUE)){
          movement$lengths[mvBoutsIndexes[c]] <- movement$lengths[mvBoutsIndexes[c]] + movement$lengths[mvBoutsIndexes[c] + 1]
          movement$lengths <-movement$lengths[-(mvBoutsIndexes[c] + 1)]
          movement$values <-movement$values[-(mvBoutsIndexes[c] + 1)]
        }
        #update two
        mvBouts <- movement$lengths > mvMin & movement$values
        mvBoutsIndexes <- which(mvBouts)
        c <- c + 1
      }
      
      
      
      #Get mv lengths, start times etc
      mvLengths <- movement$lengths[mvBoutsIndexes]
      mvNr <- sum(mvBouts)
      if(mvBoutsIndexes[1] == 1){ #If the first bout starts at timepoint 1, some tweaking is needed to get the indexing of the rle right
        mvStartTimes <- sapply(mvBoutsIndexes[-1], function(x){ sum(movement$lengths[1:(x-1)]) } ) #Sum of every run length up to the movement start == movement start frame
        mvStartTimes <- c(1, mvStartTimes)
      }
      else{
        mvStartTimes <- sapply(mvBoutsIndexes, function(x){ sum(movement$lengths[1:(x-1)]) } ) #Sum of every run length up to the movement start == movement start frame
      }
      mvEndTimes <- sapply(mvBoutsIndexes, function(x){ sum(movement$lengths[1:x]) } ) 
      #Get the average speed in every bout
      boutSpeeds <- rep(NA, length(mvStartTimes))
      for (t in 1:length(mvStartTimes)) {
        boutSpeeds[t] <- mean(centroidDist[mvStartTimes[t]:mvEndTimes[t], i], na.rm = T)
      }
    }
    else{
      mvLengths <- NA 
      mvNr <- 0
      mvStartTimes <- NA
      boutSpeeds <- NA
    }
    
    result[[i]] <- list(sleepLengths = sleepLengths, sleepNr = sleepNr, sleepStartTimes = sleepStartTimes,
                        mvLengths = mvLengths, mvNr = mvNr, mvStartTimes = mvStartTimes, dead = dead, boutSpeeds = boutSpeeds)
  }
  return(result)
}


#Get p-value from a LinearModel
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}