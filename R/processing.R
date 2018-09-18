#Processing matrix data
#flies.sleepActivity
#flies.avgByGroup
#
#
#


flies.sleepActivity <- function(trak, sleepThreshold = 5*60, deathThreshold = 1.5*60^2, mvThreshold = 3, emptyWellThreshold = 5, errorThreshold = 3*60^2, erroneousSleepDataThreshold = 5, smoothingFactor = 10){
  #sleepThreshold = time of no movement to call sleep (s). Default 5 min
  #deathThreshold = Minimum time of no movement to call dead (s). If no movement > deathThreshold AND nomore movement after that point, call dead. Default 1.5 h
  #mvThreshold = threshold to call bout of continous movement. Default 2s
  #emptyWellThreshold = If the total number of run lengths is < emptyWellThreshold, discard that well as empty/fly dead from the start
  #errorThreshold = threshold after which later movement is logged as warning. Default = 3h
  #erroneous(Sleep/Movement)DataThreshold = cutoff for codensing sleep bouts, if the next bout contains less than (erroneousDataThreshold) of (Movement/Sleep) frames, it is assumed to be erroneous data and processed as part of the prior bout. Default = 5
  sleepMin <- sleepThreshold*trak@hz
  deadMin <- deathThreshold*trak@hz
  mvMin <- mvThreshold*trak@hz
  errorMin <- errorThreshold*trak@hz
  
  #Naming
  speed <- trak@speed

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
        boutSpeeds[t] <- mean(speed[mvStartTimes[t]:mvEndTimes[t], i], na.rm = T)
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
  trak@activity <- result
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

