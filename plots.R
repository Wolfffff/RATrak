#Plots function 
#plot.flyMv_allFigs - All figures
#plot.flyMv_rollAvg_grouped
#plot.flyMovement_plate
#plot.highlightBouts
#plot.flyMv_rollNoMov
#plot.flyMv_avgSleepLength
#plot.flyMv_sleepNr
#plot.flyMv_avgMvLength
#
#
#



plot.flyMv_allFigs <- function(centroidDist, activity, fileBaseName, 
                               sex = NA, treatments = NA, hz = 5, time = 'min', 
                               treatmentLevels = NA, width = 5*60^2, by = 5*60*30, avgMv = T, noMv = F){ 
  #Wrapper function that produces various figures of fly activity
  
  #Cummulative movement 
  file <- paste(fileBaseName, '_cumMv.png', sep = '')
  png(filename = file, width = 1200, height = 800)
  plot.flyMv_cumMv(centroidDist = centroidDist, sex = sex, 
                   treatments = treatments, time = time, treatmentLevels = treatmentLevels, hz = hz)
  dev.off()
  
  #Survival
  file <- paste(fileBaseName, '_survival.png', sep = '')
  png(filename = file, width = 1200, height = 800)
  plot.flyMv_survival(activity = activity, treatments = treatments, treatmentLevels = treatmentLevels)
  dev.off()
  
  #Movement and sleep comparisons across whole experiment
  file <- paste(fileBaseName, '_sleepAndMvBouts.png', sep = '')
  png(filename = file, width = 1200, height = 800)
  par(mfrow = c(2,2))
  plot.flyMv_avgSleepLength(activity = activity, treatments = treatments, treatmentLevels = treatmentLevels)
  plot.flyMv_sleepNr(activity = activity, treatments = treatments, treatmentLevels = treatmentLevels)
  mtext(text = 'Sleep', side = 3, cex = 4, line = .5, adj = -.3)
  
  plot.flyMv_avgMvLength(activity = activity, treatments = treatments, treatmentLevels = treatmentLevels)
  plot.flyMv_mvNr(activity = activity, treatments = treatments, treatmentLevels = treatmentLevels)
  mtext(text = 'Movement', side = 3, cex = 4, line = .5, adj = -.5)
  dev.off()
  
  #Average movement per window
  if(avgMv){
    file <- paste(fileBaseName, '_avgMvPerWindow.png', sep = '')
    png(filename = file, width = 1200, height = 800)
    plot.flyMv_rollAvg(centroidDist = centroidDist, sex = sex, 
                       treatments = treatments, time = time, treatmentLevels = treatmentLevels, width = width, by = by, hz = hz)
    dev.off()
  }
  
  #Fraction no movement per window
  if(noMv){
    file <- paste(fileBaseName, '_fracNoMvPerWindow.png', sep = '')
    png(filename = file, width = 1200, height = 800)
    plot.flyMv_rollNoMov(centroidDist = centroidDist, sex = sex, 
                         treatments = treatments, time = time, treatmentLevels = treatmentLevels, width = width, by = by, hz = hz)
    dev.off()
  }
}



plot.flyMv_rollAvg_grouped <- function(centroidDist, sex = NA, treatments = NA, hz = 5, time = 'min', 
                                       treatmentLevels = NA, title = NA, 
                                       width = 5*60^2, by = 5*60*30, sdShading = F, ...){
  #Function under construction. sd-shading and no treatment not implemented yet
  if(!require(zoo))
    stop('Can\'t load package zoo')
  if(!require(data.table))
    stop('Can\'t load package data.table')
  if(class(centroidDist)[1] != 'data.table')
    stop('centroidDist need to be of class data.table')
  if(sdShading & !require(matrixStats))
    stop('Can\'t load package matrixStats')
  if(sdShading & !require(ggplot2))
    stop('Can\'t load package ggplot2')
  if(time == 'min')
    framesPerTime <- hz*60
  else if(time == 'h')
    framesPerTime <- hz*60^2
  else
    stop('time needs to be \'min\' or \'h\'')
  
  #Average by treatment and sex
  if(!is.na(sex[1])){ #Assumes sex = logical vector. T == male
    if(!is.na(treatments[1])){
      treatments.uniq <- unique(treatments)
      nTreatments <- length(treatments.uniq)
      centroidDist.groupAvg <- as.data.frame(matrix(nrow = nrow(centroidDist), ncol = 2*nTreatments))
      
      #centroidDist.groupAvg will be organized as: 
      # - column 1 TO nTreatments = males in each treatment
      # - column nTreatments + 1 TO 2*nTreatments = females in each treatment
      for(i in 1:nTreatments){ #There should be a smarter way of doing this within the data.table indexing framework
        centroidDist.groupAvg[,i] <- rowMeans(centroidDist[, treatments == treatments.uniq[i] & sex, with = F]) #males
        centroidDist.groupAvg[,i + nTreatments] <- rowMeans(centroidDist[, treatments == treatments.uniq[i] & !sex, with = F]) #females
      }
      
      #Calculate SDs of the above means
      if(sdShading){
        centroidDist.groupSD <- as.data.frame(matrix(nrow = nrow(centroidDist), ncol = 2*nTreatments))
        
        for(i in 1:nTreatments){ #There should be a smarter way of doing this within the data.table indexing framework
          centroidDist.groupSD[,i] <- rowSds(as.matrix(centroidDist[, treatments == treatments.uniq[i] & sex, with = F])) #males
          centroidDist.groupSD[,i + nTreatments] <- rowSds(as.matrix(centroidDist[, treatments == treatments.uniq[i] & !sex, with = F])) #females
        }
      }
    }
    #else ...
  }
  
  if(!is.na(width) & !is.na(by)){ #Average sliding windows as given by 'width' and 'by'
    centroidDist.avg <- rollapply(data = centroidDist.groupAvg, width = width, FUN = mean, by.column = T, by = by, ...)
    
    #Create vector with time points matching the averaged windows
    tmp <- 1:nrow(centroidDist)
    t <- tmp[seq(1, nrow(centroidDist) - (width - 1), by)]/framesPerTime
  }
  else{ #No sliding window
    centroidDist.avg <- centroidDist.groupAvg
    rm(centroidDist.groupAvg)
    gc()
    
    #Time vector
    t <- (1:nrow(centroidDist))/framesPerTime
  }
  
  #Color for treatment
  if(!is.na(treatments[1])){
    cols.palette <- rainbow(length(unique(treatments)), start = 0, end = 4/6)
    cols <- c(cols.palette, cols.palette)
    
    if(is.na(treatmentLevels[1]))
      treatmentLevels <- treatments.uniq
    # treatmentLevels <- paste('group', unique(treatments))
  }
  else
    cols <- rep('black', ncol(centroidDist))
  
  #Line type for sex
  if(!is.na(sex[1])){ #Assumes sex = logical vector. T == male
    if(!is.na(treatments[1]))
      ltys <- c(rep(2, nTreatments), rep(1, nTreatments))
    #else ...
  }
  
  if(sdShading){
    #Modified from https://stackoverflow.com/questions/29743503/how-to-add-shaded-confidence-intervals-to-line-plot-with-specified-values/29747072
    p <- ggplot(data=data, aes(x=interval, y=OR, colour=Drug)) + geom_point() + geom_line()
    
  }
  else{
    plot(t, centroidDist.avg[,1], type = 'l', ylim = c(0, max(centroidDist.avg, na.rm = T)), ylab = '', xlab = '', col = cols[1], lty = ltys[1], lwd = 2)
    for(i in 2:ncol(centroidDist.avg)){
      points(t, centroidDist.avg[,i], type = 'l', ylim = c(0, max(centroidDist.avg, na.rm = T)), col = cols[i], lty = ltys[i], lwd = 2)
    }
    if(is.na(title))
      title(paste('Movement during', round(tail(t, 1), digits = 4), time), cex.main = 2)
    else
      title(title, cex.main = 2)
    
    mtext(paste('Time (', time, ')', sep = ''), side = 1, cex = 2, line = 3)
    mtext('Distance traveled per window', side = 2, cex = 2, line = 2)
    
    #Legends
    if(!is.na(treatmentLevels[1]))
      legend('topright', treatmentLevels, col = cols.palette, cex = 2, pch = 19)
    if(!is.na(sex[1]))
      legend('top', c('Male', 'Female'), lty = 2:1, cex = 2, lwd = 2)  
  }
}



plot.flyMv_cumMv <- function(centroidDist, sex = NA, treatments = NA, hz = 5, time = 'min', treatmentLevels = NA, title = NA){
  centroidDist.cumDist <- apply(centroidDist, MARGIN = 2, FUN = cumsum)
  
  if(time == 'min')
    t <- 1:nrow(centroidDist)/(hz*60)
  else if(time == 'h')
    t <- 1:nrow(centroidDist)/(hz*60^2)
  else
    stop('time needs to be \'min\' or \'h\'')
  
  #Color for treatment
  if(!is.na(treatments[1])){
    # if(is.logical(treatments)){ #Two treatments, indicated by a logival vector
    #   cols[treatments] <- 'red'
    #   
    #   if(is.na(treatmentLevels[1]))
    #     treatmentLevels <- c('Treatment', 'Control')
    # }
    # else if(is.numeric(treatments)){ #Many treatments, indicated by a numeric vector
    #   cols.palette <- rainbow(length(unique(treatments)))
    #   tmp <- as.factor(treatments)
    #   cols <- cols.palette[tmp]
    #   
    #   if(is.na(treatmentLevels[1]))
    #     treatmentLevels <- paste('group', levels(tmp))
    # }
    # else
    #   stop('treatments needs to be logical (two treatments) or numeric (multiple treatments)')
    
    cols.palette <- rainbow(length(unique(treatments)))
    tmp <- as.factor(treatments)
    cols <- cols.palette[tmp]
    
    if(is.na(treatmentLevels[1]))
      treatmentLevels <- paste('group', levels(tmp))
  }
  else
    cols <- rep('black', ncol(centroidDist))
  
  #Line type for sex
  ltys <- rep(1, ncol(centroidDist))
  if(!is.na(sex[1])) #Assumes sex = logical vector. T == male
    ltys[sex] <- 2
  
  plot(t, centroidDist.cumDist[,1], type = 'l', ylim = c(0, max(centroidDist.cumDist)), ylab = '', xlab = '', col = cols[1], lty = ltys[1])
  for(i in 2:ncol(centroidDist.cumDist)){
    points(t, centroidDist.cumDist[,i], type = 'l', ylim = c(0, max(centroidDist.cumDist)), col = cols[i], lty = ltys[i])
  }
  if(is.na(title))
    title(paste('Movement during', round(tail(t, 1), digits = 4), time), cex.main = 2)
  else
    title(title, cex.main = 2)
  
  mtext(paste('Time (', time, ')', sep = ''), side = 1, cex = 2, line = 3)
  mtext('Cumulative distance traveled', side = 2, cex = 2, line = 2)
  
  #Legends
  if(!is.na(treatmentLevels[1]))
    legend('topleft', treatmentLevels, col = cols.palette, cex = 2, pch = 19)
  if(!is.na(sex[1]))
    legend('top', c('Male', 'Female'), lty = 2:1, cex = 2)  
}



plot.flyMv_rollAvg <- function(centroidDist, sex = NA, treatments = NA, hz = 5, time = 'min', 
                               treatmentLevels = NA, title = NA, 
                               width = 5*60^2, by = 5*60*30, ...){
  if(!require(zoo))
    stop('Can\'t load package zoo')
  if(time == 'min')
    framesPerTime <- hz*60
  else if(time == 'h')
    framesPerTime <- hz*60^2
  else
    stop('time needs to be \'min\' or \'h\'')
  
  centroidDist.avg <- rollapply(data = centroidDist, width = width, FUN = mean, by.column = T, by = by, ...)
  
  #Create vector with time points matching the averaged windows
  tmp <- 1:nrow(centroidDist)
  t <- tmp[seq(1, nrow(centroidDist) - (width - 1), by)]/framesPerTime
  
  #Color for treatment
  if(!is.na(treatments[1])){
    cols.palette <- rainbow(length(unique(treatments)))
    tmp <- as.factor(treatments)
    cols <- cols.palette[tmp]
    
    if(is.na(treatmentLevels[1]))
      treatmentLevels <- paste('group', levels(tmp))
  }
  else
    cols <- rep('black', ncol(centroidDist))
  
  
  #Line type for sex
  ltys <- rep(1, ncol(centroidDist))
  if(!is.na(sex[1])) #Assumes sex = logical vector. T == male
    ltys[sex] <- 2
  
  plot(t, centroidDist.avg[,1], type = 'l', ylim = c(0, max(centroidDist.avg)), ylab = '', xlab = '', col = cols[1], lty = ltys[1])
  for(i in 2:ncol(centroidDist.avg)){
    points(t, centroidDist.avg[,i], type = 'l', ylim = c(0, max(centroidDist.avg)), col = cols[i], lty = ltys[i])
  }
  if(is.na(title))
    title(paste('Movement during', round(tail(t, 1), digits = 4), time), cex.main = 2)
  else
    title(title, cex.main = 2)
  
  mtext(paste('Time (', time, ')', sep = ''), side = 1, cex = 2, line = 3)
  mtext('Distance traveled per window', side = 2, cex = 2, line = 2)
  
  #Legends
  if(!is.na(treatmentLevels[1]))
    legend('bottomleft', treatmentLevels, col = cols.palette, cex = 2, pch = 19)
  if(!is.na(sex[1]))
    legend('bottom', c('Male', 'Female'), lty = 2:1, cex = 2)  
}



plot.flyMovement_plate <- function(centroidDist, nCols = 10, colRange = c("blue", "red")){
  nrWithMov <- apply(centroidDist, 2, FUN = function(x){sum(x != 0)})
  fracWithMov <- nrWithMov/nrow(centroidDist)
  print(fracWithMov)
  if(ncol(centroidDist) == 91){ #96 well plate
    x <- rep(1:12, 8)
    y <- sort(rep(1:8, 12), decreasing = T)
    pal = colorRampPalette(colRange)
    tmp <- as.numeric(cut(fracWithMov, breaks = seq(0, max(fracWithMov), length.out = nCols))) + 1
    print(tmp)
    tmp[is.na(tmp)] <- 1
    cols <- c('white', pal(nCols))
    plot(x, y, cex = 5.1, yaxt = 'n', xaxt = 'n', xlab = '', ylab = '')
    points(x, y, pch = 19, cex = 5, col = cols[tmp])
    axis(side = 1, 1:12)
    axis(side = 2, at = 1:8, labels = c('H', 'G', 'F', 'E', 'D', 'C', 'B', 'A'))
  }
  
}



plot.highlightBouts <- function(centroidDist = centroidDist,sleepActivity = flies.sleepActivity(centroidDist = as.data.frame(centroidDist), erroneousSleepDataThreshold = 0), flyNumber = 1, start = 1, end = 5, hz = 5, timeScale = "s", plots = "both"){
  #timeScale('h', 'm' or 's') gives timescale of plotting. Default is 's'
  #start is start time in units of timeScale(e.g. 5 with 'h' is start at hour 5). Default is 1
  #end is end time in units of timeScale as above. Default is 5
  #plots is the type of plot output("both", "sleep" or "movement"). Default is "both"
  #hz is hz used in data collection
  #flyNumber is the index of the fly you are interested in
  
  if(timeScale == "h"){
    plotFactor = hz*60^2
  }else if(timeScale == "m"){
    plotFactor = hz*60
  }else{
    plotFactor = hz
  }
  
  
  start <- start*plotFactor
  end <- end*plotFactor
  s <- 1:nrow(centroidDist)/plotFactor
  
  if(plots == "both"){
    
    plot(s[start:end], as.data.frame(centroidDist)[start:end, flyNumber], type = 'l', xlab = timeScale, ylab = 'Speed')
    sleepStart <- sleepActivity[[flyNumber]]$sleepStartTimes/(plotFactor)
    sleepEnd <- sleepActivity[[flyNumber]]$sleepStartTimes/(plotFactor) + sleepActivity[[flyNumber]]$sleepLengths/(plotFactor)
    rect(xleft = sleepStart, ybottom = 0, xright = sleepEnd, ytop = 35, col = rgb(120,220,220, maxColorValue = 255, alpha = 150))
    #Movement
    plot(s[start:end], centroidDist[[flyNumber]][start:end], type = 'l', xlab = timeScale, ylab = 'Speed')
    rect(xleft = sleepActivity[[flyNumber]]$mvStartTimes/plotFactor, ybottom = 0, xright = sleepActivity[[flyNumber]]$mvStartTimes/plotFactor + sleepActivity[[flyNumber]]$mvLengths/plotFactor, ytop = 35, col = rgb(220,220,220, maxColorValue = 255, alpha = 100))
    for(i in 1:length( sleepActivity[[flyNumber]]$mvStartTimes )){
      start <- sleepActivity[[flyNumber]]$mvStartTimes[i]/plotFactor
      end <- sleepActivity[[flyNumber]]$mvStartTimes[i]/plotFactor + sleepActivity[[flyNumber]]$mvLengths[i]/plotFactor
      
      
      avgSpeed <- sleepActivity[[flyNumber]]$boutSpeeds[i]
      lines(x = c(start, end), y = c(avgSpeed, avgSpeed), col = 'blue', lwd = 3)
    }
  }
  else if(plots == "movement"){
    plot(s[start:end], centroidDist[[flyNumber]][start:end], type = 'l', xlab = timeScale, ylab = 'Speed')
    rect(xleft = sleepActivity[[flyNumber]]$mvStartTimes/plotFactor, ybottom = 0, xright = sleepActivity[[flyNumber]]$mvStartTimes/plotFactor + sleepActivity[[flyNumber]]$mvLengths/plotFactor, ytop = 35, col = rgb(220,220,220, maxColorValue = 255, alpha = 100))
    for(i in 1:length( sleepActivity[[flyNumber]]$mvStartTimes )){
      start <- sleepActivity[[flyNumber]]$mvStartTimes[i]/plotFactor
      end <- sleepActivity[[flyNumber]]$mvStartTimes[i]/plotFactor + sleepActivity[[flyNumber]]$mvLengths[i]/plotFactor
      avgSpeed <- sleepActivity[[flyNumber]]$boutSpeeds[i]
      lines(x = c(start, end), y = c(avgSpeed, avgSpeed), col = 'blue', lwd = 3)
    }
  }
  else if(plots == "sleep"){
    plot(s[start:end], as.data.frame(centroidDist)[start:end, flyNumber], type = 'l', xlab = timeScale, ylab = 'Speed')
    sleepStart <- sleepActivity[[flyNumber]]$sleepStartTimes/(plotFactor)
    sleepEnd <- sleepActivity[[flyNumber]]$sleepStartTimes/(plotFactor) + sleepActivity[[flyNumber]]$sleepLengths/(plotFactor)
    rect(xleft = sleepStart, ybottom = 0, xright = sleepEnd, ytop = 35, col = rgb(120,220,220, maxColorValue = 255, alpha = 100))
  }
}



plot.flyMv_rollNoMov <- function(centroidDist, sex = NA, treatments = NA, hz = 5, time = 'min', 
                                 treatmentLevels = NA, title = NA, 
                                 width, by = 1, ...){
  if(!require(zoo))
    stop('Can\'t load package zoo')
  if(time == 'min')
    framesPerTime <- hz*60
  else if(time == 'h')
    framesPerTime <- hz*60^2
  else
    stop('time needs to be \'min\' or \'h\'')
  
  centroidDist.avg <- rollapply(data = centroidDist, width = width, FUN = function(x){sum(x == 0)/width}, by.column = T, by = by, ...)
  
  #Create vector with time points matching the averaged windows
  tmp <- 1:nrow(centroidDist)
  t <- tmp[seq(1, nrow(centroidDist) - (width - 1), by)]/framesPerTime
  
  #Color for treatment
  if(!is.na(treatments[1])){
    cols.palette <- rainbow(length(unique(treatments)))
    tmp <- as.factor(treatments)
    cols <- cols.palette[tmp]
    
    if(is.na(treatmentLevels[1]))
      treatmentLevels <- paste('group', levels(tmp))
  }
  else
    cols <- rep('black', ncol(centroidDist))
  
  #Line type for sex
  ltys <- rep(1, ncol(centroidDist))
  if(!is.na(sex[1])) #Assumes sex = logical vector. T == male
    ltys[sex] <- 2
  
  plot(t, centroidDist.avg[,1], type = 'l', ylim = c(0, max(centroidDist.avg)), ylab = '', xlab = '', col = cols[1], lty = ltys[1])
  for(i in 2:ncol(centroidDist.avg)){
    points(t, centroidDist.avg[,i], type = 'l', ylim = c(0, max(centroidDist.avg)), col = cols[i], lty = ltys[i])
  }
  if(is.na(title))
    title(paste('Sleep during', round(tail(t, 1), digits = 4), time), cex.main = 2)
  else
    title(title, cex.main = 2)
  
  mtext(paste('Time (', time, ')', sep = ''), side = 1, cex = 2, line = 3)
  mtext('% time without movement per window', side = 2, cex = 1.5, line = 2)
  
  #Legends
  if(!is.na(treatmentLevels[1]))
    legend('bottomleft', treatmentLevels, col = cols.palette, cex = 2, pch = 19)
  if(!is.na(sex[1]))
    legend('bottom', c('Male', 'Female'), lty = 2:1, cex = 2)
}



plot.flyMv_survival <- function(activity, treatments = NA, time = 'h', hz = 5, treatmentLevels = NA, experimentLength = NA){
  #TO DO: support for just one group
  if(time == 'min')
    framesPerTime <- hz*60
  else if(time == 'h')
    framesPerTime <- hz*60^2
  else
    stop('time needs to be \'min\' or \'h\'')
  
  activity.death <- sapply(activity, function(x){x$dead})/framesPerTime
  tmp <- as.factor(treatments)
  activity.death_treatments <- split(activity.death, tmp)
  
  #Create time x-axis
  if(is.na(experimentLength)) #x-axis extends until last death
    t <- seq(1, max(activity.death, na.rm = T), length.out = 100)
  else
    t <- seq(1, experimentLength/framesPerTime, length.out = 100)
  
  #Fraction alive
  # activity.death_treatment.surv <- sapply(t, function(x){sum(activity.death_treatment > x, na.rm = T)/length(activity.death_treatment)})
  # activity.death_control.surv <- sapply(t, function(x){sum(activity.death_control > x, na.rm = T)/length(activity.death_control)})
  survival <- list()
  for(i in 1:length(activity.death_treatments)){
    aliveThroughout <- sum(is.na(activity.death_treatments[[i]])) #NA == still alive at the end of the experiment
    survival[[i]] <- sapply(t, function(x){(sum(activity.death_treatments[[i]] > x, na.rm = T) + aliveThroughout)/length(activity.death_treatments[[i]])})
  }
  
  #Color for treatment
  if(!is.na(treatments[1])){
    cols.palette <- rainbow(length(activity.death_treatments), start = 0, end = 4/6)
    
    if(is.na(treatmentLevels[1]))
      treatmentLevels <- names(activity.death_treatments)
    # treatmentLevels <- paste('group', levels(tmp))
  }
  else
    cols <- rep('black', ncol(centroidDist))
  
  # plot(t, activity.death_control.surv, type = 'l', xlab = '', ylab = '', lwd = 3)
  # points(t, activity.death_treatment.surv, type = 'l', col = 'red', pch = 1.5, lwd = 3)
  plot(t, survival[[1]], type = 'l', xlab = '', ylab = '', lwd = 3, ylim = c(0,1), col = cols.palette[1])
  for(i in 2:length(survival))
    points(t, survival[[i]], type = 'l', pch = 1.5, lwd = 3, col = cols.palette[i])
  
  mtext('Fraction of flies alive', side = 2, cex = 2, line = 2)
  mtext(paste('Time (', time, ')', sep = ''), side = 1, cex = 2, line = 3)
  if(!is.na(treatmentLevels[1]))
    legend('bottomleft', treatmentLevels, col = cols.palette, cex = 2, pch = 19)
}



plot.flyMv_avgSleepLength <- function(activity, treatments = NA, treatmentLevels = NA, test = NA){
  # treatment.avgSleep <- sapply(activity[treatment], FUN = function(x){mean(x$sleepLengths)})/60
  # control.avgSleep <- sapply(activity[!treatment], FUN = function(x){mean(x$sleepLengths)})/60
  treatments <- as.factor(treatments)
  activity_treatments <- split(activity, treatments)
  avgSleep <- list()
  for(i in 1:length(activity_treatments))
    avgSleep[[i]] <- sapply(activity_treatments[[i]], FUN = function(x){mean(x$sleepLengths)})/(5*60)
  
  if(is.na(treatmentLevels[1]))
    treatmentLevels <- paste('group', levels(treatments))
  
  box <- boxplot(avgSleep, xaxt = 'n', frame = F)
  axis(side = 1, at = 1:nlevels(treatments), labels = treatmentLevels, cex.axis = 2)
  mtext(side = 2, text = 'Mean sleep length (min)', cex = 2, line = 2)
  
  if(nlevels(treatments) == 2 & !is.na(test)){
    if(test == 'wilcox'){
      p <- wilcox.test(avgSleep[[1]], avgSleep[[2]])$p.value
      # text(x = 1.5, y = mean(box$stats[5,]), labels = paste('p =', round(p, digits = 3)), cex = 2)
      legend('topleft', paste('p =', round(p, digits = 3)), title = 'Wilcoxon')
    }
    if(test == 't.test'){
      p <- t.test(avgSleep[[1]], avgSleep[[2]])$p.value
      # text(x = 1.5, y = mean(box$stats[5,]), labels = paste('p =', round(p, digits = 3)), cex = 2)
      legend('topleft', paste('p =', round(p, digits = 3)), title = 't-test')
    }
  }
  if(nlevels(treatments) > 2 & !is.na(test)){
    model <- lm(unlist(avgSleep) ~ treatments)
    p <- lmp(model)
    # text(x = 1.5, y = mean(box$stats[5,]), labels = paste('p =', round(p, digits = 3)), cex = 2)
    legend('topleft', paste('p =', round(p, digits = 3)), title = 'Anova')
  }
}



plot.flyMv_sleepNr <- function(activity, treatments = NA, treatmentLevels = NA, test = 'wilcox'){
  treatments <- as.factor(treatments)
  activity_treatments <- split(activity, treatments)
  avgSleep <- list()
  for(i in 1:length(activity_treatments))
    avgSleep[[i]] <- sapply(activity_treatments[[i]], FUN = function(x){mean(x$sleepNr)})
  
  if(is.na(treatmentLevels[1]))
    treatmentLevels <- paste('group', levels(treatments))
  
  box <- boxplot(avgSleep, xaxt = 'n', frame = F)
  axis(side = 1, at = 1:nlevels(treatments), labels = treatmentLevels, cex.axis = 2)
  mtext(side = 2, text = 'Number of sleep bouts', cex = 2, line = 2)
  
  if(nlevels(treatments) == 2 & !is.na(test)){
    if(test == 'wilcox'){
      p <- wilcox.test(avgSleep[[1]], avgSleep[[2]])$p.value
      # text(x = 1.5, y = mean(box$stats[5,]), labels = paste('p =', round(p, digits = 3)), cex = 2)
      legend('topleft', paste('p =', round(p, digits = 3)), title = 'Wilcoxon')
    }
    if(test == 't.test'){
      p <- t.test(avgSleep[[1]], avgSleep[[2]])$p.value
      # text(x = 1.5, y = mean(box$stats[5,]), labels = paste('p =', round(p, digits = 3)), cex = 2)
      legend('topleft', paste('p =', round(p, digits = 3)), title = 't-test')
    }
  }
  if(nlevels(treatments) > 2 & !is.na(test)){
    model <- lm(unlist(avgSleep) ~ treatments)
    p <- lmp(model)
    # text(x = 1.5, y = mean(box$stats[5,]), labels = paste('p =', round(p, digits = 3)), cex = 2)
    legend('topleft', paste('p =', round(p, digits = 3)), title = 'Anova')
  }
}



plot.flyMv_avgMvLength <- function(activity, treatments = NA, treatmentLevels = NA, test = 'wilcox'){
  treatments <- as.factor(treatments)
  activity_treatments <- split(activity, treatments)
  avgSleep <- list()
  for(i in 1:length(activity_treatments))
    avgSleep[[i]] <- sapply(activity_treatments[[i]], FUN = function(x){mean(x$mvLengths)})/5
  
  if(is.na(treatmentLevels[1]))
    treatmentLevels <- paste('group', levels(treatments))
  
  box <- boxplot(avgSleep, xaxt = 'n', frame = F)
  axis(side = 1, at = 1:nlevels(treatments), labels = treatmentLevels, cex.axis = 2)
  mtext(side = 2, text = 'Mean length mv bout (s)', cex = 2, line = 2)
  
  if(nlevels(treatments) == 2 & !is.na(test)){
    if(test == 'wilcox'){
      p <- wilcox.test(avgSleep[[1]], avgSleep[[2]])$p.value
      # text(x = 1.5, y = mean(box$stats[5,]), labels = paste('p =', round(p, digits = 3)), cex = 2)
      legend('topleft', paste('p =', round(p, digits = 3)), title = 'Wilcoxon')
    }
    if(test == 't.test'){
      p <- t.test(avgSleep[[1]], avgSleep[[2]])$p.value
      # text(x = 1.5, y = mean(box$stats[5,]), labels = paste('p =', round(p, digits = 3)), cex = 2)
      legend('topleft', paste('p =', round(p, digits = 3)), title = 't-test')
    }
  }
  if(nlevels(treatments) > 2 & !is.na(test)){
    model <- lm(unlist(avgSleep) ~ treatments)
    p <- lmp(model)
    # text(x = 1.5, y = mean(box$stats[5,]), labels = paste('p =', round(p, digits = 3)), cex = 2)
    legend('topleft', paste('p =', round(p, digits = 3)), title = 'Anova')
  }
}