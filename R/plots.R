#Plots function
#plot.flyMv_allFigs - All figures
#plot.flyMv_cumMv - Requires work to identify sexes
#plot.flyMv_rollAvg - Working
#plot.flyMovement_plate
#plot.highlightBouts
#plot.flyMv_rollNoMov
#plot.flyMv_avgSleepLength
#plot.flyMv_sleepNr
#plot.flyMv_avgMvLength
#plot.gXeBoxes
#
#
#

plot.flyMv_mvNr <-
  function(trak,
           treatmentLevels = NA,
           test = 'wilcox',
           ...) {
    #Naming
    activity <- trak@activity$result
    treatments = trak@metadata$Treatment
    
    treatments <- as.factor(treatments)
    activity_treatments <- split(activity, treatments)
    avgSleep <- list()
    for (i in 1:length(activity_treatments))
      avgSleep[[i]] <-
      sapply(
        activity_treatments[[i]],
        FUN = function(x) {
          mean(x$mvNr)
        }
      )
    if (!all(is.na(avgSleep[[1]]))) {
    if (is.na(treatmentLevels[1]))
      treatmentLevels <- paste('group', levels(treatments))
    
      box <-
        boxplot(
          avgSleep,
          xaxt = 'n',
          frame = F,
          ylim = range(avgSleep, na.rm = TRUE)
        )
      axis(
        side = 1,
        at = 1:nlevels(treatments),
        labels = treatmentLevels,
        cex.axis = 2
      )
      mtext(
        side = 2,
        text = 'Number of movement bouts',
        cex = 2,
        line = 2
      )
      
      if (nlevels(treatments) == 2 & !is.na(test)) {
        if (test == 'wilcox') {
          p <- wilcox.test(avgSleep[[1]], avgSleep[[2]])$p.value
          # text(x = 1.5, y = mean(box$stats[5,]), labels = paste('p =', round(p, digits = 3)), cex = 2)
          legend('topleft', paste('p =', round(p, digits = 3)), title = 'Wilcoxon')
        }
        if (test == 't.test') {
          p <- t.test(avgSleep[[1]], avgSleep[[2]])$p.value
          # text(x = 1.5, y = mean(box$stats[5,]), labels = paste('p =', round(p, digits = 3)), cex = 2)
          legend('topleft', paste('p =', round(p, digits = 3)), title = 't-test')
        }
      }
      if (nlevels(treatments) > 2 & !is.na(test)) {
        model <- lm(unlist(avgSleep) ~ as.factor(treatments))
        p <- lmp(model)
        # text(x = 1.5, y = mean(box$stats[5,]), labels = paste('p =', round(p, digits = 3)), cex = 2)
        legend('topleft', paste('p =', round(p, digits = 3)), title = 'Anova')
      }
    }
    else{
      print("No sleep bouts available")
    }
  }

#Converting to trak obj
plot.flyMv_allFigs <- function(trak,
                               fileBaseName,
                               time = 'min',
                               treatmentLevels = NA,
                               width = 5 * 60 ^ 2,
                               by = 5 * 60 * 30,
                               avgMv = T,
                               noMv = F) {
  #Wrapper function that produces various figures of fly activity
  
  #Cummulative movement
  file <- paste(fileBaseName, '_cumMv.png', sep = '')
  png(filename = file,
      width = 1200,
      height = 800)
  plot.flyMv_cumMv(trak,time=time, treatmentLevels = treatmentLevels)
  dev.off()
  
  #Survival
  file <- paste(fileBaseName, '_survival.png', sep = '')
  png(filename = file,
      width = 1200,
      height = 800)
  plot.flyMv_survival(trak, treatmentLevels = treatmentLevels)
  dev.off()
  
  #Movement and sleep comparisons across whole experiment
  file <- paste(fileBaseName, '_sleepAndMvBouts.png', sep = '')
  png(filename = file,
      width = 1200,
      height = 800)
  par(mfrow = c(2, 2))
  plot.flyMv_avgSleepLength(trak, treatmentLevels = treatmentLevels)
  plot.flyMv_sleepNr(trak, treatmentLevels = treatmentLevels)
  
  plot.flyMv_avgMvLength(trak, treatmentLevels = treatmentLevels)
  plot.flyMv_mvNr(trak, treatmentLevels = treatmentLevels)
  dev.off()
  
  #Average movement per window
  if (avgMv) {
    file <- paste(fileBaseName, '_avgMvPerWindow.png', sep = '')
    png(filename = file,
        width = 1200,
        height = 800)
    plot.flyMv_rollAvg(
      trak,
      time = time,
      treatmentLevels = treatmentLevels,
      width = width,
      by = by
    )
    dev.off()
  }
  
  #Fraction no movement per window
  if (noMv) {
    file <- paste(fileBaseName, '_fracNoMvPerWindow.png', sep = '')
    png(filename = file,
        width = 1200,
        height = 800)
    plot.flyMv_rollNoMov(
      trak,
      time = time,
      treatmentLevels = treatmentLevels,
      width = width,
      by = by
    )
    dev.off()
  }
}


#Note: Data points are too dense for lty's to do anything
plot.flyMv_cumMv <-
  function(trak,
           time = 'min',
           treatmentLevels = NA,
           title = NA,
           start = NA,
           end = nrow(speed),
           sampling = 100) {
    #Naming to clean up later code implementation
    speed <- trak@speed
    hz <- trak@hz
    treatments <- trak@metadata$Treatment
    sex <- trak@metadata$Male
    
    if (time == 'min') {
      timeFactor <- 60
      if (end != nrow(speed)) {
        end <- end * hz * timeFactor
      }
      if (!is.na(start)) {
        start <- start * hz * timeFactor
      }
      else{
        start <- 1
      }
    }
    else if (time == 'h') {
      timeFactor <- 60 ^ 2
      if (end != nrow(speed)) {
        end <- end * hz * timeFactor
      }
      if (!is.na(start)) {
        start <- start * hz * timeFactor
      } else{
        start <- 1
      }
    }
    else{
      stop('time needs to be \'min\' or \'h\'')
    }
    speed.cumDist <-
      apply(speed[start:end, ], MARGIN = 2, FUN = cumsum)
    sample <-
      seq.int(from = 1,
              to = nrow(speed.cumDist),
              length.out = 100)
    t <-
      seq(from = (start / (hz * timeFactor)),
          to = (end / (hz * timeFactor)),
          length.out = 100)
    #Color for treatment
    if (!is.na(treatments[1])) {
      cols.palette <- rainbow(length(unique(treatments)))
      #Drop levels to keep legend clean
      tmp <- droplevels(as.factor(treatments))
      cols <- cols.palette[tmp]
      
      if (is.na(treatmentLevels[1]))
        treatmentLevels <- paste('group', levels(tmp))
    }
    else
      cols <- rep('black', ncol(speed))
    
    #Line type for sex
    ltys <-
      rep(2, ncol(speed)) #Lty's don't work because of density on big sets
    if (!is.na(sex[1]))
      #Assumes sex = logical vector. T == male
      ltys[sex] <- 1 #We also use this to define
    
    plot(
      t,
      speed.cumDist[sample, 1],
      type = 'l',
      ylim = c(0, max(speed.cumDist)),
      ylab = '',
      xlab = '',
      col = cols[1],
      lty = ltys[1],
      lwd = ltys[1]
    )
    for (i in 2:ncol(speed.cumDist)) {
      points(
        t,
        speed.cumDist[sample, i],
        type = 'l',
        ylim = c(0, max(speed.cumDist)),
        col = cols[i],
        lty = ltys[i],
        lwd = ltys[i]
      )
    }
    if (is.na(title))
      title(paste('Movement during', round((
        tail(t, 1) - head(t, 1)
      ), digits = 4), time), cex.main = 2)
    else
      title(title, cex.main = 2)
    
    mtext(
      paste('Time (', time, ')', sep = ''),
      side = 1,
      cex = 2,
      line = 3
    )
    mtext(
      'Cumulative distance traveled',
      side = 2,
      cex = 2,
      line = 2
    )
    
    #Legends
    if (!is.na(treatmentLevels[1]))
      legend(
        'topleft',
        treatmentLevels,
        col = cols.palette,
        cex = 2,
        pch = 19
      )
    if (!is.na(sex[1]))
      legend('top', c('Male', 'Female'), lty = 1:2, cex = 2)
  }




plot.flyMv_rollAvg <- function(trak,
                               time = 'min',
                               treatmentLevels = NA,
                               title = NA,
                               width = 5 * 60 ^ 2,
                               by = 5 * 60 * 10,
                               legend.treat = 'topright',
                               legend.sex = 'top',
                               legend.treat_title = '',
                               returnAvgSpeed = F,
                               ...) {
  #Naming to clean up code
  speed <- trak@speed
  hz <- trak@hz
  treatments <- trak@metadata$Treatment
  sex <- trak@metadata$Male
  framesPerTime = 0
  if (time == 'min') {
    framesPerTime <- hz * 60
  }
  else if (time == 'h') {
    framesPerTime <- hz * 60 ^ 2
  }
  else{
    stop('time needs to be \'min\' or \'h\'')
  }
  speed.avg <-
    rollapply(
      data = as.data.frame(speed),
      width = width,
      FUN = mean,
      by.column = T,
      by = by
    )
  
  #Create vector with time points matching the averaged windows
  tmp <- 1:nrow(speed)
  t <- tmp[seq(1, nrow(speed) - (width - 1), by)] / framesPerTime
  
  #Color for treatment
  if (!is.na(treatments[1])) {
    # cols.palette <- rainbow(length(unique(treatments)))
    cols.palette <-
      wes_palette(name = 'Cavalcanti1',
                  n = length(unique(treatments)),
                  type = 'continuous')
    tmp <- as.factor(treatments)
    cols <- cols.palette[tmp]
    
    if (is.na(treatmentLevels[1]))
      treatmentLevels <- paste('group', levels(tmp))
  }
  else{
    cols <- rep('black', ncol(speed))
  }
  
  #Line type for sex
  ltys <- rep(1, ncol(speed))
  if (!is.na(sex[1])) {
    #Assumes sex = logical vector. T == male
    ltys[sex] <- 2
  }
  
  plot(
    t,
    speed.avg[, 1],
    type = 'l',
    ylim = c(0, max(speed.avg)),
    ylab = '',
    xlab = '',
    col = cols[1],
    lty = ltys[1],
    ...
  )
  for (i in 2:ncol(speed.avg)) {
    points(
      t,
      speed.avg[, i],
      type = 'l',
      ylim = c(0, max(speed.avg)),
      col = cols[i],
      lty = ltys[i],
      ...
    )
  }
  if (is.na(title)) {
    title(paste('Speed during', round(tail(t, 1), digits = 4), time), cex.main = 2)
  }
  else{
    title(title, cex.main = 2)
  }
  
  mtext(
    paste('Time (', time, ')', sep = ''),
    side = 1,
    cex = 2,
    line = 3
  )
  mtext(
    'Average speed per window',
    side = 2,
    cex = 2,
    line = 2
  )
  
  #Legends
  if (!is.na(treatmentLevels[1]) & legend.treat != 'n')
    legend(
      legend.treat,
      treatmentLevels,
      col = cols.palette,
      cex = 1.5,
      pch = 19,
      title = legend.treat_title
    )
  if (!is.na(sex[1]) & legend.sex != 'n')
    legend(legend.sex,
           c('Male', 'Female'),
           lty = 2:1,
           cex = 1.5)
  
  if (returnAvgSpeed) {
    return(data.frame(speed.avg, time = t))
  }
}

#Experimental function
plot.flyMovement_plate <-
  function(speed,
           nRows = 4,
           nCols = 6,
           colRange = c("blue", "red")) {
    nrWithMov <- apply(
      speed,
      2,
      FUN = function(x) {
        sum(x != 0)
      }
    )
    fracWithMov <- nrWithMov / nrow(speed)
    if (ncol(speed) == nRows * nCols) {
      #Matching size
      x <- rep(1:nCols, nRows)
      y <- sort(rep(1:nRows, nCols), decreasing = T)
      pal = colorRampPalette(colRange)
      tmp <-
        as.numeric(cut(fracWithMov, breaks = seq(0, max(fracWithMov), length.out = nCols))) + 1
      tmp[is.na(tmp)] <- 1
      cols <- c('white', pal(nCols))
      par(mai = c(1, .5, 1, 1))
      #Ad hoc cex scaling
      scalar <- abs(x - y)
      plot(
        x,
        y,
        cex = 7 / (1 + .07 * scalar),
        yaxt = 'n',
        xaxt = 'n',
        xlab = '',
        ylab = '',
        xlim = c(.5, (nCols + .5)),
        ylim = c(.75, (nRows + .25))
      )
      title(main = "Per Well Movement")
      
      points(x,
             y,
             pch = 19,
             cex = 7 / (1 + .07 * scalar),
             col = cols[tmp])
      axis(side = 1, 1:nCols)
      possibleLabels = c('M', 'L', 'K', 'J', 'I', 'H', 'G', 'F', 'E', 'D', 'C', 'B', 'A')
      sortedLabels = tail(possibleLabels, n = nRows)
      axis(side = 2,
           at = 1:nRows,
           labels = sortedLabels)
      ColorLegend(
        x = (nCols + 1),
        y = nRows,
        height = (nRows - 1),
        width = .5,
        labels = c("Min", "Max"),
        col = pal(100)
      )
    }
    else{
      stop("Rows multiplied by columns is not equal to the number of columns in speed.")
    }
    
  }



plot.highlightBouts <-
  function(trak,
           flyNumber = 1,
           start = 1,
           end = 5,
           timeScale = "s",
           plots = "both",
           returnData = F,
           ...) {
    #timeScale('h', 'm' or 's') gives timescale of plotting. Default is 's'
    #start is start time in units of timeScale(e.g. 5 with 'h' is start at hour 5). Default is 1
    #end is end time in units of timeScale as above. Default is 5
    #plots is the type of plot output("both", "sleep" or "movement"). Default is "both"
    #hz is hz used in data collection
    #flyNumber is the index of the fly you are interested in
    
    speed <- trak@speed
    sleepActivity <- trak@activity$result
    hz = trak@hz
    
    if (timeScale %in% c('h', 'hour')) {
      plotFactor = hz * 60 ^ 2
    } else if (timeScale %in% c('m', 'min', 'minute')) {
      plotFactor = hz * 60
    } else if (timeScale == 'frames') {
      plotFactor = 1
    } else if (timeScale %in% c('s', 'sec')) {
      plotFactor = hz
    }
    else
      stop(paste('Did not recognize timeScale:', timeScale))
    
    start <- start * plotFactor
    end <- end * plotFactor
    s <- 1:nrow(speed) / plotFactor
    ymax <-
      max(as.data.frame(speed)[start:end, flyNumber], na.rm = T)
    if (returnData)
      out <-
      data.frame(time = s[start:end], speed = as.data.frame(speed)[start:end, flyNumber])
    
    #Additional arguments?
    arg <- list(...)
    
    if (plots == "both") {
      if (any(c('xlab', 'ylab') %in% names(arg)))
        plot(s[start:end], as.data.frame(speed)[start:end, flyNumber], type = 'l', ...)
      else
        plot(
          s[start:end],
          as.data.frame(speed)[start:end, flyNumber],
          type = 'l',
          xlab = timeScale,
          ylab = 'Speed',
          ...
        )
      sleepStart <-
        sleepActivity[[flyNumber]]$sleepStartTimes / (plotFactor)
      sleepEnd <-
        sleepActivity[[flyNumber]]$sleepStartTimes / (plotFactor) + sleepActivity[[flyNumber]]$sleepLengths /
        (plotFactor)
      rect(
        xleft = sleepStart,
        ybottom = 0,
        xright = sleepEnd,
        ytop = ymax,
        col = rgb(120, 220, 220, maxColorValue = 255, alpha = 150)
      )
      #Movement
      rect(
        xleft = sleepActivity[[flyNumber]]$mvBouts.startTimes / plotFactor,
        ybottom = 0,
        xright = sleepActivity[[flyNumber]]$mvBouts.startTimes / plotFactor + sleepActivity[[flyNumber]]$mvBouts.lengths /
          plotFactor,
        ytop = ymax,
        col = rgb(220, 220, 220, maxColorValue = 255, alpha = 100)
      )
      for (i in 1:length(sleepActivity[[flyNumber]]$mvBouts.startTimes)) {
        start <-
          sleepActivity[[flyNumber]]$mvBouts.startTimes[i] / plotFactor
        end <-
          sleepActivity[[flyNumber]]$mvBouts.startTimes[i] / plotFactor + sleepActivity[[flyNumber]]$mvBouts.lengths[i] /
          plotFactor
        
        
        avgSpeed <- sleepActivity[[flyNumber]]$mvBouts.avgSpeed[i]
        lines(
          x = c(start, end),
          y = c(avgSpeed, avgSpeed),
          col = 'blue',
          lwd = 3
        )
      }
    }
    else if (plots == "movement") {
      if (any(c('xlab', 'ylab') %in% names(arg)))
        plot(s[start:end], speed[[flyNumber]][start:end], type = 'l', ...)
      else
        plot(s[start:end],
             speed[[flyNumber]][start:end],
             type = 'l',
             xlab = timeScale,
             ylab = 'Speed',
             ...)
      
      rect(
        xleft = sleepActivity[[flyNumber]]$mvBouts.startTimes / plotFactor,
        ybottom = 0,
        xright = sleepActivity[[flyNumber]]$mvBouts.startTimes / plotFactor + sleepActivity[[flyNumber]]$mvBouts.lengths /
          plotFactor,
        ytop = ymax,
        col = rgb(220, 220, 220, maxColorValue = 255, alpha = 100)
      )
      for (i in 1:length(sleepActivity[[flyNumber]]$mvBouts.startTimes)) {
        start <-
          sleepActivity[[flyNumber]]$mvBouts.startTimes[i] / plotFactor
        end <-
          sleepActivity[[flyNumber]]$mvBouts.startTimes[i] / plotFactor + sleepActivity[[flyNumber]]$mvBouts.lengths[i] /
          plotFactor
        avgSpeed <- sleepActivity[[flyNumber]]$mvBouts.avgSpeed[i]
        lines(
          x = c(start, end),
          y = c(avgSpeed, avgSpeed),
          col = 'blue',
          lwd = 3
        )
      }
    }
    else if (plots == "sleep") {
      if (any(c('xlab', 'ylab') %in% names(arg)))
        plot(s[start:end], as.data.frame(speed)[start:end, flyNumber], type = 'l', ...)
      else
        plot(
          s[start:end],
          as.data.frame(speed)[start:end, flyNumber],
          type = 'l',
          xlab = timeScale,
          ylab = 'Speed',
          ...
        )
      sleepStart <-
        sleepActivity[[flyNumber]]$sleepStartTimes / (plotFactor)
      sleepEnd <-
        sleepActivity[[flyNumber]]$sleepStartTimes / (plotFactor) + sleepActivity[[flyNumber]]$sleepLengths /
        (plotFactor)
      rect(
        xleft = sleepStart,
        ybottom = 0,
        xright = sleepEnd,
        ytop = ymax,
        col = rgb(120, 220, 220, maxColorValue = 255, alpha = 100)
      )
    }
    if (returnData)
      return(out)
  }



plot.flyMv_rollNoMov <- function(trak,
                                 time = 'min',
                                 treatmentLevels = NA,
                                 title = NA,
                                 width,
                                 by = 1,
                                 ...) {
  speed <- trak@speed
  treatments <- trak@metadata$Treatment
  sex <- trak@metadata$Male
  hz <- trak@hz
  
  if (!require(zoo))
    stop('Can\'t load package zoo')
  if (time == 'min')
    framesPerTime <- hz * 60
  else if (time == 'h')
    framesPerTime <- hz * 60 ^ 2
  else
    stop('time needs to be \'min\' or \'h\'')
  
  speed.avg <-
    rollapply(
      data = speed,
      width = width,
      FUN = function(x) {
        sum(x == 0) / width
      },
      by.column = T,
      by = by,
      ...
    )
  
  #Create vector with time points matching the averaged windows
  tmp <- 1:nrow(speed)
  t <- tmp[seq(1, nrow(speed) - (width - 1), by)] / framesPerTime
  
  #Color for treatment
  if (!is.na(treatments[1])) {
    cols.palette <- rainbow(length(unique(treatments)))
    tmp <- as.factor(treatments)
    cols <- cols.palette[tmp]
    
    if (is.na(treatmentLevels[1]))
      treatmentLevels <- paste('group', levels(tmp))
  }
  else
    cols <- rep('black', ncol(speed))
  
  #Line type for sex
  ltys <- rep(1, ncol(speed))
  if (!is.na(sex[1]))
    #Assumes sex = logical vector. T == male
    ltys[sex] <- 2
  
  plot(
    t,
    speed.avg[, 1],
    type = 'l',
    ylim = c(0, max(speed.avg)),
    ylab = '',
    xlab = '',
    col = cols[1],
    lty = ltys[1]
  )
  for (i in 2:ncol(speed.avg)) {
    points(
      t,
      speed.avg[, i],
      type = 'l',
      ylim = c(0, max(speed.avg)),
      col = cols[i],
      lty = ltys[i]
    )
  }
  if (is.na(title))
    title(paste('Sleep during', round(tail(t, 1), digits = 4), time), cex.main = 2)
  else
    title(title, cex.main = 2)
  
  mtext(
    paste('Time (', time, ')', sep = ''),
    side = 1,
    cex = 2,
    line = 3
  )
  mtext(
    '% time without movement per window',
    side = 2,
    cex = 1.5,
    line = 2
  )
  
  #Legends
  if (!is.na(treatmentLevels[1]))
    legend(
      'bottomleft',
      treatmentLevels,
      col = cols.palette,
      cex = 2,
      pch = 19
    )
  if (!is.na(sex[1]))
    legend('bottom',
           c('Male', 'Female'),
           lty = 2:1,
           cex = 2)
}



plot.flyMv_survival <-
  function(trak,
           treatments = NA,
           time = 'h',
           treatmentLevels = NA,
           experimentLength = NA,
           legend.title = '',
           palette = 'wes') {
    #Naming to clean up code
    activity <- trak@activity$result
    hz <- trak@hz
    treatments <- trak@metadata$Treatment
    
    #TO DO: support for just one group
    if (time == 'min')
      framesPerTime <- hz * 60
    else if (time == 'h')
      framesPerTime <- hz * 60 ^ 2
    else
      stop('time needs to be \'min\' or \'h\'')
    
    activity.death <-
      sapply(activity, function(x) {
        x$dead
      }) / framesPerTime
    tmp <- as.factor(treatments)
    activity.death_treatments <- split(activity.death, tmp)
    if (max(activity.death, na.rm = T) != -Inf) {
      #Create time x-axis
      if (is.na(experimentLength))
        #x-axis extends until last death
        t <-
          seq(1, max(activity.death, na.rm = T), length.out = 100)
      else
        t <-
          seq(1, nrows(activity) / framesPerTime, length.out = 100)
      
      #Fraction alive
      # activity.death_treatment.surv <- sapply(t, function(x){sum(activity.death_treatment > x, na.rm = T)/length(activity.death_treatment)})
      # activity.death_control.surv <- sapply(t, function(x){sum(activity.death_control > x, na.rm = T)/length(activity.death_control)})
      survival <- list()
      for (i in 1:length(activity.death_treatments)) {
        aliveThroughout <-
          sum(is.na(activity.death_treatments[[i]])) #NA == still alive at the end of the experiment
        survival[[i]] <-
          sapply(t, function(x) {
            (sum(activity.death_treatments[[i]] > x, na.rm = T) + aliveThroughout) /
              length(activity.death_treatments[[i]])
          })
      }
      #Color for treatment
      if (!is.na(treatments[1])) {
        if (palette == 'wes')
          cols.palette <-
            wes_palette(name = 'Zissou1',
                        length(activity.death_treatments),
                        type = 'continuous')
        else
          cols.palette <-
            rainbow(length(activity.death_treatments),
                    start = 0,
                    end = 4 / 6)
        
        if (is.na(treatmentLevels[1]))
          treatmentLevels <- names(activity.death_treatments)
        # treatmentLevels <- paste('group', levels(tmp))
      }
      else
        cols <- rep('black', ncol(speed))
      
      # plot(t, activity.death_control.surv, type = 'l', xlab = '', ylab = '', lwd = 3)
      # points(t, activity.death_treatment.surv, type = 'l', col = 'red', pch = 1.5, lwd = 3)
      plot(
        t,
        survival[[1]],
        type = 'l',
        xlab = '',
        ylab = '',
        lwd = 3,
        ylim = c(0, 1),
        col = cols.palette[1]
      )
      for (i in 2:length(survival))
        points(
          t,
          survival[[i]],
          type = 'l',
          pch = 1.5,
          lwd = 3,
          col = cols.palette[i]
        )
      
      mtext(
        'Fraction of flies alive',
        side = 2,
        cex = 2,
        line = 2
      )
      mtext(
        paste('Time (', time, ')', sep = ''),
        side = 1,
        cex = 2,
        line = 3
      )
      if (!is.na(treatmentLevels[1]))
        legend(
          'bottomleft',
          treatmentLevels,
          col = cols.palette,
          cex = 1.5,
          lty = 1,
          lwd = 3,
          title = legend.title
        )
    }
    else{
      print("No deaths found.")
    }
  }



plot.flyMv_avgSleepLength <-
  function(trak,
           treatmentLevels = NA,
           test = 'wilcox',
           ...) {
    #Naming to clean up code
    activity <- trak@activity$result
    treatments = trak@metadata$Treatment
    hz <- trak@hz
    
    # treatment.avgSleep <- sapply(activity[treatment], FUN = function(x){mean(x$sleepLengths)})/60
    # control.avgSleep <- sapply(activity[!treatment], FUN = function(x){mean(x$sleepLengths)})/60
    treatments <- as.factor(treatments)
    activity_treatments <- split(activity, treatments)
    avgSleep <- list()
    for (i in 1:length(activity_treatments))
      avgSleep[[i]] <-
      sapply(
        activity_treatments[[i]],
        FUN = function(x) {
          mean(x$sleepLengths, na.rm = T)
        }
      ) / (hz * 60)
    
    if (is.na(treatmentLevels[1]))
      treatmentLevels <- paste('group', levels(treatments))
    
    box <- boxplot(avgSleep, xaxt = 'n', frame = F, ...)
    axis(
      side = 1,
      at = 1:nlevels(treatments),
      labels = treatmentLevels,
      cex.axis = 2
    )
    mtext(
      side = 2,
      text = 'Mean sleep length (min)',
      cex = 2,
      line = 2
    )
    
    if (nlevels(treatments) == 2 & !is.na(test)) {
      if (test == 'wilcox') {
        p <- wilcox.test(avgSleep[[1]], avgSleep[[2]])$p.value
        # text(x = 1.5, y = mean(box$stats[5,]), labels = paste('p =', round(p, digits = 3)), cex = 2)
        legend('topleft', paste('p =', round(p, digits = 3)), title = 'Wilcoxon')
      }
      if (test == 't.test') {
        p <- t.test(avgSleep[[1]], avgSleep[[2]])$p.value
        # text(x = 1.5, y = mean(box$stats[5,]), labels = paste('p =', round(p, digits = 3)), cex = 2)
        legend('topleft', paste('p =', round(p, digits = 3)), title = 't-test')
      }
    }
    if (nlevels(treatments) > 2 & !is.na(test)) {
      model <- lm(unlist(avgSleep) ~ as.factor(treatments))
      p <- lmp(model)
      # text(x = 1.5, y = mean(box$stats[5,]), labels = paste('p =', round(p, digits = 3)), cex = 2)
      legend('topleft', paste('p =', round(p, digits = 3)), title = 'Anova')
    }
  }



plot.flyMv_sleepNr <-
  function(trak,
           treatmentLevels = NA,
           test = 'wilcox') {
    #Naming to clean up code
    activity <- trak@activity$result
    treatments = trak@metadata$Treatment
    
    treatments <- as.factor(treatments)
    activity_treatments <- split(activity, treatments)
    avgSleep <- list()
    for (i in 1:length(activity_treatments))
      avgSleep[[i]] <-
      sapply(
        activity_treatments[[i]],
        FUN = function(x) {
          mean(x$sleepNr)
        }
      )
    
    if (is.na(treatmentLevels[1]))
      treatmentLevels <- paste('group', levels(treatments))
    
    box <- boxplot(avgSleep, xaxt = 'n', frame = F)
    axis(
      side = 1,
      at = 1:nlevels(treatments),
      labels = treatmentLevels,
      cex.axis = 2
    )
    mtext(
      side = 2,
      text = 'Number of sleep bouts',
      cex = 2,
      line = 2
    )
    
    if (nlevels(treatments) == 2 & !is.na(test)) {
      if (test == 'wilcox') {
        p <- wilcox.test(avgSleep[[1]], avgSleep[[2]])$p.value
        # text(x = 1.5, y = mean(box$stats[5,]), labels = paste('p =', round(p, digits = 3)), cex = 2)
        legend('topleft', paste('p =', round(p, digits = 3)), title = 'Wilcoxon')
      }
      if (test == 't.test') {
        p <- t.test(avgSleep[[1]], avgSleep[[2]])$p.value
        # text(x = 1.5, y = mean(box$stats[5,]), labels = paste('p =', round(p, digits = 3)), cex = 2)
        legend('topleft', paste('p =', round(p, digits = 3)), title = 't-test')
      }
    }
    if (nlevels(treatments) > 2 & !is.na(test)) {
      model <- lm(unlist(avgSleep) ~ as.factor(treatments))
      p <- lmp(model)
      # text(x = 1.5, y = mean(box$stats[5,]), labels = paste('p =', round(p, digits = 3)), cex = 2)
      legend('topleft', paste('p =', round(p, digits = 3)), title = 'Anova')
    }
  }



plot.flyMv_avgMvLength <-
  function(trak,
           treatmentLevels = NA,
           test = 'wilcox') {
    #Naming
    activity <- trak@activity$result
    treatments = trak@metadata$Treatment
    hz <- trak@hz
    
    treatments <- as.factor(treatments)
    activity_treatments <-
      split(activity, treatments) #WARNING: we need to make sure that the order in activity_treatments matches the x-axis
    avgSleep <- list()
    for (i in 1:length(activity_treatments))
      avgSleep[[i]] <-
      sapply(
        activity_treatments[[i]],
        FUN = function(x) {
          mean(x$mvBouts.lengths)
        }
      ) / hz
    
    if (is.na(treatmentLevels[1]))
      treatmentLevels <- paste('group', levels(treatments))
    
    box <- boxplot(avgSleep, xaxt = 'n', frame = F)
    axis(
      side = 1,
      at = 1:nlevels(treatments),
      labels = treatmentLevels,
      cex.axis = 2
    )
    mtext(
      side = 2,
      text = 'Mean length mv bout (s)',
      cex = 2,
      line = 2
    )
    
    if (nlevels(treatments) == 2 & !is.na(test)) {
      if (test == 'wilcox') {
        p <- wilcox.test(avgSleep[[1]], avgSleep[[2]])$p.value
        # text(x = 1.5, y = mean(box$stats[5,]), labels = paste('p =', round(p, digits = 3)), cex = 2)
        legend('topleft', paste('p =', round(p, digits = 3)), title = 'Wilcoxon')
      }
      if (test == 't.test') {
        p <- t.test(avgSleep[[1]], avgSleep[[2]])$p.value
        # text(x = 1.5, y = mean(box$stats[5,]), labels = paste('p =', round(p, digits = 3)), cex = 2)
        legend('topleft', paste('p =', round(p, digits = 3)), title = 't-test')
      }
    }
    if (nlevels(treatments) > 2 & !is.na(test)) {
      model <- lm(unlist(avgSleep) ~ treatments)
      p <- lmp(model)
      # text(x = 1.5, y = mean(box$stats[5,]), labels = paste('p =', round(p, digits = 3)), cex = 2)
      legend('topleft', paste('p =', round(p, digits = 3)), title = 'Anova')
    }
  }
#Note,
plot.gxeBoxes <-
  function(trak,
           control = FALSE,
           treatment = TRUE,
           trait = 'avgMvLength') {
    #Genotype x Environment visualization example
    meta <- trak@metadata
    activity <- trak@activity$result
    
    flies <- activity
    y = c()
    if (trait == "avgMvLength") {
      for (i in 1:length(activity)) {
        if (!is.na(activity[[i]]$mvBouts.lengths[1])) {
          y[i] <- mean(activity[[i]]$mvBouts.lengths)
        }
        else{
          y[i] <- 0
        }
      }
    }
    fam <- as.character(meta$Family)
    rotenone <- meta$Treatment
    
    #Fit models with and without interaction
    model.add <- summary(lm(y ~ rotenone + fam))
    model.int <- summary(lm(y ~ rotenone * fam))
    familyPlusRot.r2 <- round(model.add$adj.r.squared, 3)
    familyTimesRot.r2 <- round(model.int$adj.r.squared, 3)
    
    box <- boxplot(y ~ fam * rotenone, plot = F)
    #Re-order boxes
    newOrder <- order(box$names)
    box.newOrder <- box
    box.newOrder$stats <- box$stats[, newOrder]
    box.newOrder$n <- box$n[newOrder]
    box.newOrder$names <- box$names[newOrder]
    box.newOrder$conf <- box$conf[, newOrder]
    for (j in 1:length(newOrder)) {
      box.newOrder$group[box$group == newOrder[j]] <- j
    }
    cols <- sort(rep(rainbow(length(unique(
      fam
    ))), 2))
    #vector to space the boxes
    spacing <- 3
    tmp <- seq(1, length(box$names) * spacing / 2, spacing)
    at = sort(c(tmp, tmp + 1))
    #png(filename = paste('GxE_', trait, '_raw.png', sep = ''), width = 1200, height = 800)
    
    bxp(box.newOrder,
        boxfill = cols,
        xaxt = 'n',
        at = at)
    labels <-
      gsub(pattern = '(.*)\\..*',
           replacement = '\\1',
           x = box.newOrder$names)[seq(1, length(box.newOrder$names), 2)]
    grid(nx = length(labels) * 2, ny = length(labels) * 2) #nx = 52, ny = 52
    
    bxp(
      box.newOrder,
      boxfill = cols,
      xaxt = 'n',
      at = at,
      add = T
    )
    
    axis(
      side = 1,
      at = seq(1.5, length(box$names) * spacing / 2 - .5, spacing),
      labels = labels,
      las = 2
    )
    mtext(trait,
          side = 2,
          cex = 2,
          line = 2.5)
    #TODO - SW - Fix labeling
    mtext(
      'Treatments per family',
      side = 3,
      cex = 2,
      line = 1
    ) #Need to fix labeling
    #Legend with R2s
    legend <-
      c(as.expression(bquote(R2[add] ~ '=' ~ .(familyPlusRot.r2))),
        as.expression(bquote(R2[int] ~ '=' ~ .(
          familyTimesRot.r2
        ))))
    legend(
      x = 'topleft',
      legend = legend,
      title = 'Variance Explained',
      cex = 1
    )
    #dev.off()
  }


plot.singleFlyDirectonality <- function(trak, flyNumber,startIndex=5,endIndex=100,colorPalette="Darjeeling2") {
  
  coords <- trak@centroid[startIndex:endIndex, c(flyNumber * 2 - 1, flyNumber * 2)]
  coords[, 2] <- 1 - coords[, 2] #flip y coordinates
  coords =coords[coords[1]!=0 & coords[2]!=0,]
  xlim <- range(coords[coords[, 1] > 0, 1])
  ylim <- range(coords[coords[, 2] < 0, 2])
  direction <- trak@direction[startIndex:endIndex, flyNumber]
  ncols <- (length(unique(direction)))
  pal <- colorRampPalette(wes_palette(colorPalette, n = 2, "discrete"))
  cols <- pal(ncols)[as.numeric(cut(direction, breaks = ncols))]
  
  plot(
    coords,
    col = cols,
    pch = 19,
    xlab='',
    ylab=''
  )
}

plot.singleFlyDensity <- function(trak, flyNumber,startIndex=5,endIndex=1000, bins = 10, wesAndersonPalette = "BottleRocket2") {
  
  coords <- trak@centroid[startIndex:endIndex, c(flyNumber * 2 - 1, flyNumber * 2)]
  coords[, 2] <- 1 - coords[, 2] #flip y coordinates
  coords =coords[coords[1]!=0 & coords[2]!=0,]
  
  cols = wes_palette(colorPalette,2,"continuous")
  ggplot(coords, mapping = aes_string(x = names(coords)[1], y = names(coords)[2])) +
    geom_hex(bins = 20) +
    scale_fill_gradientn(colors=cols) +
    ggtitle("Hexbin") +
    xlab("X") + ylab("Y")
}
