##### Example analysis pipeline #####
#Experiment with multiple crosses and tracking data from both parents and F1s (WARNING: quite messy)
# - Calls flies.sleepActivity() for all tracking runs
# - Combines the results from all tracking runs
# - Regress out plate effects
# - Fit linear models to quantify effects of family, treatment, family*treatment, sex, plate etc

# load('180529_NN_NCcross180425_offspring.RData')
# load('180530_NN_NCcross180425_parents.RData')
# load('180529_NN_NCcross180503_offspring.RData')
# load('180529_NN_NCcross180503_parents.RData')
# load('180531_NN_NCcross180506_offspring.RData')
# load('180531_NN_NCcross180506_parents.RData')
# 
# #Reprocess the phenotypes with the somewhat updated flies.sleepActivity()
# #Eventually, I should probably do some more upstream cleaning of the speed data first
# #Cross 1
# NN_NC_cross180425_offspring1.speed_noEmpt_filt06.activity <- flies.sleepActivity(NN_NC_cross180425_offspring1.speed_noEmpt_filt06, mvThreshold = 2, 
#                                                                                  deathThreshold = 4*60^2, erroneousSleepDataThreshold = 5, smoothingFactor = 2)
# NN_NC_cross180425_offspring2.speed_noEmpt_filt06.activity <- flies.sleepActivity(NN_NC_cross180425_offspring2.speed_noEmpt_filt06, mvThreshold = 2, 
#                                                                                  deathThreshold = 4*60^2, erroneousSleepDataThreshold = 5, smoothingFactor = 2)
# NN_NC_cross180425_offspring3.speed_noEmpt_filt06.activity <- flies.sleepActivity(NN_NC_cross180425_offspring3.speed_noEmpt_filt06, mvThreshold = 2, 
#                                                                                  deathThreshold = 4*60^2, erroneousSleepDataThreshold = 5, smoothingFactor = 2)
# NN_NC_cross180425_offspring4.speed_noEmpt_filt06.activity <- flies.sleepActivity(NN_NC_cross180425_offspring4.speed_noEmpt_filt06, mvThreshold = 2, 
#                                                                                  deathThreshold = 4*60^2, erroneousSleepDataThreshold = 5, smoothingFactor = 2)
# NN_NC_cross180425_parents.speed_noEmpt_filt06.activity <- flies.sleepActivity(NN_NC_cross180425_parents.speed_noEmpt_filt06, mvThreshold = 2, 
#                                                                               deathThreshold = 4*60^2, erroneousSleepDataThreshold = 5, smoothingFactor = 2)
# #Cross 2
# NN_NC_cross180503_offspring1.speed_noEmpt_filt06.activity <- flies.sleepActivity(NN_NC_cross180503_offspring1.speed_noEmpt_filt06, mvThreshold = 2, 
#                                                                                  deathThreshold = 4*60^2, erroneousSleepDataThreshold = 5, smoothingFactor = 2)
# NN_NC_cross180503_offspring3.speed_noEmpt_filt06.activity <- flies.sleepActivity(NN_NC_cross180503_offspring3.speed_noEmpt_filt06, mvThreshold = 2, 
#                                                                                  deathThreshold = 4*60^2, erroneousSleepDataThreshold = 5, smoothingFactor = 2)
# NN_NC_cross180503_offspring4.speed_noEmpt_filt06.activity <- flies.sleepActivity(NN_NC_cross180503_offspring4.speed_noEmpt_filt06, mvThreshold = 2, 
#                                                                                  deathThreshold = 4*60^2, erroneousSleepDataThreshold = 5, smoothingFactor = 2)
# NN_NC_cross180503_parents.speed_noEmpt_filt06.activity <- flies.sleepActivity(NN_NC_cross180503_parents.speed_noEmpt_filt06, mvThreshold = 2, 
#                                                                               deathThreshold = 4*60^2, erroneousSleepDataThreshold = 5, smoothingFactor = 2)
# #Cross 3
# NN_NC_cross180506_offspring1.speed_noEmpt_filt06.activity <- flies.sleepActivity(NN_NC_cross180506_offspring1.speed_noEmpt_filt06, mvThreshold = 2, 
#                                                                                  deathThreshold = 4*60^2, erroneousSleepDataThreshold = 5, smoothingFactor = 2)
# NN_NC_cross180506_offspring2.speed_noEmpt_filt06.activity <- flies.sleepActivity(NN_NC_cross180506_offspring2.speed_noEmpt_filt06, mvThreshold = 2, 
#                                                                                  deathThreshold = 4*60^2, erroneousSleepDataThreshold = 5, smoothingFactor = 2)
# NN_NC_cross180506_offspring3.speed_noEmpt_filt06.activity <- flies.sleepActivity(NN_NC_cross180506_offspring3.speed_noEmpt_filt06, mvThreshold = 2, 
#                                                                                  deathThreshold = 4*60^2, erroneousSleepDataThreshold = 5, smoothingFactor = 2)
# NN_NC_cross180506_offspring4.speed_noEmpt_filt06.activity <- flies.sleepActivity(NN_NC_cross180506_offspring4.speed_noEmpt_filt06, mvThreshold = 2, 
#                                                                                  deathThreshold = 4*60^2, erroneousSleepDataThreshold = 5, smoothingFactor = 2)
# NN_NC_cross180506_parents.speed_noEmpt_filt06.activity <- flies.sleepActivity(NN_NC_cross180506_parents.speed_noEmpt_filt06, mvThreshold = 2, 
#                                                                               deathThreshold = 4*60^2, erroneousSleepDataThreshold = 5, smoothingFactor = 2)
# 
# 
# #Merge all the offspring meta data
# NN_NC_180531_allOffspring <- rbind(data.frame(well = NN_NC_cross180425_offspring1.data_noEmpt[,1], NN_NC_cross180425_offspring1.data_noEmpt[,2:4], plate = paste('plate', NN_NC_cross180425_offspring1.data_noEmpt$Plate, '_180425Off1', sep = ''), Family = NN_NC_cross180425_offspring1.data_noEmpt$Family),
#                                    data.frame(well = NN_NC_cross180425_offspring2.data_noEmpt[,1], NN_NC_cross180425_offspring2.data_noEmpt[,2:4], plate = paste('plate', NN_NC_cross180425_offspring2.data_noEmpt$Plate, '_180425Off2', sep = ''), Family = NN_NC_cross180425_offspring2.data_noEmpt$Family),
#                                    data.frame(well = NN_NC_cross180425_offspring3.data_noEmpt[,1], NN_NC_cross180425_offspring3.data_noEmpt[,2:4], plate = paste('plate', NN_NC_cross180425_offspring3.data_noEmpt$Plate, '_180425Off3', sep = ''), Family = NN_NC_cross180425_offspring3.data_noEmpt$Family),
#                                    data.frame(well = NN_NC_cross180425_offspring4.data_noEmpt[,1], NN_NC_cross180425_offspring4.data_noEmpt[,2:4], plate = paste('plate', NN_NC_cross180425_offspring4.data_noEmpt$Plate, '_180425Off4', sep = ''), Family = NN_NC_cross180425_offspring4.data_noEmpt$Family),
#                                    
#                                    data.frame(well = NN_NC_cross180503_offspring1.data_noEmpt[,1], NN_NC_cross180503_offspring1.data_noEmpt[,2:4], plate = paste('plate', NN_NC_cross180503_offspring1.data_noEmpt$Plate, '_180503Off1', sep = ''), Family = NN_NC_cross180503_offspring1.data_noEmpt$Family),
#                                    # NN_NC_cross180503_offspring2.data_noEmpt[,2:4], paste('plate', NN_NC_cross180503_offspring2.data_noEmpt$Plate, '_180503Off2', sep = ''), NN_NC_cross180503_offspring2.data_noEmpt$Family,
#                                    data.frame(well = NN_NC_cross180503_offspring3.data_noEmpt[,1], NN_NC_cross180503_offspring3.data_noEmpt[,2:4], plate = paste('plate', NN_NC_cross180503_offspring3.data_noEmpt$Plate, '_180503Off3', sep = ''), Family = NN_NC_cross180503_offspring3.data_noEmpt$Family),
#                                    data.frame(well = NN_NC_cross180503_offspring4.data_noEmpt[,1], NN_NC_cross180503_offspring4.data_noEmpt[,2:4], plate = paste('plate', NN_NC_cross180503_offspring4.data_noEmpt$Plate, '_180503Off4', sep = ''), Family = NN_NC_cross180503_offspring4.data_noEmpt$Family),
#                                    
#                                    data.frame(well = NN_NC_cross180506_offspring1.data_noEmpt[,1], NN_NC_cross180506_offspring1.data_noEmpt[,2:4], plate = paste('plate', NN_NC_cross180506_offspring1.data_noEmpt$Plate, '_180506Off1', sep = ''), Family = NN_NC_cross180506_offspring1.data_noEmpt$Family),
#                                    data.frame(well = NN_NC_cross180506_offspring2.data_noEmpt[,1], NN_NC_cross180506_offspring2.data_noEmpt[,2:4], plate = paste('plate', NN_NC_cross180506_offspring2.data_noEmpt$Plate, '_180506Off2', sep = ''), Family = NN_NC_cross180506_offspring2.data_noEmpt$Family),
#                                    data.frame(well = NN_NC_cross180506_offspring3.data_noEmpt[,1], NN_NC_cross180506_offspring3.data_noEmpt[,2:4], plate = paste('plate', NN_NC_cross180506_offspring3.data_noEmpt$Plate, '_180506Off3', sep = ''), Family = NN_NC_cross180506_offspring3.data_noEmpt$Family),
#                                    data.frame(well = NN_NC_cross180506_offspring4.data_noEmpt[,1], NN_NC_cross180506_offspring4.data_noEmpt[,2:4], plate = paste('plate', NN_NC_cross180506_offspring4.data_noEmpt$Plate, '_180506Off4', sep = ''), Family = NN_NC_cross180506_offspring4.data_noEmpt$Family))
# #Add empty cols for phenotypes
# NN_NC_180531_allOffspring <- cbind(NN_NC_180531_allOffspring, sleepNr = NA, mvNr = NA, avgMvLength = NA, avgSleepLength = NA, avgBoutSpeed = NA, totSleep = NA,
#                                    midP_sleep = NA, midP_mv = NA, midP_avgMvLength = NA, midP_avgSleepLength = NA, midP_avgBoutSpeed = NA, midP_totSleep = NA)
# 
# #Merge all the parent meta data
# parents180425.plate <- NN_NC_cross180425_parents.data_noEmpt$Well_orderAsTracked
# parents180425.plate[parents180425.plate < 25] <- 1
# parents180425.plate[parents180425.plate > 1 & parents180425.plate < 49] <- 2
# parents180425.plate[parents180425.plate > 2 & parents180425.plate < 73] <- 3
# parents180425.plate[parents180425.plate > 3] <- 4
# NN_NC_180531_allParents <- rbind(data.frame(Well = NN_NC_cross180425_parents.data_noEmpt$Well_orderAsTracked, Family = NN_NC_cross180425_parents.data_noEmpt$family, Empty = NN_NC_cross180425_parents.data_noEmpt$Empty, Male = NN_NC_cross180425_parents.data_noEmpt$Male, plate = paste('plate', parents180425.plate, '_180425', sep = '')), 
#                                  data.frame(Well = NN_NC_cross180503_parents.data_noEmpt$Well_orderAsTracked, Family = NN_NC_cross180503_parents.data_noEmpt$Family, Empty = NN_NC_cross180503_parents.data_noEmpt$Empty, Male = NN_NC_cross180503_parents.data_noEmpt$Male, plate = paste('plate', NN_NC_cross180503_parents.data_noEmpt$Plate, '_180503', sep = '')),
#                                  data.frame(Well = NN_NC_cross180506_parents.data_noEmpt$Well_orderAsTracked, Family = NN_NC_cross180506_parents.data_noEmpt$Family, Empty = NN_NC_cross180506_parents.data_noEmpt$Empty, Male = NN_NC_cross180506_parents.data_noEmpt$Male, plate = paste('plate', NN_NC_cross180506_parents.data_noEmpt$Plate, '_180506', sep = '')))
# 
# #Merge meta data for all flies
# NN_NC_180531_allFlies <- rbind(data.frame(well = NN_NC_cross180425_parents.data_noEmpt$Well_orderAsTracked, Rotenone = F, Family = NN_NC_cross180425_parents.data_noEmpt$family, Male = NN_NC_cross180425_parents.data_noEmpt$Male, Empty = NN_NC_cross180425_parents.data_noEmpt$Empty, plate = paste('plate', parents180425.plate, '_180425', sep = ''),  parent = T),
#                                data.frame(well = NN_NC_cross180503_parents.data_noEmpt$Well_orderAsTracked, Rotenone = F, Family = NN_NC_cross180503_parents.data_noEmpt$Family, Male = NN_NC_cross180503_parents.data_noEmpt$Male, Empty = NN_NC_cross180503_parents.data_noEmpt$Empty, plate = paste('plate', NN_NC_cross180503_parents.data_noEmpt$Plate, '_180503', sep = ''), parent = T),
#                                data.frame(well = NN_NC_cross180506_parents.data_noEmpt$Well_orderAsTracked, Rotenone = F, Family = NN_NC_cross180506_parents.data_noEmpt$Family, Male = NN_NC_cross180506_parents.data_noEmpt$Male, Empty = NN_NC_cross180506_parents.data_noEmpt$Empty, plate = paste('plate', NN_NC_cross180506_parents.data_noEmpt$Plate, '_180506', sep = ''), parent = T),
#                                
#                                data.frame(well = NN_NC_cross180425_offspring1.data_noEmpt[,1], NN_NC_cross180425_offspring1.data_noEmpt[,2:4], plate = paste('plate', NN_NC_cross180425_offspring1.data_noEmpt$Plate, '_180425Off1', sep = ''), Family = NN_NC_cross180425_offspring1.data_noEmpt$Family, parent = F), 
#                                data.frame(well = NN_NC_cross180425_offspring2.data_noEmpt[,1], NN_NC_cross180425_offspring2.data_noEmpt[,2:4], plate = paste('plate', NN_NC_cross180425_offspring2.data_noEmpt$Plate, '_180425Off2', sep = ''), Family = NN_NC_cross180425_offspring2.data_noEmpt$Family, parent = F), 
#                                data.frame(well = NN_NC_cross180425_offspring3.data_noEmpt[,1], NN_NC_cross180425_offspring3.data_noEmpt[,2:4], plate = paste('plate', NN_NC_cross180425_offspring3.data_noEmpt$Plate, '_180425Off3', sep = ''), Family = NN_NC_cross180425_offspring3.data_noEmpt$Family, parent = F), 
#                                data.frame(well = NN_NC_cross180425_offspring4.data_noEmpt[,1], NN_NC_cross180425_offspring4.data_noEmpt[,2:4], plate = paste('plate', NN_NC_cross180425_offspring4.data_noEmpt$Plate, '_180425Off4', sep = ''), Family = NN_NC_cross180425_offspring4.data_noEmpt$Family, parent = F), 
#                                
#                                data.frame(well = NN_NC_cross180503_offspring1.data_noEmpt[,1], NN_NC_cross180503_offspring1.data_noEmpt[,2:4], plate = paste('plate', NN_NC_cross180503_offspring1.data_noEmpt$Plate, '_180503Off1', sep = ''), Family = NN_NC_cross180503_offspring1.data_noEmpt$Family, parent = F), 
#                                # NN_NC_cross180503_offspring2.data_noEmpt[,2:4], paste('plate', NN_NC_cross180503_offspring2.data_noEmpt$Plate, '_180503Off2', sep = ''), NN_NC_cross180503_offspring2.data_noEmpt$Family,
#                                data.frame(well = NN_NC_cross180503_offspring3.data_noEmpt[,1], NN_NC_cross180503_offspring3.data_noEmpt[,2:4], plate = paste('plate', NN_NC_cross180503_offspring3.data_noEmpt$Plate, '_180503Off3', sep = ''), Family = NN_NC_cross180503_offspring3.data_noEmpt$Family, parent = F), 
#                                data.frame(well = NN_NC_cross180503_offspring4.data_noEmpt[,1], NN_NC_cross180503_offspring4.data_noEmpt[,2:4], plate = paste('plate', NN_NC_cross180503_offspring4.data_noEmpt$Plate, '_180503Off4', sep = ''), Family = NN_NC_cross180503_offspring4.data_noEmpt$Family, parent = F), 
#                                
#                                data.frame(well = NN_NC_cross180506_offspring1.data_noEmpt[,1], NN_NC_cross180506_offspring1.data_noEmpt[,2:4], plate = paste('plate', NN_NC_cross180506_offspring1.data_noEmpt$Plate, '_180506Off1', sep = ''), Family = NN_NC_cross180506_offspring1.data_noEmpt$Family, parent = F), 
#                                data.frame(well = NN_NC_cross180506_offspring2.data_noEmpt[,1], NN_NC_cross180506_offspring2.data_noEmpt[,2:4], plate = paste('plate', NN_NC_cross180506_offspring2.data_noEmpt$Plate, '_180506Off2', sep = ''), Family = NN_NC_cross180506_offspring2.data_noEmpt$Family, parent = F), 
#                                data.frame(well = NN_NC_cross180506_offspring3.data_noEmpt[,1], NN_NC_cross180506_offspring3.data_noEmpt[,2:4], plate = paste('plate', NN_NC_cross180506_offspring3.data_noEmpt$Plate, '_180506Off3', sep = ''), Family = NN_NC_cross180506_offspring3.data_noEmpt$Family, parent = F), 
#                                data.frame(well = NN_NC_cross180506_offspring4.data_noEmpt[,1], NN_NC_cross180506_offspring4.data_noEmpt[,2:4], plate = paste('plate', NN_NC_cross180506_offspring4.data_noEmpt$Plate, '_180506Off4', sep = ''), Family = NN_NC_cross180506_offspring4.data_noEmpt$Family, parent = F)
# )
# 
# #Add empty cols for phenotypes
# NN_NC_180531_allFlies <- cbind(NN_NC_180531_allFlies, sleepNr = NA, mvNr = NA, avgMvLength = NA, avgSleepLength = NA, avgBoutSpeed = NA, totSleep = NA,
#                                midP_sleep = NA, midP_mv = NA, midP_avgMvLength = NA, midP_avgSleepLength = NA, midP_avgBoutSpeed = NA, midP_totSleep = NA)
# 
# 
# families <- as.character(levels(NN_NC_180531_allParents$Family))
# families <- families[2:length(families)]
# for(i in 1:length(families)){
#   #Indexes in the merged data frames
#   parIndex <- which(NN_NC_180531_allParents$Family == families[i])
#   offIndex <- which(NN_NC_180531_allOffspring$Family == families[i])
#   
#   #Parents
#   if(length(parIndex) == 2){ #Both parents phenotyped
#     #Determine which data.frame to get the parental phenotypes from 
#     if(length(grep(pattern = '180425', x = NN_NC_180531_allParents$plate[parIndex])) == 2){
#       index <- which(NN_NC_cross180425_parents.data_noEmpt$family == families[i]) #The index in the original list
#       if(length(index) != 2)
#         warning(paste('Didn\'t find parents for family', families[i]))
#       dad <- which(NN_NC_cross180425_parents.data_noEmpt$family == families[i] & NN_NC_cross180425_parents.data_noEmpt$Male)
#       mom <- which(NN_NC_cross180425_parents.data_noEmpt$family == families[i] & !NN_NC_cross180425_parents.data_noEmpt$Male)
#       dad.pheno <- NN_NC_cross180425_parents.speed_noEmpt_filt06.activity[[dad]]
#       mom.pheno <- NN_NC_cross180425_parents.speed_noEmpt_filt06.activity[[mom]]
#       
#       #Put parental phenotypes into NN_NC_180531_allFlies
#       NN_NC_180531_allFlies.dad <- NN_NC_180531_allFlies$Family == families[i] & NN_NC_180531_allFlies$parent & NN_NC_180531_allFlies$Male
#       NN_NC_180531_allFlies.mom <- NN_NC_180531_allFlies$Family == families[i] & NN_NC_180531_allFlies$parent & !NN_NC_180531_allFlies$Male
#       #Dad
#       NN_NC_180531_allFlies$sleepNr[NN_NC_180531_allFlies.dad] <- dad.pheno$sleepNr
#       NN_NC_180531_allFlies$mvNr[NN_NC_180531_allFlies.dad] <- dad.pheno$mvNr
#       NN_NC_180531_allFlies$avgMvLength[NN_NC_180531_allFlies.dad] <- mean(dad.pheno$mvLengths)
#       NN_NC_180531_allFlies$avgSleepLength[NN_NC_180531_allFlies.dad] <- mean(dad.pheno$sleepLengths)
#       NN_NC_180531_allFlies$avgBoutSpeed[NN_NC_180531_allFlies.dad] <- mean(dad.pheno$boutSpeeds)
#       NN_NC_180531_allFlies$totSleep[NN_NC_180531_allFlies.dad] <- sum(dad.pheno$sleepLengths)
#       #Mom
#       NN_NC_180531_allFlies$sleepNr[NN_NC_180531_allFlies.mom] <- mom.pheno$sleepNr
#       NN_NC_180531_allFlies$mvNr[NN_NC_180531_allFlies.mom] <- mom.pheno$mvNr
#       NN_NC_180531_allFlies$avgMvLength[NN_NC_180531_allFlies.mom] <- mean(mom.pheno$mvLengths)
#       NN_NC_180531_allFlies$avgSleepLength[NN_NC_180531_allFlies.mom] <- mean(mom.pheno$sleepLengths)
#       NN_NC_180531_allFlies$avgBoutSpeed[NN_NC_180531_allFlies.mom] <- mean(mom.pheno$boutSpeeds)
#       NN_NC_180531_allFlies$totSleep[NN_NC_180531_allFlies.mom] <- sum(mom.pheno$sleepLengths)
#     }
#     else if(length(grep(pattern = '180503', x = NN_NC_180531_allParents$plate[parIndex])) == 2){
#       index <- which(NN_NC_cross180503_parents.data_noEmpt$Family == families[i])
#       if(length(index) != 2)
#         warning(paste('Didn\'t find parents for family', families[i]))
#       dad <- which(NN_NC_cross180503_parents.data_noEmpt$Family == families[i] & NN_NC_cross180503_parents.data_noEmpt$Male)
#       mom <- which(NN_NC_cross180503_parents.data_noEmpt$Family == families[i] & !NN_NC_cross180503_parents.data_noEmpt$Male)
#       dad.pheno <- NN_NC_cross180503_parents.speed_noEmpt_filt06.activity[[dad]]
#       mom.pheno <- NN_NC_cross180503_parents.speed_noEmpt_filt06.activity[[mom]]
#       
#       #Put parental phenotypes into NN_NC_180531_allFlies
#       NN_NC_180531_allFlies.dad <- NN_NC_180531_allFlies$Family == families[i] & NN_NC_180531_allFlies$parent & NN_NC_180531_allFlies$Male
#       NN_NC_180531_allFlies.mom <- NN_NC_180531_allFlies$Family == families[i] & NN_NC_180531_allFlies$parent & !NN_NC_180531_allFlies$Male
#       #Dad
#       NN_NC_180531_allFlies$sleepNr[NN_NC_180531_allFlies.dad] <- dad.pheno$sleepNr
#       NN_NC_180531_allFlies$mvNr[NN_NC_180531_allFlies.dad] <- dad.pheno$mvNr
#       NN_NC_180531_allFlies$avgMvLength[NN_NC_180531_allFlies.dad] <- mean(dad.pheno$mvLengths)
#       NN_NC_180531_allFlies$avgSleepLength[NN_NC_180531_allFlies.dad] <- mean(dad.pheno$sleepLengths)
#       NN_NC_180531_allFlies$avgBoutSpeed[NN_NC_180531_allFlies.dad] <- mean(dad.pheno$boutSpeeds)
#       NN_NC_180531_allFlies$totSleep[NN_NC_180531_allFlies.dad] <- sum(dad.pheno$sleepLengths)
#       #Mom
#       NN_NC_180531_allFlies$sleepNr[NN_NC_180531_allFlies.mom] <- mom.pheno$sleepNr
#       NN_NC_180531_allFlies$mvNr[NN_NC_180531_allFlies.mom] <- mom.pheno$mvNr
#       NN_NC_180531_allFlies$avgMvLength[NN_NC_180531_allFlies.mom] <- mean(mom.pheno$mvLengths)
#       NN_NC_180531_allFlies$avgSleepLength[NN_NC_180531_allFlies.mom] <- mean(mom.pheno$sleepLengths)
#       NN_NC_180531_allFlies$avgBoutSpeed[NN_NC_180531_allFlies.mom] <- mean(mom.pheno$boutSpeeds)
#       NN_NC_180531_allFlies$totSleep[NN_NC_180531_allFlies.mom] <- sum(mom.pheno$sleepLengths)
#     }
#     else if(length(grep(pattern = '180506', x = NN_NC_180531_allParents$plate[parIndex])) == 2){
#       index <- which(NN_NC_cross180506_parents.data_noEmpt$Family == families[i])
#       if(length(index) != 2)
#         warning(paste('Didn\'t find parents for family', families[i]))
#       dad <- which(NN_NC_cross180506_parents.data_noEmpt$Family == families[i] & NN_NC_cross180506_parents.data_noEmpt$Male)
#       mom <- which(NN_NC_cross180506_parents.data_noEmpt$Family == families[i] & !NN_NC_cross180506_parents.data_noEmpt$Male)
#       dad.pheno <- NN_NC_cross180506_parents.speed_noEmpt_filt06.activity[[dad]]
#       mom.pheno <- NN_NC_cross180506_parents.speed_noEmpt_filt06.activity[[mom]]
#       
#       #Put parental phenotypes into NN_NC_180531_allFlies
#       NN_NC_180531_allFlies.dad <- NN_NC_180531_allFlies$Family == families[i] & NN_NC_180531_allFlies$parent & NN_NC_180531_allFlies$Male
#       NN_NC_180531_allFlies.mom <- NN_NC_180531_allFlies$Family == families[i] & NN_NC_180531_allFlies$parent & !NN_NC_180531_allFlies$Male
#       #Dad
#       NN_NC_180531_allFlies$sleepNr[NN_NC_180531_allFlies.dad] <- dad.pheno$sleepNr
#       NN_NC_180531_allFlies$mvNr[NN_NC_180531_allFlies.dad] <- dad.pheno$mvNr
#       NN_NC_180531_allFlies$avgMvLength[NN_NC_180531_allFlies.dad] <- mean(dad.pheno$mvLengths)
#       NN_NC_180531_allFlies$avgSleepLength[NN_NC_180531_allFlies.dad] <- mean(dad.pheno$sleepLengths)
#       NN_NC_180531_allFlies$avgBoutSpeed[NN_NC_180531_allFlies.dad] <- mean(dad.pheno$boutSpeeds)
#       NN_NC_180531_allFlies$totSleep[NN_NC_180531_allFlies.dad] <- sum(dad.pheno$sleepLengths)
#       #Mom
#       NN_NC_180531_allFlies$sleepNr[NN_NC_180531_allFlies.mom] <- mom.pheno$sleepNr
#       NN_NC_180531_allFlies$mvNr[NN_NC_180531_allFlies.mom] <- mom.pheno$mvNr
#       NN_NC_180531_allFlies$avgMvLength[NN_NC_180531_allFlies.mom] <- mean(mom.pheno$mvLengths)
#       NN_NC_180531_allFlies$avgSleepLength[NN_NC_180531_allFlies.mom] <- mean(mom.pheno$sleepLengths)
#       NN_NC_180531_allFlies$avgBoutSpeed[NN_NC_180531_allFlies.mom] <- mean(mom.pheno$boutSpeeds)
#       NN_NC_180531_allFlies$totSleep[NN_NC_180531_allFlies.mom] <- sum(mom.pheno$sleepLengths)
#     }
#     
#     #Mid parent values 
#     if(is.list(dad.pheno) & is.list(mom.pheno)){
#       NN_NC_180531_allOffspring$midP_sleep[offIndex] <- mean(c(dad.pheno$sleepNr, mom.pheno$sleepNr))
#       NN_NC_180531_allOffspring$midP_mv[offIndex] <- mean(c(dad.pheno$mvNr, mom.pheno$mvNr))
#       NN_NC_180531_allOffspring$midP_avgMvLength[offIndex] <- mean(c(mean(dad.pheno$mvLengths), mean(mom.pheno$mvLengths)))
#       NN_NC_180531_allOffspring$midP_avgSleepLength[offIndex] <- mean(c(mean(dad.pheno$sleepLengths), mean(mom.pheno$sleepLengths)))
#       NN_NC_180531_allOffspring$midP_avgBoutSpeed[offIndex] <- mean(c(mean(dad.pheno$boutSpeeds), mean(mom.pheno$boutSpeeds)))
#       NN_NC_180531_allOffspring$midP_totSleep[offIndex] <- mean(c(sum(dad.pheno$sleepLengths), sum(mom.pheno$sleepLengths)))
#     }
#     dad.pheno <- mom.pheno <- NA
#   }
#   else
#     print(i)
#   
#   
#   #Offspring
#   #Retrieve the offspring phenotypes from the right list
#   
#   #Get the positions of offspring from this tracking run in the merged data.frame
#   tmp <- grep(pattern = '180425Off1', x = NN_NC_180531_allOffspring$plate)
#   offspringInRun <- which(1:nrow(NN_NC_180531_allOffspring) %in% tmp & NN_NC_180531_allOffspring$Family == families[i])
#   if(length(offspringInRun) > 0){
#     index <- which(NN_NC_cross180425_offspring1.data_noEmpt$Family == families[i]) #Their positions in the original data frames and list
#     if(length(index) != length(offspringInRun))
#       warning(paste('Didn\'t find offspring for family', families[i]))
#     
#     #Put phenotypes into the merged data.frame
#     NN_NC_180531_allOffspring$sleepNr[offspringInRun] <- sapply(index, function(x){NN_NC_cross180425_offspring1.speed_noEmpt_filt06.activity[[x]]$sleepNr})
#     NN_NC_180531_allOffspring$mvNr[offspringInRun] <- sapply(index, function(x){NN_NC_cross180425_offspring1.speed_noEmpt_filt06.activity[[x]]$mvNr})
#     NN_NC_180531_allOffspring$avgMvLength[offspringInRun] <- sapply(index, function(x){mean(NN_NC_cross180425_offspring1.speed_noEmpt_filt06.activity[[x]]$mvLengths)})
#     NN_NC_180531_allOffspring$avgSleepLength[offspringInRun] <- sapply(index, function(x){mean(NN_NC_cross180425_offspring1.speed_noEmpt_filt06.activity[[x]]$sleepLengths)})
#     NN_NC_180531_allOffspring$avgBoutSpeed[offspringInRun] <- sapply(index, function(x){mean(NN_NC_cross180425_offspring1.speed_noEmpt_filt06.activity[[x]]$boutSpeeds)})
#     NN_NC_180531_allOffspring$totSleep[offspringInRun] <- sapply(index, function(x){sum(NN_NC_cross180425_offspring1.speed_noEmpt_filt06.activity[[x]]$sleepLengths)})
#   }
#   #Get the positions of offspring from this tracking run in the merged data.frame
#   tmp <- grep(pattern = '180425Off2', x = NN_NC_180531_allOffspring$plate)
#   offspringInRun <- which(1:nrow(NN_NC_180531_allOffspring) %in% tmp & NN_NC_180531_allOffspring$Family == families[i])
#   if(length(offspringInRun) > 0){
#     index <- which(NN_NC_cross180425_offspring2.data_noEmpt$Family == families[i]) #Their positions in the original data frames and list
#     if(length(index) != length(offspringInRun))
#       warning(paste('Didn\'t find offspring for family', families[i]))
#     
#     #Put phenotypes into the merged data.frame
#     NN_NC_180531_allOffspring$sleepNr[offspringInRun] <- sapply(index, function(x){NN_NC_cross180425_offspring2.speed_noEmpt_filt06.activity[[x]]$sleepNr})
#     NN_NC_180531_allOffspring$mvNr[offspringInRun] <- sapply(index, function(x){NN_NC_cross180425_offspring2.speed_noEmpt_filt06.activity[[x]]$mvNr})
#     NN_NC_180531_allOffspring$avgMvLength[offspringInRun] <- sapply(index, function(x){mean(NN_NC_cross180425_offspring2.speed_noEmpt_filt06.activity[[x]]$mvLengths)})
#     NN_NC_180531_allOffspring$avgSleepLength[offspringInRun] <- sapply(index, function(x){mean(NN_NC_cross180425_offspring2.speed_noEmpt_filt06.activity[[x]]$sleepLengths)})
#     NN_NC_180531_allOffspring$avgBoutSpeed[offspringInRun] <- sapply(index, function(x){mean(NN_NC_cross180425_offspring2.speed_noEmpt_filt06.activity[[x]]$boutSpeeds)})
#     NN_NC_180531_allOffspring$totSleep[offspringInRun] <- sapply(index, function(x){sum(NN_NC_cross180425_offspring2.speed_noEmpt_filt06.activity[[x]]$sleepLengths)})
#   }
#   #Get the positions of offspring from this tracking run in the merged data.frame
#   tmp <- grep(pattern = '180425Off3', x = NN_NC_180531_allOffspring$plate)
#   offspringInRun <- which(1:nrow(NN_NC_180531_allOffspring) %in% tmp & NN_NC_180531_allOffspring$Family == families[i])
#   if(length(offspringInRun) > 0){
#     index <- which(NN_NC_cross180425_offspring3.data_noEmpt$Family == families[i]) #Their positions in the original data frames and list
#     if(length(index) != length(offspringInRun))
#       warning(paste('Didn\'t find offspring for family', families[i]))
#     
#     #Put phenotypes into the merged data.frame
#     NN_NC_180531_allOffspring$sleepNr[offspringInRun] <- sapply(index, function(x){NN_NC_cross180425_offspring3.speed_noEmpt_filt06.activity[[x]]$sleepNr})
#     NN_NC_180531_allOffspring$mvNr[offspringInRun] <- sapply(index, function(x){NN_NC_cross180425_offspring3.speed_noEmpt_filt06.activity[[x]]$mvNr})
#     NN_NC_180531_allOffspring$avgMvLength[offspringInRun] <- sapply(index, function(x){mean(NN_NC_cross180425_offspring3.speed_noEmpt_filt06.activity[[x]]$mvLengths)})
#     NN_NC_180531_allOffspring$avgSleepLength[offspringInRun] <- sapply(index, function(x){mean(NN_NC_cross180425_offspring3.speed_noEmpt_filt06.activity[[x]]$sleepLengths)})
#     NN_NC_180531_allOffspring$avgBoutSpeed[offspringInRun] <- sapply(index, function(x){mean(NN_NC_cross180425_offspring3.speed_noEmpt_filt06.activity[[x]]$boutSpeeds)})
#     NN_NC_180531_allOffspring$totSleep[offspringInRun] <- sapply(index, function(x){sum(NN_NC_cross180425_offspring3.speed_noEmpt_filt06.activity[[x]]$sleepLengths)})
#   }
#   #Get the positions of offspring from this tracking run in the merged data.frame
#   tmp <- grep(pattern = '180425Off4', x = NN_NC_180531_allOffspring$plate)
#   offspringInRun <- which(1:nrow(NN_NC_180531_allOffspring) %in% tmp & NN_NC_180531_allOffspring$Family == families[i])
#   if(length(offspringInRun) > 0){
#     index <- which(NN_NC_cross180425_offspring4.data_noEmpt$Family == families[i]) #Their positions in the original data frames and list
#     if(length(index) != length(offspringInRun))
#       warning(paste('Didn\'t find offspring for family', families[i]))
#     
#     #Put phenotypes into the merged data.frame
#     NN_NC_180531_allOffspring$sleepNr[offspringInRun] <- sapply(index, function(x){NN_NC_cross180425_offspring4.speed_noEmpt_filt06.activity[[x]]$sleepNr})
#     NN_NC_180531_allOffspring$mvNr[offspringInRun] <- sapply(index, function(x){NN_NC_cross180425_offspring4.speed_noEmpt_filt06.activity[[x]]$mvNr})
#     NN_NC_180531_allOffspring$avgMvLength[offspringInRun] <- sapply(index, function(x){mean(NN_NC_cross180425_offspring4.speed_noEmpt_filt06.activity[[x]]$mvLengths)})
#     NN_NC_180531_allOffspring$avgSleepLength[offspringInRun] <- sapply(index, function(x){mean(NN_NC_cross180425_offspring4.speed_noEmpt_filt06.activity[[x]]$sleepLengths)})
#     NN_NC_180531_allOffspring$avgBoutSpeed[offspringInRun] <- sapply(index, function(x){mean(NN_NC_cross180425_offspring4.speed_noEmpt_filt06.activity[[x]]$boutSpeeds)})
#     NN_NC_180531_allOffspring$totSleep[offspringInRun] <- sapply(index, function(x){sum(NN_NC_cross180425_offspring4.speed_noEmpt_filt06.activity[[x]]$sleepLengths)})
#   }
#   
#   #Cross 2
#   #Get the positions of offspring from this tracking run in the merged data.frame
#   tmp <- grep(pattern = '180503Off1', x = NN_NC_180531_allOffspring$plate)
#   offspringInRun <- which(1:nrow(NN_NC_180531_allOffspring) %in% tmp & NN_NC_180531_allOffspring$Family == families[i])
#   if(length(offspringInRun) > 0){
#     index <- which(NN_NC_cross180503_offspring1.data_noEmpt$Family == families[i]) #Their positions in the original data frames and list
#     if(length(index) != length(offspringInRun))
#       warning(paste('Didn\'t find offspring for family', families[i]))
#     
#     #Put phenotypes into the merged data.frame
#     NN_NC_180531_allOffspring$sleepNr[offspringInRun] <- sapply(index, function(x){NN_NC_cross180503_offspring1.speed_noEmpt_filt06.activity[[x]]$sleepNr})
#     NN_NC_180531_allOffspring$mvNr[offspringInRun] <- sapply(index, function(x){NN_NC_cross180503_offspring1.speed_noEmpt_filt06.activity[[x]]$mvNr})
#     NN_NC_180531_allOffspring$avgMvLength[offspringInRun] <- sapply(index, function(x){mean(NN_NC_cross180503_offspring1.speed_noEmpt_filt06.activity[[x]]$mvLengths)})
#     NN_NC_180531_allOffspring$avgSleepLength[offspringInRun] <- sapply(index, function(x){mean(NN_NC_cross180503_offspring1.speed_noEmpt_filt06.activity[[x]]$sleepLengths)})
#     NN_NC_180531_allOffspring$avgBoutSpeed[offspringInRun] <- sapply(index, function(x){mean(NN_NC_cross180503_offspring1.speed_noEmpt_filt06.activity[[x]]$boutSpeeds)})
#     NN_NC_180531_allOffspring$totSleep[offspringInRun] <- sapply(index, function(x){sum(NN_NC_cross180503_offspring1.speed_noEmpt_filt06.activity[[x]]$sleepLengths)})
#   }
#   #Get the positions of offspring from this tracking run in the merged data.frame
#   tmp <- grep(pattern = '180503Off2', x = NN_NC_180531_allOffspring$plate)
#   offspringInRun <- which(1:nrow(NN_NC_180531_allOffspring) %in% tmp & NN_NC_180531_allOffspring$Family == families[i])
#   if(length(offspringInRun) > 0){
#     index <- which(NN_NC_cross180503_offspring2.data_noEmpt$Family == families[i]) #Their positions in the original data frames and list
#     if(length(index) != length(offspringInRun))
#       warning(paste('Didn\'t find offspring for family', families[i]))
#     
#     #Put phenotypes into the merged data.frame
#     NN_NC_180531_allOffspring$sleepNr[offspringInRun] <- sapply(index, function(x){NN_NC_cross180503_offspring2.speed_noEmpt_filt06.activity[[x]]$sleepNr})
#     NN_NC_180531_allOffspring$mvNr[offspringInRun] <- sapply(index, function(x){NN_NC_cross180503_offspring2.speed_noEmpt_filt06.activity[[x]]$mvNr})
#     NN_NC_180531_allOffspring$avgMvLength[offspringInRun] <- sapply(index, function(x){mean(NN_NC_cross180503_offspring2.speed_noEmpt_filt06.activity[[x]]$mvLengths)})
#     NN_NC_180531_allOffspring$avgSleepLength[offspringInRun] <- sapply(index, function(x){mean(NN_NC_cross180503_offspring2.speed_noEmpt_filt06.activity[[x]]$sleepLengths)})
#     NN_NC_180531_allOffspring$avgBoutSpeed[offspringInRun] <- sapply(index, function(x){mean(NN_NC_cross180503_offspring2.speed_noEmpt_filt06.activity[[x]]$boutSpeeds)})
#     NN_NC_180531_allOffspring$totSleep[offspringInRun] <- sapply(index, function(x){sum(NN_NC_cross180503_offspring2.speed_noEmpt_filt06.activity[[x]]$sleepLengths)})
#   }
#   #Get the positions of offspring from this tracking run in the merged data.frame
#   tmp <- grep(pattern = '180503Off3', x = NN_NC_180531_allOffspring$plate)
#   offspringInRun <- which(1:nrow(NN_NC_180531_allOffspring) %in% tmp & NN_NC_180531_allOffspring$Family == families[i])
#   if(length(offspringInRun) > 0){
#     index <- which(NN_NC_cross180503_offspring3.data_noEmpt$Family == families[i]) #Their positions in the original data frames and list
#     if(length(index) != length(offspringInRun))
#       warning(paste('Didn\'t find offspring for family', families[i]))
#     
#     #Put phenotypes into the merged data.frame
#     NN_NC_180531_allOffspring$sleepNr[offspringInRun] <- sapply(index, function(x){NN_NC_cross180503_offspring3.speed_noEmpt_filt06.activity[[x]]$sleepNr})
#     NN_NC_180531_allOffspring$mvNr[offspringInRun] <- sapply(index, function(x){NN_NC_cross180503_offspring3.speed_noEmpt_filt06.activity[[x]]$mvNr})
#     NN_NC_180531_allOffspring$avgMvLength[offspringInRun] <- sapply(index, function(x){mean(NN_NC_cross180503_offspring3.speed_noEmpt_filt06.activity[[x]]$mvLengths)})
#     NN_NC_180531_allOffspring$avgSleepLength[offspringInRun] <- sapply(index, function(x){mean(NN_NC_cross180503_offspring3.speed_noEmpt_filt06.activity[[x]]$sleepLengths)})
#     NN_NC_180531_allOffspring$avgBoutSpeed[offspringInRun] <- sapply(index, function(x){mean(NN_NC_cross180503_offspring3.speed_noEmpt_filt06.activity[[x]]$boutSpeeds)})
#     NN_NC_180531_allOffspring$totSleep[offspringInRun] <- sapply(index, function(x){sum(NN_NC_cross180503_offspring3.speed_noEmpt_filt06.activity[[x]]$sleepLengths)})
#   }
#   #Get the positions of offspring from this tracking run in the merged data.frame
#   tmp <- grep(pattern = '180503Off4', x = NN_NC_180531_allOffspring$plate)
#   offspringInRun <- which(1:nrow(NN_NC_180531_allOffspring) %in% tmp & NN_NC_180531_allOffspring$Family == families[i])
#   if(length(offspringInRun) > 0){
#     index <- which(NN_NC_cross180503_offspring4.data_noEmpt$Family == families[i]) #Their positions in the original data frames and list
#     if(length(index) != length(offspringInRun))
#       warning(paste('Didn\'t find offspring for family', families[i]))
#     
#     #Put phenotypes into the merged data.frame
#     NN_NC_180531_allOffspring$sleepNr[offspringInRun] <- sapply(index, function(x){NN_NC_cross180503_offspring4.speed_noEmpt_filt06.activity[[x]]$sleepNr})
#     NN_NC_180531_allOffspring$mvNr[offspringInRun] <- sapply(index, function(x){NN_NC_cross180503_offspring4.speed_noEmpt_filt06.activity[[x]]$mvNr})
#     NN_NC_180531_allOffspring$avgMvLength[offspringInRun] <- sapply(index, function(x){mean(NN_NC_cross180503_offspring4.speed_noEmpt_filt06.activity[[x]]$mvLengths)})
#     NN_NC_180531_allOffspring$avgSleepLength[offspringInRun] <- sapply(index, function(x){mean(NN_NC_cross180503_offspring4.speed_noEmpt_filt06.activity[[x]]$sleepLengths)})
#     NN_NC_180531_allOffspring$avgBoutSpeed[offspringInRun] <- sapply(index, function(x){mean(NN_NC_cross180503_offspring4.speed_noEmpt_filt06.activity[[x]]$boutSpeeds)})
#     NN_NC_180531_allOffspring$totSleep[offspringInRun] <- sapply(index, function(x){sum(NN_NC_cross180503_offspring4.speed_noEmpt_filt06.activity[[x]]$sleepLengths)})
#   }
#   
#   #Cross 3
#   #Get the positions of offspring from this tracking run in the merged data.frame
#   tmp <- grep(pattern = '180506Off1', x = NN_NC_180531_allOffspring$plate)
#   offspringInRun <- which(1:nrow(NN_NC_180531_allOffspring) %in% tmp & NN_NC_180531_allOffspring$Family == families[i])
#   if(length(offspringInRun) > 0){
#     index <- which(NN_NC_cross180506_offspring1.data_noEmpt$Family == families[i]) #Their positions in the original data frames and list
#     if(length(index) != length(offspringInRun))
#       warning(paste('Didn\'t find offspring for family', families[i]))
#     
#     #Put phenotypes into the merged data.frame
#     NN_NC_180531_allOffspring$sleepNr[offspringInRun] <- sapply(index, function(x){NN_NC_cross180506_offspring1.speed_noEmpt_filt06.activity[[x]]$sleepNr})
#     NN_NC_180531_allOffspring$mvNr[offspringInRun] <- sapply(index, function(x){NN_NC_cross180506_offspring1.speed_noEmpt_filt06.activity[[x]]$mvNr})
#     NN_NC_180531_allOffspring$avgMvLength[offspringInRun] <- sapply(index, function(x){mean(NN_NC_cross180506_offspring1.speed_noEmpt_filt06.activity[[x]]$mvLengths)})
#     NN_NC_180531_allOffspring$avgSleepLength[offspringInRun] <- sapply(index, function(x){mean(NN_NC_cross180506_offspring1.speed_noEmpt_filt06.activity[[x]]$sleepLengths)})
#     NN_NC_180531_allOffspring$avgBoutSpeed[offspringInRun] <- sapply(index, function(x){mean(NN_NC_cross180506_offspring1.speed_noEmpt_filt06.activity[[x]]$boutSpeeds)})
#     NN_NC_180531_allOffspring$totSleep[offspringInRun] <- sapply(index, function(x){sum(NN_NC_cross180506_offspring1.speed_noEmpt_filt06.activity[[x]]$sleepLengths)})
#   }
#   #Get the positions of offspring from this tracking run in the merged data.frame
#   tmp <- grep(pattern = '180506Off2', x = NN_NC_180531_allOffspring$plate)
#   offspringInRun <- which(1:nrow(NN_NC_180531_allOffspring) %in% tmp & NN_NC_180531_allOffspring$Family == families[i])
#   if(length(offspringInRun) > 0){
#     index <- which(NN_NC_cross180506_offspring2.data_noEmpt$Family == families[i]) #Their positions in the original data frames and list
#     if(length(index) != length(offspringInRun))
#       warning(paste('Didn\'t find offspring for family', families[i]))
#     
#     #Put phenotypes into the merged data.frame
#     NN_NC_180531_allOffspring$sleepNr[offspringInRun] <- sapply(index, function(x){NN_NC_cross180506_offspring2.speed_noEmpt_filt06.activity[[x]]$sleepNr})
#     NN_NC_180531_allOffspring$mvNr[offspringInRun] <- sapply(index, function(x){NN_NC_cross180506_offspring2.speed_noEmpt_filt06.activity[[x]]$mvNr})
#     NN_NC_180531_allOffspring$avgMvLength[offspringInRun] <- sapply(index, function(x){mean(NN_NC_cross180506_offspring2.speed_noEmpt_filt06.activity[[x]]$mvLengths)})
#     NN_NC_180531_allOffspring$avgSleepLength[offspringInRun] <- sapply(index, function(x){mean(NN_NC_cross180506_offspring2.speed_noEmpt_filt06.activity[[x]]$sleepLengths)})
#     NN_NC_180531_allOffspring$avgBoutSpeed[offspringInRun] <- sapply(index, function(x){mean(NN_NC_cross180506_offspring2.speed_noEmpt_filt06.activity[[x]]$boutSpeeds)})
#     NN_NC_180531_allOffspring$totSleep[offspringInRun] <- sapply(index, function(x){sum(NN_NC_cross180506_offspring2.speed_noEmpt_filt06.activity[[x]]$sleepLengths)})
#   }
#   #Get the positions of offspring from this tracking run in the merged data.frame
#   tmp <- grep(pattern = '180506Off3', x = NN_NC_180531_allOffspring$plate)
#   offspringInRun <- which(1:nrow(NN_NC_180531_allOffspring) %in% tmp & NN_NC_180531_allOffspring$Family == families[i])
#   if(length(offspringInRun) > 0){
#     index <- which(NN_NC_cross180506_offspring3.data_noEmpt$Family == families[i]) #Their positions in the original data frames and list
#     if(length(index) != length(offspringInRun))
#       warning(paste('Didn\'t find offspring for family', families[i]))
#     
#     #Put phenotypes into the merged data.frame
#     NN_NC_180531_allOffspring$sleepNr[offspringInRun] <- sapply(index, function(x){NN_NC_cross180506_offspring3.speed_noEmpt_filt06.activity[[x]]$sleepNr})
#     NN_NC_180531_allOffspring$mvNr[offspringInRun] <- sapply(index, function(x){NN_NC_cross180506_offspring3.speed_noEmpt_filt06.activity[[x]]$mvNr})
#     NN_NC_180531_allOffspring$avgMvLength[offspringInRun] <- sapply(index, function(x){mean(NN_NC_cross180506_offspring3.speed_noEmpt_filt06.activity[[x]]$mvLengths)})
#     NN_NC_180531_allOffspring$avgSleepLength[offspringInRun] <- sapply(index, function(x){mean(NN_NC_cross180506_offspring3.speed_noEmpt_filt06.activity[[x]]$sleepLengths)})
#     NN_NC_180531_allOffspring$avgBoutSpeed[offspringInRun] <- sapply(index, function(x){mean(NN_NC_cross180506_offspring3.speed_noEmpt_filt06.activity[[x]]$boutSpeeds)})
#     NN_NC_180531_allOffspring$totSleep[offspringInRun] <- sapply(index, function(x){sum(NN_NC_cross180506_offspring3.speed_noEmpt_filt06.activity[[x]]$sleepLengths)})
#   }
#   #Get the positions of offspring from this tracking run in the merged data.frame
#   tmp <- grep(pattern = '180506Off4', x = NN_NC_180531_allOffspring$plate)
#   offspringInRun <- which(1:nrow(NN_NC_180531_allOffspring) %in% tmp & NN_NC_180531_allOffspring$Family == families[i])
#   if(length(offspringInRun) > 0){
#     index <- which(NN_NC_cross180506_offspring4.data_noEmpt$Family == families[i]) #Their positions in the original data frames and list
#     if(length(index) != length(offspringInRun))
#       warning(paste('Didn\'t find offspring for family', families[i]))
#     
#     #Put phenotypes into the merged data.frame
#     NN_NC_180531_allOffspring$sleepNr[offspringInRun] <- sapply(index, function(x){NN_NC_cross180506_offspring4.speed_noEmpt_filt06.activity[[x]]$sleepNr})
#     NN_NC_180531_allOffspring$mvNr[offspringInRun] <- sapply(index, function(x){NN_NC_cross180506_offspring4.speed_noEmpt_filt06.activity[[x]]$mvNr})
#     NN_NC_180531_allOffspring$avgMvLength[offspringInRun] <- sapply(index, function(x){mean(NN_NC_cross180506_offspring4.speed_noEmpt_filt06.activity[[x]]$mvLengths)})
#     NN_NC_180531_allOffspring$avgSleepLength[offspringInRun] <- sapply(index, function(x){mean(NN_NC_cross180506_offspring4.speed_noEmpt_filt06.activity[[x]]$sleepLengths)})
#     NN_NC_180531_allOffspring$avgBoutSpeed[offspringInRun] <- sapply(index, function(x){mean(NN_NC_cross180506_offspring4.speed_noEmpt_filt06.activity[[x]]$boutSpeeds)})
#     NN_NC_180531_allOffspring$totSleep[offspringInRun] <- sapply(index, function(x){sum(NN_NC_cross180506_offspring4.speed_noEmpt_filt06.activity[[x]]$sleepLengths)})
#   }
# }
# #Put the offspring phenotypes into NN_NC_180531_allFlies
# offspring <- !NN_NC_180531_allFlies$parent
# NN_NC_180531_allFlies[!NN_NC_180531_allFlies$parent, 8:17] <- NN_NC_180531_allOffspring[, 7:16]
# 
# #Save
# save(list = c('NN_NC_180531_allOffspring', 'NN_NC_180531_allParents', 'NN_NC_180531_allFlies'), file = '180611_TrackingAllFliesSummary.RData')
# 
# save(list = c('NN_NC_cross180425_offspring1.speed_noEmpt_filt06', 'NN_NC_cross180425_offspring1.data_noEmpt', 'NN_NC_cross180425_offspring1.speed_noEmpt_filt06.activity',
#               'NN_NC_cross180425_offspring2.speed_noEmpt_filt06', 'NN_NC_cross180425_offspring2.data_noEmpt', 'NN_NC_cross180425_offspring2.speed_noEmpt_filt06.activity',
#               'NN_NC_cross180425_offspring3.speed_noEmpt_filt06', 'NN_NC_cross180425_offspring3.data_noEmpt', 'NN_NC_cross180425_offspring3.speed_noEmpt_filt06.activity',
#               'NN_NC_cross180425_offspring4.speed_noEmpt_filt06', 'NN_NC_cross180425_offspring4.data_noEmpt', 'NN_NC_cross180425_offspring4.speed_noEmpt_filt06.activity',
#               'NN_NC_cross180425_parents.speed_noEmpt_filt06', 'NN_NC_cross180425_parents.data_noEmpt', 'NN_NC_cross180425_parents.speed_noEmpt_filt06.activity'), 
#      file = '180611_NN_NCcross180425_speed.RData')
# 
# save(list = c('NN_NC_cross180503_offspring1.speed_noEmpt_filt06', 'NN_NC_cross180503_offspring1.data_noEmpt', 'NN_NC_cross180503_offspring1.speed_noEmpt_filt06.activity',
#               'NN_NC_cross180503_offspring3.speed_noEmpt_filt06', 'NN_NC_cross180503_offspring3.data_noEmpt', 'NN_NC_cross180503_offspring3.speed_noEmpt_filt06.activity',
#               'NN_NC_cross180503_offspring4.speed_noEmpt_filt06', 'NN_NC_cross180503_offspring4.data_noEmpt', 'NN_NC_cross180503_offspring4.speed_noEmpt_filt06.activity',
#               'NN_NC_cross180503_parents.speed_noEmpt_filt06', 'NN_NC_cross180503_parents.data_noEmpt', 'NN_NC_cross180503_parents.speed_noEmpt_filt06.activity'), 
#      file = '180611_NN_NCcross180503_speed.RData')
# 
# save(list = c('NN_NC_cross180506_offspring1.speed_noEmpt_filt06', 'NN_NC_cross180506_offspring1.data_noEmpt', 'NN_NC_cross180506_offspring1.speed_noEmpt_filt06.activity',
#               'NN_NC_cross180506_offspring2.speed_noEmpt_filt06', 'NN_NC_cross180506_offspring2.data_noEmpt', 'NN_NC_cross180506_offspring2.speed_noEmpt_filt06.activity',
#               'NN_NC_cross180506_offspring3.speed_noEmpt_filt06', 'NN_NC_cross180506_offspring3.data_noEmpt', 'NN_NC_cross180506_offspring3.speed_noEmpt_filt06.activity',
#               'NN_NC_cross180506_offspring4.speed_noEmpt_filt06', 'NN_NC_cross180506_offspring4.data_noEmpt', 'NN_NC_cross180506_offspring4.speed_noEmpt_filt06.activity',
#               'NN_NC_cross180506_parents.speed_noEmpt_filt06', 'NN_NC_cross180506_parents.data_noEmpt', 'NN_NC_cross180506_parents.speed_noEmpt_filt06.activity'), 
#      file = '180611_NN_NCcross180506_speed.RData')
# 
# 
# #Fix mid parent
# for(i in 1:6){
#   trait <- colnames(NN_NC_180531_allOffspring)[7:12][i]
#   y <- NN_NC_180531_allFlies[, trait]
#   
#   #Mid parent values
#   for(j in 1:length(families)){
#     parents <- which(NN_NC_180531_allFlies$parent & NN_NC_180531_allFlies$Family == families[j])
#     F1s <- which(!NN_NC_180531_allFlies$parent & NN_NC_180531_allFlies$Family == families[j])
#     if(length(parents) != 2)
#       warning(paste('Found', length(parents), 'parents , trait:', trait, ', fam:', families[j]))
#     
#     parents.col <- which(colnames(NN_NC_180531_allFlies) == trait) + 6
#     NN_NC_180531_allFlies[F1s, parents.col] <- mean(y[parents])
#   }
# }
# 
# #Regress plate
# NN_NC_180531_allFlies.plateResid <- NN_NC_180531_allFlies
# NN_NC_180531_allFlies.plateResid[1:5, 8:19] <- NA
# families <- as.character(unique(NN_NC_180531_allOffspring$Family))
# for(i in 1:6){
#   trait <- colnames(NN_NC_180531_allOffspring)[7:12][i]
#   y <- unlist(NN_NC_180531_allFlies[, trait])
#   
#   #Regress out plate
#   y.resid <- resid(lm(y ~ NN_NC_180531_allFlies$plate, na.action=na.exclude))
#   NN_NC_180531_allFlies.plateResid[, trait] <- y.resid
#   
#   #Mid parent values
#   for(j in 1:length(families)){
#     parents <- which(NN_NC_180531_allFlies$parent & NN_NC_180531_allFlies$Family == families[j])
#     F1s <- which(!NN_NC_180531_allFlies$parent & NN_NC_180531_allFlies$Family == families[j])
#     if(length(parents) != 2)
#       warning(paste('Found', length(parents), 'parents , trait:', trait, ', fam:', families[j]))
#     
#     parents.col <- which(colnames(NN_NC_180531_allFlies) == trait) + 6
#     NN_NC_180531_allFlies.plateResid[F1s, parents.col] <- mean(y.resid[parents])
#   }
# }
# 
# #Summarize effects
# NN_NC_180531_allOffspring.effects <- data.frame(rotenone.p = rep(NA, 6), rotenone.r2 = NA, sex.p = NA, sex.r2 = NA, 
#                                                 plate.p = NA, plate.r2 = NA, family.p = NA, family.r2 = NA,
#                                                 parentOff_allFlies.p = NA, parentOff_NC.p = NA, parentOff_NN.p = NA, 
#                                                 parentOff_allFlies.r2 = NA, parentOff_NC.r2 = NA, parentOff_NN.r2 = NA, 
#                                                 familyPlusRot.p = NA, familyPlusRot.r2 = NA, 
#                                                 familyTimesRot.p = NA, familyTimesRot.r2 = NA)
# rownames(NN_NC_180531_allOffspring.effects) <- colnames(NN_NC_180531_allOffspring)[7:12]
# NN_NC_180531_allOffspring.effects_plateResid <- NN_NC_180531_allOffspring.effects
# 
# offspring <- NN_NC_180531_allFlies[!NN_NC_180531_allFlies$parent, ]
# offspring.resid <- NN_NC_180531_allFlies.plateResid[!NN_NC_180531_allFlies.plateResid$parent, ]
# NC <- grep(pattern = 'NC', x = offspring$Family)
# NC <- 1:nrow(offspring) %in% NC
# NN <- grep(pattern = 'NN', x = offspring$Family)
# NN <- 1:nrow(offspring) %in% NN
# for(i in 1:6){
#   trait <- rownames(NN_NC_180531_allOffspring.effects)[i]
#   midp <- which(colnames(NN_NC_180531_allFlies) == trait) + 6
#   
#   #Raw speed
#   y <- offspring[, trait]
#   
#   model <- lm(y ~ offspring$Rotenone)
#   NN_NC_180531_allOffspring.effects[trait, "rotenone.p"] <- lmp(model)
#   NN_NC_180531_allOffspring.effects[trait, "rotenone.r2"] <- summary(model)$adj.r.squared
#   model <- lm(y ~ offspring$Male)
#   NN_NC_180531_allOffspring.effects[trait, "sex.p"] <- lmp(model)
#   NN_NC_180531_allOffspring.effects[trait, "sex.r2"] <- summary(model)$adj.r.squared
#   model <- lm(y ~ offspring$plate)
#   NN_NC_180531_allOffspring.effects[trait, "plate.p"] <- lmp(model)
#   NN_NC_180531_allOffspring.effects[trait, "plate.r2"] <- summary(model)$adj.r.squared
#   model <- lm(y ~ offspring$Family)
#   NN_NC_180531_allOffspring.effects[trait, "family.p"] <- lmp(model)
#   NN_NC_180531_allOffspring.effects[trait, "family.r2"] <- summary(model)$adj.r.squared
#   model <- lm(y ~ offspring[, midp])
#   NN_NC_180531_allOffspring.effects[trait, "parentOff_allFlies.p"] <- lmp(model)
#   NN_NC_180531_allOffspring.effects[trait, "parentOff_allFlies.r2"] <- summary(model)$adj.r.squared
#   model <- lm(y[NC] ~ offspring[NC, midp])
#   NN_NC_180531_allOffspring.effects[trait, "parentOff_NC.p"] <- lmp(model)
#   NN_NC_180531_allOffspring.effects[trait, "parentOff_NC.r2"] <- summary(model)$adj.r.squared
#   model <- lm(y[NN] ~ offspring[NN, midp])
#   NN_NC_180531_allOffspring.effects[trait, "parentOff_NN.p"] <- lmp(model)
#   NN_NC_180531_allOffspring.effects[trait, "parentOff_NN.r2"] <- summary(model)$adj.r.squared
#   model <- lm(y ~ offspring$Family + offspring$Rotenone)
#   NN_NC_180531_allOffspring.effects[trait, "familyPlusRot.p"] <- lmp(model)
#   NN_NC_180531_allOffspring.effects[trait, "familyPlusRot.r2"] <- summary(model)$adj.r.squared
#   model <- lm(y ~ offspring$Family * offspring$Rotenone)
#   NN_NC_180531_allOffspring.effects[trait, "familyTimesRot.p"] <- lmp(model)
#   NN_NC_180531_allOffspring.effects[trait, "familyTimesRot.r2"] <- summary(model)$adj.r.squared
#   
#   #Residuals
#   y <- offspring.resid[, trait]
#   
#   model <- lm(y ~ offspring.resid$Rotenone)
#   NN_NC_180531_allOffspring.effects_plateResid[trait, "rotenone.p"] <- lmp(model)
#   NN_NC_180531_allOffspring.effects_plateResid[trait, "rotenone.r2"] <- summary(model)$adj.r.squared
#   model <- lm(y ~ offspring.resid$Male)
#   NN_NC_180531_allOffspring.effects_plateResid[trait, "sex.p"] <- lmp(model)
#   NN_NC_180531_allOffspring.effects_plateResid[trait, "sex.r2"] <- summary(model)$adj.r.squared
#   model <- lm(y ~ offspring.resid$plate)
#   NN_NC_180531_allOffspring.effects_plateResid[trait, "plate.p"] <- lmp(model)
#   NN_NC_180531_allOffspring.effects_plateResid[trait, "plate.r2"] <- summary(model)$adj.r.squared
#   model <- lm(y ~ offspring.resid$Family)
#   NN_NC_180531_allOffspring.effects_plateResid[trait, "family.p"] <- lmp(model)
#   NN_NC_180531_allOffspring.effects_plateResid[trait, "family.r2"] <- summary(model)$adj.r.squared
#   model <- lm(y ~ offspring.resid[, midp])
#   NN_NC_180531_allOffspring.effects_plateResid[trait, "parentOff_allFlies.p"] <- lmp(model)
#   NN_NC_180531_allOffspring.effects_plateResid[trait, "parentOff_allFlies.r2"] <- summary(model)$adj.r.squared
#   model <- lm(y[NC] ~ offspring.resid[NC, midp])
#   NN_NC_180531_allOffspring.effects_plateResid[trait, "parentOff_NC.p"] <- lmp(model)
#   NN_NC_180531_allOffspring.effects_plateResid[trait, "parentOff_NC.r2"] <- summary(model)$adj.r.squared
#   model <- lm(y[NN] ~ offspring.resid[NN, midp])
#   NN_NC_180531_allOffspring.effects_plateResid[trait, "parentOff_NN.p"] <- lmp(model)
#   NN_NC_180531_allOffspring.effects_plateResid[trait, "parentOff_NN.r2"] <- summary(model)$adj.r.squared
#   model <- lm(y ~ offspring.resid$Family + offspring.resid$Rotenone)
#   NN_NC_180531_allOffspring.effects_plateResid[trait, "familyPlusRot.p"] <- lmp(model)
#   NN_NC_180531_allOffspring.effects_plateResid[trait, "familyPlusRot.r2"] <- summary(model)$adj.r.squared
#   model <- lm(y ~ offspring.resid$Family * offspring.resid$Rotenone)
#   NN_NC_180531_allOffspring.effects_plateResid[trait, "familyTimesRot.p"] <- lmp(model)
#   NN_NC_180531_allOffspring.effects_plateResid[trait, "familyTimesRot.r2"] <- summary(model)$adj.r.squared
# }
# save(list = c('NN_NC_180531_allOffspring.effects', 'NN_NC_180531_allOffspring.effects_plateResid'), file = '180613_phenoEffSummary.RData')
