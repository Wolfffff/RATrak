source("utils.R")
source("processing.R")
source("plots.R")

loadPackages(c("data.table", "zoo", "matrixStats", "ggplot2", "tidyverse"))

#Read manually created "info" csv containing 
runInfo <- read.csv("/Users/Wolf/RareTrombone/Resources/info.csv", sep = ',',header = TRUE)

#Read binary data into matrix with given colCount
centroidDist <- readBinary("/Users/Wolf/RareTrombone/Resources/05-19-2018-19-13-19_Speed.bin", 144, 1, 10)
sleepActivity <- flies.sleepActivity(centroidDist)
plot.flyMv_cumMv(centroidDist, runInfo$Male[1:10], runInfo$Treatment[1:10])
#plot.flyMv_rollAvg(centroidDist, runInfo$Male, runInfo$Treatment)
#plot.flyMv_rollAvg_grouped(centroidDist, runInfo$Treatment)



