source("utils.R")
source("processing.R")
source("plots.R")

loadPackages(c("data.table", "zoo", "matrixStats", "ggplot2", "tidyverse"))

#Read binary data into matrix with given colCount
speed <- readBinary("/Users/Wolf/RareTrombone/Resources/05-19-2018-19-13-19_Speed.bin", 144, 1, 10)

#Read manually created "info" csv containing metadata
metadata <- readMetadata("/Users/Wolf/RareTrombone/Resources/info.csv")


#Using only first 10
sex <- metadata$Male[1:10]
treatments <- metadata$Treatment[1:10]


#Group info before passing into - make sure you pass the same info into further functions
groupedInfo = flies.avgByGroup(speed, sex, treatments)


#plot.flyMv_cumMv(speed, sex, treatments)
#plot.flyMv_cumMv(speed = groupedInfo$speed, sex = groupedInfo$sex, treatments=groupedInfo$treatments)

plot.flyMv_rollAvg(speed,sex = sex, treatments = treatments)


#Treatment and sex
#plot.flyMv_rollAvg_grouped(speed, sex = metadata$Male[1:10], treatments = metadata$Treatment[1:10])
#plot.flyMv_rollAvg_grouped(speed, treatments = metadata$Treatment[1:10])
#plot.flyMv_rollAvg(centroidDist, runInfo$Male, runInfo$Treatment)
#plot.flyMv_rollAvg_grouped(centroidDist, runInfo$Treatment)

#sleepActivity <- flies.sleepActivity(speed)

