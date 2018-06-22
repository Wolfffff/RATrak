source("R/utils.R")
source("R/processing.R")
source("R/plots.R")

loadPackages(c("data.table", "zoo", "matrixStats", "ggplot2", "tidyverse"))

#Read binary data into matrix with given colCount
speed <- readBinary("/Users/Wolf/RareTrombone/Resources/05-19-2018-19-13-19_Speed.bin", 144,1, 10)

#Read manually created "info" csv containing metadata
metadata <- readMetadata("/Users/Wolf/RareTrombone/Resources/info.csv")

#Using only first 10
sex <- metadata$Male[1:10]
treatments <- metadata$Treatment[1:10]
activity <- flies.sleepActivity(speed)
plot.flyMv_allFigs(speed, activity, "Name", sex = sex, treatments = treatments)

#Group info before passing into - make sure you pass the same info into further functions
groupedInfo = flies.avgByGroup(speed)


#plot.flyMv_cumMv(speed, sex, treatments, start = 150, sampling = 100)
#plot.flyMv_cumMv(speed = groupedInfo$speed, sex = groupedInfo$sex, treatments=groupedInfo$treatments)
#plot.flyMv_rollAvg(speed,sex = sex, treatments = treatments)




