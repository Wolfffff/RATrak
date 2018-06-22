##
### Example 1
##

#Read binary data into matrix with given colCount - only take in column 1:10
speed <- readBinary("05-19-2018-19-13-19_Speed.bin", 144,1, 10)

#Read manually created "info" csv containing metadata
metadata <- readMetadata("info.csv")

#Using only first 10
sex <- metadata$Male[1:10]
treatments <- metadata$Treatment[1:10]

#Grouping
groupedInfo = flies.avgByGroup(speed, sex, treatments)

#Calculating activity
activity <- flies.sleepActivity(groupedInfo$speed)

#Plotting all
plot.flyMv_allFigs(speed, activity, "Name", sex = groupedInfo$sex, treatments = groupedInfo$treatments, noMv= T)


##
### Example 2
##

