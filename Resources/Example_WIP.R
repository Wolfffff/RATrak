##
### Example 1
##

#Read binary data into matrix with given colCount - only take in column 1:10
#speed <- readBinary("/Users/Wolf/RareTrombone/Resources/05-19-2018-19-13-19_Speed.bin", 144,1,24)

#Read manually created "info" csv containing metadata
#metadata <- readMetadata("info.csv")

#Using only first 10
#sex <- metadata$Male[1:24]
#treatments <- metadata$Treatment[1:24]
#activity = flies.sleepActivity(speed)


plot.flyMovement_plate(speed,4,6)


