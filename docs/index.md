# RATrak

Utilize a variety of summarization, plotting, and general utility functions for gaining insight into time series data. Specifically designed to work with [AutoTracker](https://github.com/de-Bivort-Lab/autotracker).

To see some examples of the code in use, see the project's [resources](https://github.com/Wolfffff/RATrak) folder or examples below. 

## Structure

![](https://raw.githubusercontent.com/Wolfffff/RATrak/master/Resources/RATrak_200618.png)


## Example Figures
### GxE Box Plot
![](https://raw.githubusercontent.com/Wolfffff/RATrak/master/Resources/GxE_avgMvLength_resid.png)

###Grouped Speed

![](https://raw.githubusercontent.com/Wolfffff/RATrak/master/Resources/groupedSpeed2.png)

## Well Plot

![](https://raw.githubusercontent.com/Wolfffff/RATrak/master/Resources/24well.png)

## Installation

```R
install.packages("devtools")
library(devtools)
install_github("wolfffff/RATrak")
```

## Sample code

```R
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
```

```R
More sample code to come...
```

## Contributing

1. [Fork it](https://github.com/wolfffff/RATrak/fork)
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Add some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create new PR

