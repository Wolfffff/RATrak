trak = readInfo.margo(
  rawDataFolder = "raw_data/",
  wellCount = 115,
  metadataFileName = "info.csv"
)
trak <- flies.activity(trak,noiseLevelThreshold = 3)

groupInfo = flies.group(trak, groupBy = c("male", "plate"))
groupActivity = flies.activityByGroup(trak, groups = groupInfo)
window = flies.extractTimeWindow(trak,
                                  start = 0,
                                  end = 10)
plot(extract@time,extract@area$V2)
temp = flies.extractActivity(extract)

plot.highlightBouts(
  trak,
  timeScale = 'min',
  fly = 36,
  start = 10,
  end = 25
)

window = flies.extractTimeWindow(trak, 1, 10, 'min')

plot.singleFlyDirectonality(trak,36,pch=19)

plot.singleFlyDensity(trak,36,startIndex = 1,endIndex = 1000)

plot.flyMv_allFigs(trak,"test",width=3*5*60)
