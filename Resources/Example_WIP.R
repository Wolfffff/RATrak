trak <- readInfo("Resources/05-19-2018-19-13-19_Speed.bin", "Resources/info.csv", 144,1,48)
trak.avg <- flies.avgByGroup(trak)
plot.flyMv_allFigs(trak.avg, "Example", avgMv = T, noMv = T)