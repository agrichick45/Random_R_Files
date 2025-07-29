cropF<-read_csv("SOCDiff_Flood_cropland.csv")
forestF<-read_csv("SOCDiff_Flood_forests.csv")
grasslandF<-read_csv("SOCDiff_Flood_grassland.csv")
pastureF<-read_csv("SOCDiff_Flood_pasture.csv")
RIDF<-read_csv("SOCDiff_Flood_RID.csv")
shrublandF<-read_csv("SOCDiff_Flood_shrubland.csv")
tundraF<-read_csv("SOCDiff_Flood_tundra.csv")
urbanF<-read_csv("SOCDiff_Flood_urban.csv")
floodedF<-read_csv("SOCDiff_Flood_flooded.csv")

floodData<-rbind(cropF, floodedF, forestF, grasslandF, pastureF, RIDF, shrublandF, tundraF, urbanF)

floodData

floodData$IQR<-floodData$Q3-floodData$Q1
floodData$Fiveto95<-floodData$NinetyFive-floodData$Five

