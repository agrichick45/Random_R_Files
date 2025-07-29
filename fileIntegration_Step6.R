### Step 6: Combine all Files together

# Retrieve Summary Statistics and combine-----------
#Recall the 8 land types: 
# 1: Histosol
# 2: Aquic
# 3: Humic Soils
# 4: Gelisols
# 5: All Others


#import the csv files
histosolSS<- read.csv("SoilDiff_0.30cm_sumstat_Basin_histisol.csv")
aquicSS<- read.csv("SoilDiff_0.30cm_sumstat_Basin_aquic.csv")
humicSS<- read.csv("SoilDiff_0.30cm_sumstat_Basin_upHist.csv")
gelisolSS<- read.csv("SoilDiff_0.30cm_sumstat_Basin_gelisols.csv")
otherSoilSS<- read.csv("SoilDiff_0.30cm_sumstat_Basin_others.csv")


#combine into 1 dataset
SOC_Err_0.30SS<- rbind(histosolSS, aquicSS, humicSS, gelisolSS, otherSoilSS)

# add in IQR and 5 to 95 percentile range

SOC_Err_0.30SS$IQR<- SOC_Err_0.30SS$Q3-SOC_Err_0.30SS$Q1
SOC_Err_0.30SS$FivetoNinetyFiveRange<- SOC_Err_0.30SS$NinetyFive-SOC_Err_0.30SS$Five
SOC_Err_0.30SS$IQR.median<- (SOC_Err_0.30SSb b$IQR)/(SOC_Err_0.30SS$median)

write.csv(SOC_Err_0.30SS, "TotalCalcSpread.csv")