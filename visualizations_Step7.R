#Step 7: Visualizations

# Summary Stats for select basins------------------

#Subset, removing all small areas
SOC_0.30SS_Sub<-SOC_Err_0.30SS %>%
  filter(PerArea >= 0.005)

SOC_0.30SS_sample<- SOC_Err_0.30SS%>%filter(basin_nm == "Lower_Mississippi_River_Basin" | basin_nm == "Scheldt")

hydrictypeorder<- c("Histosols", "Aquic", "Humic", "Gelisols", "Others")
hydrictypecolororder<- c("Green", "Blue", "Purple", "Red", "Yellow") 

ggplot(SOC_0.30SS_sample)+
  geom_dumbbell(aes(y= `SoilType`, x=minimum, xend= maximum), color= "gray", size=1)+
  geom_dumbbell(aes(y= `SoilType`, x=Five, xend=NinetyFive), color= "#495057", size=1.2)+
  geom_point(aes(y= `SoilType`, x=median), size=1.5, color = "red")+
  geom_point(aes(y= `SoilType`, x=Q1), size=1, color = "blue")+
  geom_point(aes(y= `SoilType`, x=Q3), size=1, color = "blue")+
  theme(axis.text.y= element_text(size= 10))+facet_wrap(~basin_nm)+theme_minimal()+
  labs(x= "Soil Organic Carbon Stock Error, tC/ha", y= "Soil Type")






ggplot(SOC_0.30SS_sample)+
  geom_dumbbell(aes(y= `SoilType`, x=minimum, xend= maximum), color= "#adb5bd", size=1)+
  geom_dumbbell(aes(y= `SoilType`, x=Five, xend=NinetyFive, color = factor(SoilType, levels=hydrictypeorder)), 
                size=1.75)+scale_color_manual(values=hydrictypecolororder)+
  geom_point(aes(y= `SoilType`, x=median), size=1.5, color = "red")+
  geom_point(aes(y= `SoilType`, x=Q1), size=3, color = "blue", shape= "|")+
  geom_point(aes(y= `SoilType`, x=Q3), size=3, color = "blue", shape= "|")+
  theme(axis.text.y= element_text(size= 10))+facet_wrap(~basin_nm)+theme_minimal()+
  labs(x= "Soil Organic Carbon Stock 0-30cm, tC/ha", y= "Soil Type")+
  theme(legend.position = "none")


ggplot(SOC_0.30SS_Sub)+
  geom_dumbbell(aes(y= `SoilType`, x=minimum, xend= maximum), color= "#adb5bd", size=1)+
  geom_dumbbell(aes(y= `SoilType`, x=Five, xend=NinetyFive, color = factor(SoilType, levels=hydrictypeorder)), 
                size=1.75)+scale_color_manual(values=hydrictypecolororder)+
  geom_point(aes(y= `SoilType`, x=median), size=1.5, color = "red")+
  geom_point(aes(y= `SoilType`, x=Q1), size=3, color = "blue", shape= "|")+
  geom_point(aes(y= `SoilType`, x=Q3), size=3, color = "blue", shape= "|")+
  theme(axis.text.y= element_text(size= 10))+facet_wrap(~basin_nm)+theme_minimal()+
  labs(x= "Soil Organic Carbon Stock Error 0-30cm, tC/ha", y= "Soil Type")+
  theme(legend.position = "none")


# Summary Stats for Globe-------------------

ggplot(SOC_0.30SS_Sub, aes(y= `SoilType`, x= median))+geom_boxplot()
ggplot(SOC_0.30SS_Sub, aes(y= `SoilType`, x= IQR))+geom_boxplot()
ggplot(SOC_0.30SS_Sub, aes(y= `SoilType`, x= FivetoNinetyFiveRange))+geom_boxplot()
ggplot(SOC_0.30SS_Sub, aes(y= `SoilType`, x= IQR.median))+geom_boxplot()


ggplot(SOC_0.30SS_Sub%>%filter(reg_nm == "Indonesia"))+
  #geom_dumbbell(aes(y= `SoilType`, x=minimum, xend= maximum), color= "#adb5bd", size=1)+
  geom_dumbbell(aes(y= `SoilType`, x=Five, xend=NinetyFive, color = factor(SoilType, levels=hydrictypeorder)), 
                size=1.75)+scale_color_manual(values=hydrictypecolororder)+
  geom_point(aes(y= `SoilType`, x=median), size=1.5, color = "red")+
  geom_point(aes(y= `SoilType`, x=Q1), size=3, color = "blue", shape= "|")+
  geom_point(aes(y= `SoilType`, x=Q3), size=3, color = "blue", shape= "|")+
  theme(axis.text.y= element_text(size= 10))+facet_wrap(~basin_nm)+theme_bw()+
  labs(x= "Soil Organic Carbon Stock Error, tC/ha", y= "Soil Type")+
  theme(legend.position = "none")





## Scatterplots-------------

ggplot(SOC_Err_0.30SS)+
  geom_point(aes(x=median, y=IQR))+
  theme_dark()+
  labs(x= "Median SOC value, tC/ha", y= "Interquartile Range", title= "IQR v Median")+
  geom_smooth(aes(x=median, y=IQR), method="lm")+
  stat_regline_equation(aes(x=median, y=IQR), label.x= 60, label.y=60)+
  stat_cor(aes(x=median, y=IQR, label=..rr.label..), label.x=60, label.y=50)

### By Land Type-------------------
## Median v IQR

ggplot(SOC_0.30SS)+
  geom_point(aes(x=median, y=IQR, color= factor(LandType, levels=landtypeorder)))+
  scale_color_manual(values=landtypecolororder)+theme_dark()+
  labs(x= "Median SOC value, tC/ha", y= "Interquartile Range", title= "IQR v Median by Land Type", color= "Land Type")+
  theme(legend.position = "right")+
  geom_smooth(aes(x=median, y=IQR), method="lm")+
  stat_regline_equation(aes(x=median, y=IQR), label.x= 60, label.y=60)+
  stat_cor(aes(x=median, y=IQR, label=..rr.label..), label.x=60, label.y=50)

ggplot(SOC_0.30SS)+
  geom_point(aes(x=median, y=IQR, color= factor(LandType, levels=landtypeorder)))+
  scale_color_manual(values=landtypecolororder)+theme_dark()+facet_wrap(~LandType, nrow=2)+
  labs(x= "Median SOC value, tC/ha", y= "Interquartile Range", title= "IQR v Median by Land Type")+
  theme(legend.position = "none")+
  geom_smooth(aes(x=median, y=IQR), method="lm")+
  stat_regline_equation(aes(x=median, y=IQR), label.x= 40, label.y=60)+
  stat_cor(aes(x=median, y=IQR, label=..rr.label..), label.x=40, label.y=50)

## Median v Five to 95 Percentile Range
ggplot(SOC_0.30SS)+
  geom_point(aes(x=median, y=FivetoNinetyFiveRange, color= factor(LandType, levels=landtypeorder)))+
  scale_color_manual(values=landtypecolororder)+theme_dark()+
  labs(x= "Median SOC value, tC/ha", y= "95-5 Percentile", title= "Five to NinetyFive Range v Median by Land Type", color= "Land Type")+
  theme(legend.position = "right")+
  geom_smooth(aes(x=median, y=FivetoNinetyFiveRange), method="lm")+
  stat_regline_equation(aes(x=median, y=FivetoNinetyFiveRange), label.x= 60, label.y=100)+
  stat_cor(aes(x=median, y=FivetoNinetyFiveRange, label=..rr.label..), label.x=60, label.y=90)

ggplot(SOC_0.30SS)+
  geom_point(aes(x=median, y=FivetoNinetyFiveRange , color= factor(LandType, levels=landtypeorder)))+
  scale_color_manual(values=landtypecolororder)+theme_dark()+facet_wrap(~LandType, nrow=2)+
  labs(x= "Median SOC value, tC/ha", y= "95-5 Percentile", title= "Five to NinetyFive Range v Median by Land Type")+
  theme(legend.position = "none")+
  geom_smooth(aes(x=median, y=FivetoNinetyFiveRange), method="lm")+
  stat_regline_equation(aes(x=median, y=FivetoNinetyFiveRange), label.x= 60, label.y=100)+
  stat_cor(aes(x=median, y=FivetoNinetyFiveRange, label=..rr.label..), label.x=60, label.y=90)

## IQR v Five to Ninety Five
ggplot(SOC_0.30SS)+
  geom_point(aes(x=IQR, y=FivetoNinetyFiveRange, color= factor(LandType, levels=landtypeorder)))+
  scale_color_manual(values=landtypecolororder)+theme_dark()+
  labs(x= "IQR SOC value, tC/ha", y= "95-5 Percentile", title= "Five to NinetyFive Range v IQR by Land Type", color= "Land Type")+
  theme(legend.position = "right")+
  geom_smooth(aes(x=IQR, y=FivetoNinetyFiveRange), method="lm")+
  stat_regline_equation(aes(x=IQR, y=FivetoNinetyFiveRange), label.x= 60, label.y=100)+
  stat_cor(aes(x=IQR, y=FivetoNinetyFiveRange, label=..rr.label..), label.x=60, label.y=90)

ggplot(SOC_0.30SS)+
  geom_point(aes(x=IQR, y=FivetoNinetyFiveRange , color= factor(LandType, levels=landtypeorder)))+
  scale_color_manual(values=landtypecolororder)+theme_dark()+facet_wrap(~LandType, nrow=2)+
  labs(x= "IQR SOC value, tC/ha", y= "95-5 Percentile", title= "Five to NinetyFive Range v IQR by Land Type")+
  theme(legend.position = "none")+
  geom_smooth(aes(x=IQR, y=FivetoNinetyFiveRange), method="lm")+
  stat_regline_equation(aes(x=IQR, y=FivetoNinetyFiveRange), label.x= 30, label.y=100)+
  stat_cor(aes(x=IQR, y=FivetoNinetyFiveRange, label=..rr.label..), label.x=30, label.y=90)



### By Region---------

WesternHemisphere<- c("USA", "Brazil", "Mexico", "Canada", "Central America and Caribbean", 
                      "South America_Northern", "South America_Southern", "Argentina", "Colombia")
WHColor<- c("#1E0052", "#390099", "#6c0079", "#9e0059", "#FF0054", "#FF5400", "#FF8900", "#FFBD00", "#FFE499")

## Median v IQR
ggplot(SOC_0.30SS%>%filter(reg_nm %in% WesternHemisphere))+
  geom_point(aes(x=median, y=IQR, color= factor(reg_nm)))+
  scale_color_manual(values=WHColor)+theme_dark()+
  labs(x= "Median SOC value, tC/ha", y= "Interquartile Range", title= "IQR v Median by Region", 
       color= "Region")+theme(legend.position = "right")+
  geom_smooth(aes(x=median, y=IQR), method="lm")+
  stat_regline_equation(aes(x=median, y=IQR), label.x= 60, label.y=60)+
  stat_cor(aes(x=median, y=IQR, label=..rr.label..), label.x=60, label.y=50)

ggplot(SOC_0.30SS%>%filter(reg_nm %in% WesternHemisphere))+
  geom_point(aes(x=median, y=IQR, color= factor(reg_nm)))+
  scale_color_manual(values=WHColor)+theme_dark()+facet_wrap(~reg_nm, nrow=3)+
  labs(x= "Median SOC value, tC/ha", y= "Interquartile Range", title= "IQR v Median by Region", 
       color= "Region")+theme(legend.position = "none")+
  geom_smooth(aes(x=median, y=IQR), method="lm")+
  stat_regline_equation(aes(x=median, y=IQR), label.x= 60, label.y=60)+
  stat_cor(aes(x=median, y=IQR, label=..rr.label..), label.x=60, label.y=50)

## Median v Five to 95
ggplot(SOC_0.30SS%>%filter(reg_nm %in% WesternHemisphere))+
  geom_point(aes(x=median, y=FivetoNinetyFiveRange , color= factor(reg_nm)))+
  scale_color_manual(values=WHColor)+theme_dark()+
  labs(x= "Median SOC value, tC/ha", y= "95-5 Percentile", title= "Five to NinetyFive Range v Median by Land Type",
       color= "Region")+theme(legend.position = "right")+
  geom_smooth(aes(x=median, y=FivetoNinetyFiveRange), method="lm")+
  stat_regline_equation(aes(x=median, y=FivetoNinetyFiveRange), label.x= 60, label.y=110)+
  stat_cor(aes(x=median, y=FivetoNinetyFiveRange, label=..rr.label..), label.x=60, label.y=100)

ggplot(SOC_0.30SS%>%filter(reg_nm %in% WesternHemisphere))+
  geom_point(aes(x=median, y=FivetoNinetyFiveRange , color= factor(reg_nm)))+
  scale_color_manual(values=WHColor)+theme_dark()+facet_wrap(~reg_nm, nrow=3)+
  labs(x= "Median SOC value, tC/ha", y= "95-5 Percentile", title= "Five to NinetyFive Range v Median by Land Type",
       color= "Region")+theme(legend.position = "none")+
  geom_smooth(aes(x=median, y=FivetoNinetyFiveRange), method="lm")+
  stat_regline_equation(aes(x=median, y=FivetoNinetyFiveRange), label.x= 60, label.y=60)+
  stat_cor(aes(x=median, y=FivetoNinetyFiveRange, label=..rr.label..), label.x=60, label.y=50)


### By Region & Color by Land Type---------

WesternHemisphere<- c("USA", "Brazil", "Mexico", "Canada", "Central America and Caribbean", 
                      "South America_Northern", "South America_Southern", "Argentina", "Colombia")

## Median v IQR
ggplot(SOC_0.30SS_Sub%>%filter(reg_nm %in% WesternHemisphere))+
  geom_point(aes(x=median, y=IQR, color= factor(SoilType, levels=hystictypeorder)))+
  scale_color_manual(values=soiltypecolororder)+theme_dark()+facet_wrap(~reg_nm, nrow=3)+
  labs(x= "Median SOC value, tC/ha", y= "Interquartile Range", title= "IQR v Median by Land Type", 
       color= "Land Type")+geom_smooth(aes(x=median, y=IQR), method="lm")+
  geom_smooth(aes(x=median, y=IQR), method="lm")+
  stat_regline_equation(aes(x=median, y=IQR), label.x= 50, label.y=60)+
  stat_cor(aes(x=median, y=IQR, label=..rr.label..), label.x=50, label.y=50)

## Median v Five to 95 Percentile Range
ggplot(SOC_0.30SS%>%filter(reg_nm %in% WesternHemisphere))+
  geom_point(aes(x=median, y=FivetoNinetyFiveRange , color= factor(LandType, levels=landtypeorder)))+
  scale_color_manual(values=landtypecolororder)+theme_dark()+facet_wrap(~reg_nm, nrow=3)+
  labs(x= "Median SOC value, tC/ha", y= "95-5 Percentile", title= "Five to NinetyFive Range v Median by Land Type",
       color= "Land Type")+
  geom_smooth(aes(x=median, y=FivetoNinetyFiveRange), method="lm")+
  stat_regline_equation(aes(x=median, y=FivetoNinetyFiveRange), label.x= 50, label.y=60)+
  stat_cor(aes(x=median, y=FivetoNinetyFiveRange, label=..rr.label..), label.x=50, label.y=50)

## IQR v Five to Ninety Five
ggplot(SOC_0.30SS%>%filter(reg_nm %in% WesternHemisphere))+
  geom_point(aes(x=IQR, y=FivetoNinetyFiveRange , color= factor(LandType, levels=landtypeorder)))+
  scale_color_manual(values=landtypecolororder)+theme_dark()+facet_wrap(~reg_nm, nrow=3)+
  labs(x= "IQR SOC value, tC/ha", y= "95-5 Percentile", title= "Five to NinetyFive Range v IQR by Land Type", 
       color= "Land Type")+
  geom_smooth(aes(x=IQR, y=FivetoNinetyFiveRange), method="lm")+
  stat_regline_equation(aes(x=IQR, y=FivetoNinetyFiveRange), label.x= 40, label.y=60)+
  stat_cor(aes(x=IQR, y=FivetoNinetyFiveRange, label=..rr.label..), label.x=40, label.y=50)

### By Land Type, Color by Region---------

WesternHemisphere<- c("USA", "Brazil", "Mexico", "Canada", "Central America and Caribbean", 
                      "South America_Northern", "South America_Southern", "Argentina", "Colombia")

## Median v IQR
ggplot(SOC_0.30SS_Sub%>%filter(reg_nm %in% WesternHemisphere))+
  geom_point(aes(x=median, y=IQR, color= factor(reg_nm)))+
  scale_color_manual(values=WHColor)+theme_dark()+facet_wrap(~LandType, nrow=2)+
  labs(x= "Median SOC value, tC/ha", y= "Interquartile Range", title= "IQR v Median by Land Type", 
       color= "Region")+geom_smooth(aes(x=median, y=IQR), method="lm")+
  geom_smooth(aes(x=median, y=IQR), method="lm")+
  stat_regline_equation(aes(x=median, y=IQR), label.x= 50, label.y=60)+
  stat_cor(aes(x=median, y=IQR, label=..rr.label..), label.x=50, label.y=50)

## Median v Five to 95 Percentile Range
ggplot(SOC_0.30SS%>%filter(reg_nm %in% WesternHemisphere))+
  geom_point(aes(x=median, y=FivetoNinetyFiveRange , color= factor(reg_nm)))+
  scale_color_manual(values=WHColor)+theme_dark()+facet_wrap(~LandType, nrow=2)+
  labs(x= "Median SOC value, tC/ha", y= "95-5 Percentile", title= "Five to NinetyFive Range v Median by Land Type",
       color= "Region")+
  geom_smooth(aes(x=median, y=FivetoNinetyFiveRange), method="lm")+
  stat_regline_equation(aes(x=median, y=FivetoNinetyFiveRange), label.x= 12, label.y=80)+
  stat_cor(aes(x=median, y=FivetoNinetyFiveRange, label=..rr.label..), label.x=12, label.y=70)

## IQR v Five to Ninety Five
ggplot(SOC_0.30SS%>%filter(reg_nm %in% WesternHemisphere))+
  geom_point(aes(x=IQR, y=FivetoNinetyFiveRange , color= factor(reg_nm)))+
  scale_color_manual(values=WHColor)+theme_dark()+facet_wrap(~LandType, nrow=2)+
  labs(x= "IQR SOC value, tC/ha", y= "95-5 Percentile", title= "Five to NinetyFive Range v IQR by Land Type", 
       color= "Region")+
  geom_smooth(aes(x=IQR, y=FivetoNinetyFiveRange), method="lm")+
  stat_regline_equation(aes(x=IQR, y=FivetoNinetyFiveRange), label.x= 30, label.y=25)+
  stat_cor(aes(x=IQR, y=FivetoNinetyFiveRange, label=..rr.label..), label.x=30, label.y=15)

## Facet Grid----------------


ggplot(SOC_0.30SS_Sub%>%filter(reg_nm %in% WesternHemisphere))+
  geom_point(aes(x=median, y=IQR, color= factor(reg_nm)))+
  scale_color_manual(values=WHColor)+theme_dark()+facet_grid(LandType~reg_nm)+
  labs(x= "Median SOC value, tC/ha", y= "IQR", title= "Interquartile Range v Median by Land Type & Region",
       subtitle= "Western Hemisphere", caption= "Based on Analysis of SoilGrid 0-30 2020 SOC data with the ESA-Ramankutty-MODIS land use raster", 
       color= "Region")+
  geom_smooth(aes(x=median, y=IQR), method="lm")+
  stat_regline_equation(aes(x=median, y=IQR), label.x= 35, label.y=60)+
  stat_cor(aes(x=median, y=IQR, label=..rr.label..), label.x=35, label.y=45)+
  ylim(c(0, 75))+theme(legend.position = "none")

ggplot(SOC_0.30SS%>%filter(reg_nm %in% WesternHemisphere))+
  geom_point(aes(x=median, y=FivetoNinetyFiveRange, color= factor(reg_nm)))+
  scale_color_manual(values=WHColor)+theme_dark()+facet_grid(LandType~reg_nm)+
  labs(x= "Median SOC value, tC/ha", y= "95-5 Percentile", title= "Five to NinetyFive Range v Median by Land Type & Region", 
       subtitle= "Western Hemisphere", caption= "Based on Analysis of SoilGrid 0-30 2020 SOC data with the ESA-Ramankutty-MODIS land use raster", 
       color= "Region")+geom_smooth(aes(x=median, y=FivetoNinetyFiveRange), method="lm")+
  stat_regline_equation(aes(x=median, y=FivetoNinetyFiveRange), label.x= 35, label.y=60)+
  stat_cor(aes(x=median, y=FivetoNinetyFiveRange, label=..rr.label..), label.x=35, label.y=45)+
  ylim(c(0, 75))+theme(legend.position = "none")
