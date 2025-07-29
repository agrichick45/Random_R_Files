library(terra)
library(tidyverse)


#Load soil taxonomy raster

raster <- rast("TAXOUSDA_250m_ll.tif")

#read in the csv file for classification

baseTable <-read_csv("TAXOUSDA_250m_ll.csv")

#Drop the group and Description tables
baseTable$Group<-NULL
baseTable$Description<-NULL

#Simplify Taxonomy
tax_simple<-classify(raster, baseTable, othersNA=TRUE)

plot(tax_simple)

#save raster
writeRaster(tax_simple, "wetland_coldsoils.tif")


