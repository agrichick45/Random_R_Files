#load the bivariate chloropleth library
library(biscale)
library(cowplot)

#I Merge together the two median soil stock and soil uncertainty estimates
#into one file with the basin id and total area.

biData<-read_csv("ggplot2Data.csv")

#Read in the shapefile
BasinShapeFile<-st_read("reg_basin_boundaries_moirai_landcells_3p1_0p5arcmin.shp") 

#create the bivariate columns classes

data <- bi_class(biData, x = Stocksmedian, y = Errmedian, style = "quantile", dim = 3)

#Merge the two files together
shapes<-merge(BasinShapeFile, data, by="key")

#Use ggplot2 to create the bivariate chloropleth map
map <- ggplot() +
  geom_sf(data = shapes, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "DkBlue", dim = 3) +
  labs(
    title = "Median Soil Carbon Values",
  ) +
  bi_theme()

legend <- bi_legend(pal = "DkBlue",
                    dim = 3,
                    xlab = "Stocks ",
                    ylab = "Variability ",
                    size = 8)

finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0, 0, 0.2, 0.2)


#create the bivariate columns classes for IQR

data <- bi_class(biData, x = StocksQ1Q3, y = ErrQ1Q3, style = "quantile", dim = 3)

#Merge the two files together
shapes<-merge(BasinShapeFile, data, by="key")

#Use ggplot2 to create the bivariate chloropleth map
map <- ggplot() +
  geom_sf(data = shapes, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "DkBlue", dim = 3) +
  labs(
    title = "IQR Soil Carbon Values",
  ) +
  bi_theme()

legend <- bi_legend(pal = "DkBlue",
                    dim = 3,
                    xlab = "Stocks ",
                    ylab = "Variability ",
                    size = 8)

finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0, 0, 0.2, 0.2)


#create the bivariate columns classes for 5%

data <- bi_class(biData, x = StockFive, y = ErrFive, style = "quantile", dim = 3)

#Merge the two files together
shapes<-merge(BasinShapeFile, data, by="key")

#Use ggplot2 to create the bivariate chloropleth map
map <- ggplot() +
  geom_sf(data = shapes, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "DkBlue", dim = 3) +
  labs(
    title = "Five Percent Soil Carbon Values",
  ) +
  bi_theme()

legend <- bi_legend(pal = "DkBlue",
                    dim = 3,
                    xlab = "Stocks ",
                    ylab = "Variability ",
                    size = 8)

finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0, 0, 0.2, 0.2)


#create the bivariate columns classes for 95%

data <- bi_class(biData, x = StockNinetyFive, y = ErrNinetyFive, style = "quantile", dim = 3)

#Merge the two files together
shapes<-merge(BasinShapeFile, data, by="key")

#Use ggplot2 to create the bivariate chloropleth map
map <- ggplot() +
  geom_sf(data = shapes, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "DkBlue", dim = 3) +
  labs(
    title = "Ninety-Five Percent Soil Carbon Values",
  ) +
  bi_theme()

legend <- bi_legend(pal = "DkBlue",
                    dim = 3,
                    xlab = "Stocks ",
                    ylab = "Variability ",
                    size = 8)

finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0, 0, 0.2, 0.2)