rm(list=ls(all=TRUE))

library(raster)
library(rgdal)
library(ggplot2)
library(ggrastr)
library(maps)
library(viridis)
library(plyr)#join
library(colorspace)
library(paletteer)

C_roots_myco_fractions_col_total<-read.csv("C:/Users/barcelom/Desktop/PAPER 4/FINAL/C_roots_myco_fractions_col_total_reduced_Bailey&Land_cover.csv")

C_roots_myco_fractions_col_total.naomit<-subset(C_roots_myco_fractions_col_total, numeric_id2!="NA")

AM<-C_roots_myco_fractions_col_total.naomit[,c(2,3,37)]

EcM<-C_roots_myco_fractions_col_total.naomit[,c(2,3,42)]

## get coastline map ##

continents<- readOGR("C:/Users/barcelom/Downloads/gaul2006_coast/gaul2006_coast.shp")

continents@data$id <- seq(1:nrow(continents@data))

# in order to plot polygons, first fortify the data
continents@data$id <- rownames(continents@data)
# create a data.frame from our spatial object
continentsdata <- fortify(continents)
# merge the "fortified" data with the data from our spatial object
continentsdf <- merge(continentsdata, continents@data,
                   by = "id")

continentsdf_clean<-subset(continentsdf, hole=="FALSE")

## PLOT MAP ###

p <- ggplot() + coord_fixed() +
  xlab("") + ylab("")

base_world_messy <- p + geom_polygon(data = continentsdf_clean, aes(x = long, y = lat , group = id), 
                                     colour="lightgrey", fill="white", size=0.1)



AM_map <- 
  base_world_messy+
  scale_color_viridis(direction = -1, option = "D",
                      na.value = "azure4",
                      breaks = c(1.5,1,0.5, 0),
                      guide = "colourbar", guide_legend(title = "AM"), 
                      labels = c(1.5,1,0.5, 0))+
  rasterise(geom_point(data=AM, 
                       aes(x=x, y=y, colour=final_AM_biomass), 
                       pch=15, size=0.1), dpi=300)  + 
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.border = element_rect(colour = "black", fill=NA))

plot(AM_map)


AM_NA<- C_roots_myco_fractions_col_total.naomit[is.na(C_roots_myco_fractions_col_total.naomit$final_AM_biomass),]

AM_mapNA <- 
  base_world_messy+
  scale_color_continuous_sequential(palette = "Blues 3",
                                    na.value = "azure4",
                                    limits=c(0,1.5),
                                    breaks=c(0,1,1.5))+
  rasterise(geom_point(data=AM_NA, 
                       aes(x=x, y=y, colour=final_AM_biomass), 
                       pch=15, size=0.1), dpi=300)  + 
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.border = element_rect(colour = "black", fill=NA))

plot(AM_mapNA)

EcM_map <- 
  base_world_messy+
  scale_color_viridis(direction = -1, option = "D",
                      na.value = "azure4",
                      breaks = c(0,1.5,2.5,3.5),
                      guide = "colourbar", guide_legend(title = "EcM"), 
                      labels = c(0,1.5,2.5,3.5))+
  rasterise(geom_point(data=EcM, 
                       aes(x=x, y=y, colour=final_EcM_biomass), 
                       pch=15, size=0.1), dpi=300)  + 
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.border = element_rect(colour = "black", fill=NA))

plot(EcM_map)

EcM_NA<- C_roots_myco_fractions_col_total.naomit[is.na(C_roots_myco_fractions_col_total.naomit$final_EcM_biomass),]

EcM_mapNA <- 
  base_world_messy+
  scale_color_continuous_sequential(palette = "Greens 3",
                                    na.value = "azure4",
                                    limits=c(0,1.5),
                                    breaks=c(0,1,1.5))+
  rasterise(geom_point(data=EcM_NA, 
                       aes(x=x, y=y, colour=final_EcM_biomass), 
                       pch=15, size=0.1), dpi=300)  + 
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.border = element_rect(colour = "black", fill=NA))

plot(EcM_mapNA)
