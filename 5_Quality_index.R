### Add colinzation data

##### AM #####

colonization_DB_AM<- read.delim("C:/Users/barcelom/Desktop/PAPER 4/Colonization database/colonization_database_clean_AM.txt",
                                header = TRUE, sep = "\t", na.strings = c("", "NA"))

### create mean of AM colonization per species ###

colonization_species_list_AM<- as.data.frame(colonization_DB_AM[,c(9,13)])
colnames(colonization_species_list_AM)[[1]]<-"species"

Length_AM_colinization_species<-aggregate(colonization_species_list_AM[[2]], list(colonization_species_list_AM[[1]]), length)
colnames(Length_AM_colinization_species)[[2]]<-"number_occurrences_colonization"
Mean_AM_colinization_species<-cbind(aggregate(colonization_species_list_AM[[2]], list(colonization_species_list_AM[[1]]), mean),
                                    Length_AM_colinization_species)
colnames(Mean_AM_colinization_species)[[2]]<-"AM_colonization_mean"
colnames(Mean_AM_colinization_species)[[1]]<-"Species"
Mean_AM_colinization_species[[3]]<-NULL

### subset species with more than 20 records

Species_list_AM<-subset(Mean_AM_colinization_species, number_occurrences_colonization>20)
Species_list_AM<-as.vector(Species_list_AM[[1]])


# Create a list for different plant species

n <- length(Species_list_AM)
list_species<- vector("list", n)

#Create empty dataframe to store results

MeanDF_AM<- matrix(0, ncol = n, nrow = 54) 

MeanDF_AM<- data.frame(MeanDF_AM)

## 
for(i in 1:length(list_species)){

list_species[[i]] <- filter(colonization_DB_AM, species %in% Species_list_AM[i])

DF<-as.data.frame(list_species[[i]])
DF2<-as.data.frame(DF[[13]])

  for (j in 1:nrow(DF2)){
  MeanDF_AM[j,i]<-mean(DF2[sample(nrow(DF2), j), ])}
}

#change name of colums by the names of the files

colnames(MeanDF_AM)<-Species_list_AM

#change 0 for NA

MeanDF_AM[MeanDF_AM == 0] <- NA


write.csv(MeanDF_AM, "C:/Users/barcelom/Desktop/PAPER 4/Uncertainty/MeanDF_AM.csv")



##### EcM #####

colonization_DB_EcM<- read.delim("C:/Users/barcelom/Desktop/PAPER 4/Colonization database/colonization_database_clean_EcM.txt",
                                 header = TRUE, sep = "\t", na.strings = c("", "NA"))
root_tips<- read.delim("C:/Users/barcelom/Desktop/PAPER 4/Root_tips/Root_tips_mg_plant.txt",
                       header = TRUE, sep = "\t")

#join colinization ecm and root tips

colonizatio_EcM<-join(colonization_DB_EcM,root_tips)

### create mean of EcM colonization per species ###

colonization_species_list_EcM<- as.data.frame(colonization_DB_EcM[,c(9,16)])
colnames(colonization_species_list_EcM)[[1]]<-"species"

Length_EcM_colinization_species<-aggregate(colonization_species_list_EcM[[2]], list(colonization_species_list_EcM[[1]]), length)
colnames(Length_EcM_colinization_species)[[2]]<-"number_occurrences_colonization"
Mean_EcM_colinization_species<-cbind(aggregate(colonization_species_list_EcM[[2]], list(colonization_species_list_EcM[[1]]), mean),
                                     Length_EcM_colinization_species)
colnames(Mean_EcM_colinization_species)[[2]]<-"EcM_colonization_mean"
colnames(Mean_EcM_colinization_species)[[1]]<-"Species"
Mean_EcM_colinization_species[[3]]<-NULL

### subset species with more than 14 record

Species_list_EcM<-subset(Mean_EcM_colinization_species, number_occurrences_colonization>14)
Species_list_EcM<-as.vector(Species_list_EcM[[1]])

colonization_DB_EcM_filetered <- filter(colonization_DB_EcM, species %in% Species_list_EcM)

###### MAPS OF UNCERTENTIES SOURCES #####

## read coordinates

C_roots_myco_fractions_col_total<-read.csv("C:/Users/barcelom/Desktop/PAPER 4/FINAL/C_roots_myco_fractions_col_total_reduced_Bailey&Land_cover.csv")

C_roots_myco_fractions_col_total.naomit<-subset(C_roots_myco_fractions_col_total, numeric_id2!="NA")

## read uncertanties info ##

AM<-read.csv("C:/Users/barcelom/Desktop/PAPER 4/Splot/ID_AM_Col_total.csv")[,c(2:8)]

EcM<-read.csv("C:/Users/barcelom/Desktop/PAPER 4/Splot/ID_EcM_Col_total.csv")[,c(2:8)]

## join cordinates and uncertainty info##


AM_ID<-join(C_roots_myco_fractions_col_total.naomit[,c(2,3,15)], AM, by=("numeric_id2"))

EcM_ID<-join(C_roots_myco_fractions_col_total.naomit[,c(2,3,15)], EcM, by=("numeric_id2"))

###### MAP final raster ####

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

### number of occurrence in colnization databse ###

AM_map_num_occurrences <- 
  base_world_messy+
  scale_color_viridis(direction = -1, option = "magma",
                      na.value = "Lightgrey",
                      limits = c(0,30),
                      breaks = c(20, 10, 0),
                      guide = "colourbar", guide_legend(title = "AM"), 
                      labels = c(20, 10, 0))+
  rasterise(geom_point(data=AM_ID, 
                       aes(x=x, y=y, colour=AM_number_occurrences_colonization), 
                       pch=15, size=0.1),dpi=300) + 
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.border = element_rect(colour = "black", fill=NA))

plot(AM_map_num_occurrences)

AM_number_occurrences_colonization<- AM_ID[complete.cases(AM_ID$AM_number_occurrences_colonization), ]
coordinates(AM_number_occurrences_colonization)<-~x+y
rtemp_10min <- raster(resolution=1/6) # this creates a blank 10 min raster
AM_number_occurrences_colonization_raster<- raster::rasterize(AM_number_occurrences_colonization, rtemp_10min, field="AM_number_occurrences_colonization")
#writeRaster(AM_number_occurrences_colonization_raster, "C:/Users/barcelom/Desktop/PAPER 4/Submission_files/Final_rasters/AM_occurrences_colonization.tiff", overwrite=TRUE)


EcM_map_num_occurrences <- 
  base_world_messy+
  scale_color_viridis(direction = -1, option = "magma",
                      na.value = "Lightgrey",
                      limits = c(0,30),
                      breaks = c(20, 10, 0),
                      guide = "colourbar", guide_legend(title = "EcM"), 
                      labels = c(20, 10, 0))+
  rasterise(geom_point(data=EcM_ID, 
                       aes(x=x, y=y, colour=EcM_number_occurrences_colonization), 
                       pch=15, size=0.1),dpi=300) + 
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.border = element_rect(colour = "black", fill=NA))

plot(EcM_map_num_occurrences)

EcM_number_occurrences_colonization<- EcM_ID[complete.cases(EcM_ID$EcM_number_occurrences_colonization), ]
coordinates(EcM_number_occurrences_colonization)<-~x+y
rtemp_10min <- raster(resolution=1/6) # this creates a blank 10 min raster
EcM_number_occurrences_colonization_raster<- raster::rasterize(EcM_number_occurrences_colonization, rtemp_10min, field="EcM_number_occurrences_colonization")
#writeRaster(EcM_number_occurrences_colonization_raster, "C:/Users/barcelom/Desktop/PAPER 4/Submission_files/Final_rasters/EcM_occurrences_colonization.tiff", overwrite=TRUE)


### number of relative abundance covered with colonization data ###

AM_map_relative_cover <- 
  base_world_messy+
  scale_color_viridis(direction = -1, option = "cividis",
                      na.value = "Lightgrey",
                      breaks = c( 0.8, 0.6, 0.4, 0.2, 0),
                      guide = "colourbar", guide_legend(title = "AM"), 
                      labels = c(0.8, 0.6, 0.4, 0.2, 0))+
  rasterise(geom_point(data=AM_ID, 
                       aes(x=x, y=y, colour=AM_Relative_cover), 
                       pch=15, size=0.1),dpi=300) + 
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.border = element_rect(colour = "black", fill=NA))

plot(AM_map_relative_cover)

AM_Relative_cover<- AM_ID[complete.cases(AM_ID$AM_Relative_cover), ]
coordinates(AM_Relative_cover)<-~x+y
rtemp_10min <- raster(resolution=1/6) # this creates a blank 10 min raster
AM_Relative_cover_raster<- raster::rasterize(AM_Relative_cover, rtemp_10min, field="AM_Relative_cover")
#writeRaster(AM_Relative_cover_raster, "C:/Users/barcelom/Desktop/PAPER 4/Submission_files/Final_rasters/AM_rel.abundance_colonization.tiff", overwrite=TRUE)


EcM_map_relative_cover <- 
  base_world_messy+
  scale_color_viridis(direction = -1, option = "cividis",
                      na.value = "Lightgrey",
                      breaks = c(0.8, 0.6, 0.4, 0.2, 0),
                      guide = "colourbar", guide_legend(title = "EcM"), 
                      labels = c(0.8, 0.6, 0.4, 0.2, 0))+
  rasterise(geom_point(data=EcM_ID, 
                       aes(x=x, y=y, colour=EcM_Relative_cover), 
                       pch=15, size=0.1),dpi=300) + 
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.border = element_rect(colour = "black", fill=NA))

plot(EcM_map_relative_cover)

EcM_Relative_cover<- EcM_ID[complete.cases(EcM_ID$EcM_Relative_cover), ]
coordinates(EcM_Relative_cover)<-~x+y
rtemp_10min <- raster(resolution=1/6) # this creates a blank 10 min raster
EcM_Relative_cover_raster<- raster::rasterize(EcM_Relative_cover, rtemp_10min, field="EcM_Relative_cover")
#writeRaster(EcM_Relative_cover_raster, "C:/Users/barcelom/Desktop/PAPER 4/Submission_files/Final_rasters/EcM_rel.abundance_colonization.tiff", overwrite=TRUE)


### number of Splots ###

AM_map_number_Splots <- 
  base_world_messy+
  scale_color_viridis(direction = -1, option = "mako",
                      na.value = "Lightgrey",
                      limits = c(0,2500),
                      breaks = c(2000, 1500, 1000, 500, 0),
                      guide = "colourbar", guide_legend(title = "AM"), 
                      labels = c(2000, 1500, 1000, 500, 0))+
  rasterise(geom_point(data=AM_ID, 
                       aes(x=x, y=y, colour=number_Splots), 
                       pch=15, size=0.1),dpi=300) + 
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.border = element_rect(colour = "black", fill=NA))

plot(AM_map_number_Splots)

AM_number_Splots<- AM_ID[complete.cases(AM_ID$number_Splots), ]
coordinates(AM_number_Splots)<-~x+y
rtemp_10min <- raster(resolution=1/6) # this creates a blank 10 min raster
AM_number_Splots_raster<- raster::rasterize(AM_number_Splots, rtemp_10min, field="number_Splots")
#writeRaster(AM_number_occurrences_colonization, "C:/Users/barcelom/Desktop/PAPER 4/Submission_files/Final_rasters/AM_plots.tiff", overwrite=TRUE)


EcM_map_number_Splots <- 
  base_world_messy+
  scale_color_viridis(direction = -1, option = "mako",
                      na.value = "Lightgrey",
                      limits = c(0,2500),
                      breaks = c(2000, 1500, 1000, 500, 0),
                      guide = "colourbar", guide_legend(title = "EcM"), 
                      labels = c(2000, 1500, 1000, 500, 0))+
  rasterise(geom_point(data=EcM_ID, 
                       aes(x=x, y=y, colour=number_Splots), 
                       pch=15, size=0.1),dpi=300) + 
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.border = element_rect(colour = "black", fill=NA))

plot(EcM_map_number_Splots)

EcM_number_Splots<- EcM_ID[complete.cases(EcM_ID$number_Splots), ]
coordinates(EcM_number_Splots)<-~x+y
rtemp_10min <- raster(resolution=1/6) # this creates a blank 10 min raster
EcM_number_Splots_raster<- raster::rasterize(EcM_number_Splots, rtemp_10min, field="number_Splots")
#writeRaster(EcM_number_occurrences_colonization, "C:/Users/barcelom/Desktop/PAPER 4/Submission_files/Final_rasters/EcM_plots.tiff", overwrite=TRUE)

