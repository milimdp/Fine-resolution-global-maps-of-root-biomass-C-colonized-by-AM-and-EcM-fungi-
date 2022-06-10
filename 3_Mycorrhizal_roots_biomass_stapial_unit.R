rm(list=ls(all=TRUE))
library(plyr)#join
library(dplyr)
library(raster)

#### GET MYCO FRACTIONS OF EACH GROWTH FORM  USING REDUCED BAILEY ECOREGIONS #####

Myco_fractions<-read.csv("C:/Users/barcelom/Desktop/PAPER 4/Mycorrhizal maps/pft_myco_fractions.csv")[,c(4:8,10:13,24,26,28,30:41)]

ID<-read.csv("C:/Users/barcelom/Desktop/PAPER 4/id_raster/final_clean_reduced_BAiley&Land_cover.csv")

colnames(Myco_fractions)[[4]]<-"wcs_int16_rc6"

Myco_fractions<-subset(Myco_fractions, continents>0)

Myco_fractions2<-join(Myco_fractions,ID[!duplicated(ID$wcs_int16_rc6),c(6,8)], by="wcs_int16_rc6")

Myco_fractions2<-join(Myco_fractions2,ID[!duplicated(ID$globcover_2015),c(7,9)], by="globcover_2015")

Myco_fractions2$globcover_2015.2<-paste0(Myco_fractions2$globcover_2015.2, "A")
Myco_fractions2$continents<-paste0(Myco_fractions2$continents, "B")
Myco_fractions2$Bailey2<-paste0(Myco_fractions2$Bailey2, "C")

Myco_fractions2$id2 <- as.factor(paste0(Myco_fractions2$Bailey2, Myco_fractions2$globcover_2015.2, Myco_fractions2$continents))

### join with spatial units ID ###

ID$globcover_2015.2<-paste0(ID$globcover_2015.2, "A")
ID$continents<-paste0(ID$continents, "B")
ID$Bailey2<-paste0(ID$Bailey2, "C")

ID$id2 <- as.factor(paste0(ID$Bailey2, ID$globcover_2015.2, ID$continents))

Myco_fractions_ID<-join(Myco_fractions2, ID[!duplicated(ID$id2),c(10,11)], by=("id2"))


############################################################
####### GET ROOT BIOMASS PER MYCO TYPE PER GROWTH FORM ######

#### HERBS ####

#C_roots_herbs<-raster("C:/Users/barcelom/Desktop/PAPER 4/Global_Maps_C_Density/harmonized/grass_bgbc.tif")

#C_roots_herbs_agg<-aggregate(C_roots_herbs, 60, fun=mean, na.rm=TRUE)

#writeRaster(C_roots_herbs_agg, "C:/Users/barcelom/Desktop/PAPER 4/Global_Maps_C_Density/harmonized/C_roots_herbs_aggregated.tif")

C_roots_herbs_agg<-raster("C:/Users/barcelom/Desktop/PAPER 4/Global_Maps_C_Density/harmonized/C_roots_herbs_aggregated.tif")

#exctract values in coordinates from myco maps

Myco_fractions_ID_xy<-Myco_fractions_ID[,c(2,3)]

coordinates(Myco_fractions_ID_xy)<-  ~x+ y 

extract_herbs<-as.data.frame(extract(C_roots_herbs_agg, Myco_fractions_ID_xy))

C_roots_myco_herbs<-as.data.frame(cbind(Myco_fractions_ID,extract_herbs))

colnames(C_roots_myco_herbs)[[29]]<-"Herbs_root_C"

## get mean in each ID ##

C_roots_myco_herbs_ID<-aggregate(Herbs_root_C ~ numeric_id2, C_roots_myco_herbs, function(x) mean(x))

#### WOODY ###

#C_roots_woody<-raster("C:/Users/barcelom/Desktop/PAPER 4/Global_Maps_C_Density/harmonized/woody_bgbc.tif")

#C_roots_woody_agg<-aggregate(C_roots_woody, 60, fun=mean, na.rm=TRUE)

#writeRaster(C_roots_woody_agg, "C:/Users/barcelom/Desktop/PAPER 4/Global_Maps_C_Density/harmonized/C_roots_woody_aggregated.tif",overwrite=TRUE)

C_roots_woody_agg<-raster("C:/Users/barcelom/Desktop/PAPER 4/Global_Maps_C_Density/harmonized/C_roots_woody_aggregated.tif")

#exctract values in coordinates from myco maps

extract_woody<-as.data.frame(extract(C_roots_woody_agg, Myco_fractions_ID_xy))

C_roots_myco_woody<-as.data.frame(cbind(Myco_fractions_ID,extract_woody))

colnames(C_roots_myco_woody)[[29]]<-"woody_root_C"

## get mean in each growth form ##

C_roots_myco_woody_ID<-aggregate(woody_root_C ~ numeric_id2, C_roots_myco_woody, function(x) mean(x))

#### TUNDRA ###

#C_roots_tundra<-raster("C:/Users/barcelom/Desktop/PAPER 4/Global_Maps_C_Density/harmonized/tundra_bgbc.tif")

#C_roots_tundra_agg<-aggregate(C_roots_tundra, 60, fun=mean, na.rm=TRUE)

#writeRaster(C_roots_tundra_agg, "C:/Users/barcelom/Desktop/PAPER 4/Global_Maps_C_Density/harmonized/C_roots_tundra_aggregated.tif")

C_roots_tundra_agg<-raster("C:/Users/barcelom/Desktop/PAPER 4/Global_Maps_C_Density/harmonized/C_roots_tundra_aggregated.tif")


#exctract values in coordinates from myco maps

extract_tundra<-as.data.frame(extract(C_roots_tundra_agg, Myco_fractions_ID_xy))

C_roots_myco_tundra<-as.data.frame(cbind(Myco_fractions_ID,extract_tundra))

colnames(C_roots_myco_tundra)[[29]]<-"tundra_root_C"

## get mean in each growth form ##

C_roots_myco_tundra_ID<-aggregate(tundra_root_C ~ numeric_id2, C_roots_myco_tundra, function(x) mean(x))

### join woody and herbs

C_roots_myco_ID<-cbind(C_roots_myco_herbs_ID, C_roots_myco_woody_ID[[2]], C_roots_myco_tundra_ID[[2]])



###########################################################
################# GET ROOTS COLONIZED #####################


AM_col_woody<- read.csv("C:/Users/barcelom/Desktop/PAPER 4/Splot/ID_AM_Col_woody_reduced_Bailey&Land_cover.csv")
AM_col_herb<- read.csv("C:/Users/barcelom/Desktop/PAPER 4/Splot/ID_AM_Col_herbs_reduced_Bailey&Land_cover.csv")

EcM_col_woody<- read.csv("C:/Users/barcelom/Desktop/PAPER 4/Splot/ID_EcM_Col_woody_reduced_Bailey&Land_cover.csv")
EcM_col_herb<- read.csv("C:/Users/barcelom/Desktop/PAPER 4/Splot/ID_EcM_Col_herb_reduced_Bailey&Land_cover.csv")

#join myco fractions root biomass and colonization data

C_roots_myco_fractions<-join(Myco_fractions_ID[, c(2,3,6,7,10:15,22:24,28)], C_roots_myco_ID,by="numeric_id2")


C_roots_myco_fractions_col1<-join(C_roots_myco_fractions, AM_col_woody[,c(2:5)] ,by="numeric_id2")
C_roots_myco_fractions_col2<-join(C_roots_myco_fractions_col1, EcM_col_woody[,c(2:5)] ,by="numeric_id2")
C_roots_myco_fractions_col3<-join(C_roots_myco_fractions_col2, AM_col_herb[,c(2:5)] ,by="numeric_id2")
C_roots_myco_fractions_col_total<-join(C_roots_myco_fractions_col3, EcM_col_herb[,c(2:5)] ,by="numeric_id2")
colnames(C_roots_myco_fractions_col_total)[[16]]<-"Woody_root_C"
colnames(C_roots_myco_fractions_col_total)[[17]]<-"tundra_root_C"

#calculate fractions of AM woody vs AM hebs

C_roots_myco_fractions_col_total$AM_woody_relative<-((C_roots_myco_fractions_col_total$AMt*C_roots_myco_fractions_col_total$trees_value) + 
                                                    (C_roots_myco_fractions_col_total$AMs*C_roots_myco_fractions_col_total$shrubs_value))/
                                                      (C_roots_myco_fractions_col_total$trees_value+C_roots_myco_fractions_col_total$shrubs_value)

C_roots_myco_fractions_col_total$EcM_woody_relative<-(C_roots_myco_fractions_col_total$EMt*C_roots_myco_fractions_col_total$trees_value) + 
                                                     (C_roots_myco_fractions_col_total$EMs*C_roots_myco_fractions_col_total$shrubs_value)/
                                                    (C_roots_myco_fractions_col_total$trees_value+C_roots_myco_fractions_col_total$shrubs_value)

#### create final output ####

###CONSTANTS##
#cumulative root fraction first 30CM  (according to Jackson et al 1996)
#HERBS = 77.1%
#WOODY = 54.6%
#coarse vs fine root 14,1% woody; 88,5% hebs (according to FRED)

#calculate fine root biomass C stored in firts 30Cm in each spatial unit per growth form

### AM ##
C_roots_myco_fractions_col_total$final_AM_biomass_herbs<-C_roots_myco_fractions_col_total$Herbs_root_C*0.1*
  C_roots_myco_fractions_col_total$AMh*0.771*0.885*
  (C_roots_myco_fractions_col_total$AM_herbs_colonization_mean/100)
C_roots_myco_fractions_col_total$final_AM_biomass_woody<-C_roots_myco_fractions_col_total$Woody_root_C*0.1*
  C_roots_myco_fractions_col_total$AM_woody_relative*0.546*0.141*(C_roots_myco_fractions_col_total$AM_woody_colonization_mean/100)

  #tundra 

AM<-read.csv("C:/Users/barcelom/Desktop/PAPER 4/Splot/ID_AM_Col_total.csv")[,c(2:8)]

C_roots_myco_fractions_col_total<-join(C_roots_myco_fractions_col_total, AM[,c(1,2)], by="numeric_id2")

C_roots_myco_fractions_col_total$final_AM_biomass_tundra<-C_roots_myco_fractions_col_total$tundra_root_C*0.1*
  (C_roots_myco_fractions_col_total$am/100)*0.771*0.885*
  (C_roots_myco_fractions_col_total$AM_colonization_mean/100)

C_roots_myco_fractions_col_total$final_AM_biomass<-rowSums(C_roots_myco_fractions_col_total[,c("final_AM_biomass_herbs", "final_AM_biomass_woody", "final_AM_biomass_tundra")],
                                                           na.rm = TRUE) * NA ^ (rowSums(!is.na(C_roots_myco_fractions_col_total[,c("final_AM_biomass_herbs", "final_AM_biomass_woody", "final_AM_biomass_tundra")])) == 0)

### EcM ###

C_roots_myco_fractions_col_total$final_EcM_biomass_herbs<-C_roots_myco_fractions_col_total$Herbs_root_C*0.1*
  C_roots_myco_fractions_col_total$EMh*0.771*0.885*
  (C_roots_myco_fractions_col_total$EcM_herbs_colonization_mean/100)
C_roots_myco_fractions_col_total$final_EcM_biomass_woody<-C_roots_myco_fractions_col_total$Woody_root_C*0.1*
  C_roots_myco_fractions_col_total$EcM_woody_relative*0.546*0.141*(C_roots_myco_fractions_col_total$EcM_woody_colonization_mean/100)


## tundra
EcM<-read.csv("C:/Users/barcelom/Desktop/PAPER 4/Splot/ID_EcM_Col_total.csv")[,c(2:8)]

C_roots_myco_fractions_col_total<-join(C_roots_myco_fractions_col_total, EcM[,c(1,2)], by="numeric_id2")

C_roots_myco_fractions_col_total$final_EcM_biomass_tundra<-C_roots_myco_fractions_col_total$tundra_root_C*0.1*
  (C_roots_myco_fractions_col_total$ecm/100)*0.771*0.885*
  (C_roots_myco_fractions_col_total$EcM_colonization_mean/100)

C_roots_myco_fractions_col_total$final_EcM_biomass<-rowSums(C_roots_myco_fractions_col_total[,c("final_EcM_biomass_herbs","final_EcM_biomass_woody", "final_EcM_biomass_tundra")],
                                                            na.rm = TRUE) * NA ^ (rowSums(!is.na(C_roots_myco_fractions_col_total[,c("final_EcM_biomass_herbs", "final_EcM_biomass_herbs", "final_EcM_biomass_woody", "final_EcM_biomass_tundra")])) == 0)


#write.csv(C_roots_myco_fractions_col_total,"C:/Users/barcelom/Desktop/PAPER 4/FINAL/C_roots_myco_fractions_col_total_reduced_Bailey&Land_cover.csv" )

