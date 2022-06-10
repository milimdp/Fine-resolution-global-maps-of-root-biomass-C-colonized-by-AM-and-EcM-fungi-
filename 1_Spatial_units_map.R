rm(list=ls(all=TRUE))
library(raster)
library(rgdal)
library(dplyr)

### create raster map with spatial units ####

c <- readOGR("C:/Users/barcelom/Desktop/PAPER 4/Mycorrhizal maps/continents_NZ_mine/continents_nz.shp")
proj4string(c) <- CRS("+proj=longlat +ellps=WGS84")
c$continents <- 1:nrow(c)

#now we take the 10-min aggregated raster
glob_new_10times<-raster("C:/Users/barcelom/Desktop/PAPER 4/Ecoregions-Land Use/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7_aggregated_10times.tif")

bai <- raster("C:/Users/barcelom/Desktop/PAPER 4/Ecoregions-Land Use/wcs_int16_rc6.tif")

#create empty raster for the CCL
rtemp_10min_glob <- raster(resolution=1/6, crs="+proj=longlat +ellps=WGS84")
glob_new<-projectRaster(from=glob_new_10times, to=rtemp_10min_glob, method="ngb")
# rasterize the continents 
rtemp_10min <- raster(resolution=1/6) # this creates a blank 10 min raster
cr <- rasterize(c,rtemp_10min)
levels(cr)[[1]]

# convert aggregated raster to points
p <- rasterToPoints(bai)
baixy <- as.data.frame(p)
coordinates(baixy)= ~ x + y

# extract data at points from bailey and continents
globcover_2015 <- extract(glob_new,baixy)
continents <- extract(cr,baixy)

# add myc and continents column back to data
epts <- cbind(p,globcover_2015,continents)

final <- na.omit(right_join(as.data.frame(c),as.data.frame(epts),by="continents"))


#clean unwanted land uses (bare and urban areas, no data, croplands, sparse vegetation  and lichens and mosses)

final_clean<- subset(final, globcover_2015!=0 & globcover_2015!=10 & globcover_2015!=11 & globcover_2015!=12 & globcover_2015!=20 
                     & globcover_2015!=30 & globcover_2015!=140 & globcover_2015!=190 & globcover_2015!=200
                     & globcover_2015!=201 & globcover_2015!=202 & globcover_2015!=210 & globcover_2015!=220 & globcover_2015!=230) 

#remove water and ice from Bailey's Ecoregions

final_clean<-subset(final_clean, wcs_int16_rc6>0 & wcs_int16_rc6!=1 & wcs_int16_rc6!=2 & wcs_int16_rc6!=100 & wcs_int16_rc6!=101)


### create simplified labeling of Bailey ecoregions ####

final_clean$Bailey2 <- final_clean$wcs_int16_rc6

#Tundra division

final_clean$Bailey2[final_clean$Bailey2==16] <- 1
final_clean$Bailey2[final_clean$Bailey2==17] <- 1
final_clean$Bailey2[final_clean$Bailey2==18] <- 1
final_clean$Bailey2[final_clean$Bailey2==19] <- 1
final_clean$Bailey2[final_clean$Bailey2==20] <- 1

#Tundra division Mnt
final_clean$Bailey2[final_clean$Bailey2==21] <- 2
final_clean$Bailey2[final_clean$Bailey2==22] <- 3

#Subartic division

final_clean$Bailey2[final_clean$Bailey2==3] <- 3
final_clean$Bailey2[final_clean$Bailey2==4] <- 3
final_clean$Bailey2[final_clean$Bailey2==5] <- 3
final_clean$Bailey2[final_clean$Bailey2==6] <- 3
final_clean$Bailey2[final_clean$Bailey2==7] <- 3
final_clean$Bailey2[final_clean$Bailey2==8] <- 3
final_clean$Bailey2[final_clean$Bailey2==9] <- 3
final_clean$Bailey2[final_clean$Bailey2==10] <-3

#Subartic division Mnt

final_clean$Bailey2[final_clean$Bailey2==11] <- 4
final_clean$Bailey2[final_clean$Bailey2==12] <- 4
final_clean$Bailey2[final_clean$Bailey2==13] <- 4
final_clean$Bailey2[final_clean$Bailey2==14] <- 4
final_clean$Bailey2[final_clean$Bailey2==15] <- 4

# Warm continental division

final_clean$Bailey2[final_clean$Bailey2==47] <- 5
final_clean$Bailey2[final_clean$Bailey2==48] <- 5

# Warm continental division Mnt

final_clean$Bailey2[final_clean$Bailey2==49] <- 6
final_clean$Bailey2[final_clean$Bailey2==50] <- 6
final_clean$Bailey2[final_clean$Bailey2==51] <- 6

# Hot continental division

final_clean$Bailey2[final_clean$Bailey2==23] <- 7
final_clean$Bailey2[final_clean$Bailey2==24] <- 7

# Hot continental division Mnt

final_clean$Bailey2[final_clean$Bailey2==25] <- 8

#Subtropical division

final_clean$Bailey2[final_clean$Bailey2==44] <- 9

#Subtropical division Mnt

final_clean$Bailey2[final_clean$Bailey2==45] <- 10
final_clean$Bailey2[final_clean$Bailey2==46] <- 10

#Marine division

final_clean$Bailey2[final_clean$Bailey2==26] <- 11
final_clean$Bailey2[final_clean$Bailey2==27] <- 11
final_clean$Bailey2[final_clean$Bailey2==28] <- 11
final_clean$Bailey2[final_clean$Bailey2==29] <- 11

#Marine division Mnt

final_clean$Bailey2[final_clean$Bailey2==30] <- 12
final_clean$Bailey2[final_clean$Bailey2==31] <- 12
final_clean$Bailey2[final_clean$Bailey2==32] <- 12

#Prairie division

final_clean$Bailey2[final_clean$Bailey2==37] <- 13
final_clean$Bailey2[final_clean$Bailey2==38] <- 13
final_clean$Bailey2[final_clean$Bailey2==39] <- 13
final_clean$Bailey2[final_clean$Bailey2==40] <- 13
final_clean$Bailey2[final_clean$Bailey2==41] <- 13
final_clean$Bailey2[final_clean$Bailey2==140] <- 13

#Prairie division Mnt

final_clean$Bailey2[final_clean$Bailey2==42] <- 14
final_clean$Bailey2[final_clean$Bailey2==43] <- 14

#Mediterranean division

final_clean$Bailey2[final_clean$Bailey2==33] <- 15
final_clean$Bailey2[final_clean$Bailey2==34] <- 15
final_clean$Bailey2[final_clean$Bailey2==133] <- 15

#Mediterranean division Mnt

final_clean$Bailey2[final_clean$Bailey2==35] <- 16
final_clean$Bailey2[final_clean$Bailey2==36] <- 16
final_clean$Bailey2[final_clean$Bailey2==135] <- 16

#Tropical/subtropical steppe division

final_clean$Bailey2[final_clean$Bailey2==73] <- 17
final_clean$Bailey2[final_clean$Bailey2==74] <- 17
final_clean$Bailey2[final_clean$Bailey2==75] <- 17
final_clean$Bailey2[final_clean$Bailey2==76] <- 17
final_clean$Bailey2[final_clean$Bailey2==77] <- 17
final_clean$Bailey2[final_clean$Bailey2==175] <- 17

#Tropical/subtropical steppe division Mnt

final_clean$Bailey2[final_clean$Bailey2==78] <- 18
final_clean$Bailey2[final_clean$Bailey2==79] <- 18
final_clean$Bailey2[final_clean$Bailey2==80] <- 18
final_clean$Bailey2[final_clean$Bailey2==81] <- 18
final_clean$Bailey2[final_clean$Bailey2==179] <- 18

#Tropical/subtropical desert division

final_clean$Bailey2[final_clean$Bailey2==63] <- 19
final_clean$Bailey2[final_clean$Bailey2==64] <- 19
final_clean$Bailey2[final_clean$Bailey2==65] <- 19
final_clean$Bailey2[final_clean$Bailey2==66] <- 19
final_clean$Bailey2[final_clean$Bailey2==67] <- 19
final_clean$Bailey2[final_clean$Bailey2==68] <- 19

#Tropical/subtropical desert division

final_clean$Bailey2[final_clean$Bailey2==69] <- 20
final_clean$Bailey2[final_clean$Bailey2==70] <- 20
final_clean$Bailey2[final_clean$Bailey2==71] <- 20
final_clean$Bailey2[final_clean$Bailey2==72] <- 20


#Temperate steppe division

final_clean$Bailey2[final_clean$Bailey2==58] <- 21
final_clean$Bailey2[final_clean$Bailey2==59] <- 21
final_clean$Bailey2[final_clean$Bailey2==60] <- 21

#Temperate steppe division Mnt

final_clean$Bailey2[final_clean$Bailey2==61] <- 22
final_clean$Bailey2[final_clean$Bailey2==62] <- 22

# TEmperate desert division

final_clean$Bailey2[final_clean$Bailey2==52] <- 23
final_clean$Bailey2[final_clean$Bailey2==53] <- 23
final_clean$Bailey2[final_clean$Bailey2==54] <- 23
final_clean$Bailey2[final_clean$Bailey2==55] <- 23
final_clean$Bailey2[final_clean$Bailey2==56] <- 23

# Temperate desert division Mnt

final_clean$Bailey2[final_clean$Bailey2==57] <- 24

#Savanna division

final_clean$Bailey2[final_clean$Bailey2==89] <- 25
final_clean$Bailey2[final_clean$Bailey2==90] <- 25
final_clean$Bailey2[final_clean$Bailey2==91] <- 25
final_clean$Bailey2[final_clean$Bailey2==92] <- 25
final_clean$Bailey2[final_clean$Bailey2==93] <- 25
final_clean$Bailey2[final_clean$Bailey2==94] <- 25
final_clean$Bailey2[final_clean$Bailey2==192] <-25
final_clean$Bailey2[final_clean$Bailey2==292] <- 25
final_clean$Bailey2[final_clean$Bailey2==392] <- 25
final_clean$Bailey2[final_clean$Bailey2==492] <- 25
final_clean$Bailey2[final_clean$Bailey2==592] <- 25
final_clean$Bailey2[final_clean$Bailey2==692] <- 25

#Savanna division Mnt

final_clean$Bailey2[final_clean$Bailey2==95] <- 26
final_clean$Bailey2[final_clean$Bailey2==96] <- 26
final_clean$Bailey2[final_clean$Bailey2==97] <- 26
final_clean$Bailey2[final_clean$Bailey2==195] <- 26
final_clean$Bailey2[final_clean$Bailey2==196] <- 26
final_clean$Bailey2[final_clean$Bailey2==197] <- 26
final_clean$Bailey2[final_clean$Bailey2==295] <- 26
final_clean$Bailey2[final_clean$Bailey2==297] <- 26
final_clean$Bailey2[final_clean$Bailey2==397] <- 26
final_clean$Bailey2[final_clean$Bailey2==497] <- 26
final_clean$Bailey2[final_clean$Bailey2==597] <- 26

#Rainforest division

final_clean$Bailey2[final_clean$Bailey2==82] <- 27
final_clean$Bailey2[final_clean$Bailey2==83] <- 27
final_clean$Bailey2[final_clean$Bailey2==84] <- 27
final_clean$Bailey2[final_clean$Bailey2==85] <- 27
final_clean$Bailey2[final_clean$Bailey2==182] <- 27
final_clean$Bailey2[final_clean$Bailey2==184] <- 27

#Rainforest division Mnt

final_clean$Bailey2[final_clean$Bailey2==86] <- 28
final_clean$Bailey2[final_clean$Bailey2==87] <- 28
final_clean$Bailey2[final_clean$Bailey2==88] <- 28
final_clean$Bailey2[final_clean$Bailey2==188] <- 28
final_clean$Bailey2[final_clean$Bailey2==288] <- 28
final_clean$Bailey2[final_clean$Bailey2==388] <- 28


### create simplified labeling of Land cover uses ####

final_clean$globcover_2015.2 <- final_clean$globcover_2015

final_clean$globcover_2015.2[final_clean$globcover_2015.2==121] <- 120
final_clean$globcover_2015.2[final_clean$globcover_2015.2==122] <- 120
final_clean$globcover_2015.2[final_clean$globcover_2015.2==81] <- 80
final_clean$globcover_2015.2[final_clean$globcover_2015.2==82] <- 80
final_clean$globcover_2015.2[final_clean$globcover_2015.2==61] <- 60
final_clean$globcover_2015.2[final_clean$globcover_2015.2==62] <- 60
final_clean$globcover_2015.2[final_clean$globcover_2015.2==71] <- 70
final_clean$globcover_2015.2[final_clean$globcover_2015.2==72] <- 70
final_clean$globcover_2015.2[final_clean$globcover_2015.2==151] <- 150
final_clean$globcover_2015.2[final_clean$globcover_2015.2==152] <- 150
final_clean$globcover_2015.2[final_clean$globcover_2015.2==153] <- 150


final_clean$id2 <- paste0(final_clean$Bailey2, sep="_", final_clean$globcover_2015.2, sep="_", final_clean$continents)


### #create unic numeric ID for each combination of continent, ecoregion and land cover ####

final_clean$numeric_id2 <- as.numeric(factor(final_clean$id2 , levels=unique(final_clean$id2)))

#number of distinct spatial units
#length(unique(final_clean$numeric_id2))

#write.csv(final_clean, "C:/Users/barcelom/Desktop/PAPER 4/id_raster/final_clean_reduced_Bailey&Land_cover.csv")

# create raster with numeric_id

coordinates(final_clean)<- ~x+y

rtemp_10min <- raster(resolution=1/6) # this creates a blank 10 min raster
id_raster2 <- rasterize(final_clean, rtemp_10min, field="numeric_id2")
plot(id_raster2)

#writeRaster(id_raster2,overwrite=TRUE, "C:/Users/barcelom/Desktop/PAPER 4/id_raster/final_clean_reduced_Bailey&Land_cover.tif")