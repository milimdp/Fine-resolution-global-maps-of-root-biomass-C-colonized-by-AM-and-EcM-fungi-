### sPlot ####
rm(list=ls(all=TRUE))
library(dplyr)
library(data.table)
library(plyr)#join
library("plotrix")#standard error function 
options(scipen = 999) #avoid exponential notation

id_raster2 <- raster("C:/Users/barcelom/Desktop/PAPER 4/id_raster/final_clean_reduced_Bailey&Land_cover.tif")

#load sPlot data

load("C:/Users/barcelom/Desktop/PAPER 4/Splot/sPlot_data/sPlotOpen.RData(2)/sPlotOpen.RData")

#filter first resampled itineration

coordinates(header.oa)<- ~Longitude+Latitude

sPlot_records<-as.data.frame(extract(id_raster2, header.oa))

sPlot_records_final <-cbind(as.data.frame(sPlot_records),as.data.frame(header.oa))

colnames(sPlot_records_final)[1]<-"numeric_id2"

sPlot_records_final<-subset(sPlot_records_final, numeric_id2>0)

length(unique(sPlot_records_final$numeric_id2))

#summary

# count number of distinct plots in each spatial unit

DT <- data.table(sPlot_records_final)

num_plots<-DT[, .(number_of_distinct_plots = length(unique(PlotObservationID))), by = numeric_id2]

#### ADD mycorrhizal type##

myco_type<-read.table("C:/Users/barcelom/Desktop/PAPER 4/Colonization database/Genera_myco_type.txt", header=TRUE, sep="\t")

# get genera

DT2.oa$Genus<- gsub("([A-Za-z]+).*", "\\1", DT2.oa$Species)

#join

DT2.oa_myco_type<- join(DT2.oa, myco_type, by="Genus")

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

#join Splot data

DT2.oa_myco_type_colonization2<- join(DT2.oa_myco_type, Mean_AM_colinization_species, by="Species")

DT2.oa_myco_type_colonization<- join(DT2.oa_myco_type_colonization2, Mean_EcM_colinization_species, by="Species")


###join growth fortms

growth_form<- read.csv("C:/Users/barcelom/Desktop/PAPER 4/Growth forms/TRY/species_growth_form_FINAL_woody_vs_herbs.csv", header = TRUE)
colnames(growth_form)[[2]]<-"Species"

#growth_form$Corrected[growth_form$Corrected=="climber"] <- "shrub" 
#growth_form$Corrected[growth_form$Corrected=="fern"] <- "herb" 

DT2.oa_myco_type_colonz_GrwFrm<-join(DT2.oa_myco_type_colonization, growth_form, by="Species")

### join spatial units

DT2.oa_myco_type_colonization_id<- join(DT2.oa_myco_type_colonz_GrwFrm, sPlot_records_final)


DT2<-subset(DT2.oa_myco_type_colonization_id, Resample_1!="NA")

DT2_reduced<-DT2[,-c(3,4,5,7,13,15:16,18:23,26:57)]

#### SUMARY ####

#separate myco type

AM1<- subset(DT2_reduced, Mycorrhizal.type=="AM")

AM_NM<- subset(DT2_reduced, Mycorrhizal.type=="NM-AM")

AM_EcM<-subset(DT2_reduced, Mycorrhizal.type=="EcM-AM")
AM_EcM$Relative_cover<-AM_EcM$Relative_cover/2

AM<-rbind(AM1, AM_NM, AM_EcM)

EcM1<- subset(DT2_reduced, Mycorrhizal.type=="EcM")

EcM<-rbind(AM_EcM, EcM1)

#relative cover each mycotype

#AM

rel.cov_AM<- aggregate(Relative_cover~ PlotObservationID , AM, function(x) sum(x))

AM2<-subset(AM,AM_colonization_mean!="NA")

rel.cov_col_AM<- aggregate(Relative_cover~ PlotObservationID , AM2, function(x) sum(x))

rel.cov_col_AM_total<- join(rel.cov_col_AM, rel.cov_AM, by="PlotObservationID")

colnames(rel.cov_col_AM_total)[[2]]<-"Rel.cov.AM.Col"
colnames(rel.cov_col_AM_total)[[3]]<-"Rel.cov.AM.total"

rel.cov_col_AM_total$Rel.cov.AM.relative<-rel.cov_col_AM_total$Rel.cov.AM.Col/rel.cov_col_AM_total$Rel.cov.AM.total

## separate per growth form

AM_herb<-subset(AM2, Corrected=="herb")

rel.cov_col_AM_herb<- aggregate(Relative_cover~ PlotObservationID , AM_herb, function(x) sum(x))

AM_woody<-subset(AM2, Corrected=="woody")

rel.cov_col_AM_woody<- aggregate(Relative_cover~ PlotObservationID , AM_woody, function(x) sum(x))

growth_form1<-join(rel.cov_col_AM_total, rel.cov_col_AM_herb, by="PlotObservationID")
growth_form_AM<-join(growth_form1, rel.cov_col_AM_woody, by="PlotObservationID")

growth_form_AM[is.na(growth_form_AM)] <- 0

growth_form_AM$sum<-(growth_form_AM[[5]]+growth_form_AM[[6]])/growth_form_AM[[2]]
colnames(growth_form_AM)[[5]]<-"Relative_cover_herb_AM"
colnames(growth_form_AM)[[6]]<-"Relative_cover_woody_AM"

#EcM

rel.cov_EcM<- aggregate(Relative_cover~ PlotObservationID, EcM, function(x) sum(x))

EcM2<-subset(EcM,EcM_colonization_mean!="NA")

rel.cov_col_EcM<- aggregate(Relative_cover~ PlotObservationID , EcM2, function(x) sum(x))

rel.cov_col_EcM_total<- join(rel.cov_col_EcM, rel.cov_EcM, by="PlotObservationID")

colnames(rel.cov_col_EcM_total)[[2]]<-"Rel.cov.EcM.Col"
colnames(rel.cov_col_EcM_total)[[3]]<-"Rel.cov.EcM.total"

rel.cov_col_EcM_total$Rel.cov.EcM.relative<-rel.cov_col_EcM_total$Rel.cov.EcM.Col/rel.cov_col_EcM_total$Rel.cov.EcM.total

## separate per growth form

#maje only woody vs non woody

EcM_herb<-subset(EcM2, Corrected=="herb")

rel.cov_col_EcM_herb<- aggregate(Relative_cover~ PlotObservationID , EcM_herb, function(x) sum(x))

EcM_woody<-subset(EcM2, Corrected=="woody")

rel.cov_col_EcM_woody<- aggregate(Relative_cover~ PlotObservationID , EcM_woody, function(x) sum(x))

growth_form1<-join(rel.cov_col_EcM_total, rel.cov_col_EcM_herb, by="PlotObservationID")
growth_form_EcM<-join(growth_form1, rel.cov_col_EcM_woody, by="PlotObservationID")

growth_form_EcM[is.na(growth_form_EcM)] <- 0

growth_form_EcM$sum<-(growth_form_EcM[[5]]+growth_form_EcM[[6]])/growth_form_EcM[[2]]
colnames(growth_form_EcM)[[5]]<-"Relative_cover_herb_EcM"
colnames(growth_form_EcM)[[6]]<-"Relative_cover_woody_EcM"

### join relatide cover in each myco type and Splot info

splot_records_colonization<- join (sPlot_records_final, rel.cov_col_AM_total, by="PlotObservationID")

splot_records_colonization2<- join (splot_records_colonization, rel.cov_col_EcM_total, by="PlotObservationID")

splot_records_colonization_final<- splot_records_colonization2[,-c(3,4,8,11:44)]

# reduce plots using resampling within each spatial unit and  myco type

#AM

splot_records_colonization_final_na.omit_AM<- subset(splot_records_colonization_final, Rel.cov.AM.relative!="NA")

splot_records_colonization_final_na.omit_resample_AM<-subset(splot_records_colonization_final_na.omit_AM, Resample_1_consensus=="TRUE") # resample_1_consesus has the highest number of spatial units

#length(unique(splot_records_colonization_final_na.omit_AM$numeric_id2))

# get info of spatial units that where excluded from resample_1_consensus

spatial_unit_complete_AM<- unique(as.data.frame(splot_records_colonization_final_na.omit_AM[[1]]))
colnames(spatial_unit_complete_AM)<-"numeric_id2"

spatial_unit_resample_AM<- unique(as.data.frame(splot_records_colonization_final_na.omit_resample_AM[[1]]))
colnames(spatial_unit_resample_AM)<-"numeric_id2"
spatial_unit_resample_AM$T<-"TRUE"

join_AM<- merge( spatial_unit_complete_AM, spatial_unit_resample_AM, all.x=TRUE)

join_AM[is.na(join_AM)] <- 1

SU_AM<- subset(join_AM, T==1)

SU2_AM<- join(SU_AM, splot_records_colonization_final_na.omit_AM, by="numeric_id2")
SU2_AM[[2]]<-NULL

spatial_unit_resample_completeAM<-rbind(splot_records_colonization_final_na.omit_resample_AM, SU2_AM)

#write.csv(spatial_unit_resample_completeAM, "C:/Users/barcelom/Desktop/PAPER 4/Splot/AM_relative_cover.csv")

#ECM

splot_records_colonization_final_na.omit_EcM<- subset(splot_records_colonization_final, Rel.cov.EcM.relative!="NA")

splot_records_colonization_final_na.omit_resample_EcM<-subset(splot_records_colonization_final_na.omit_EcM, Resample_3=="TRUE") # resample_3 has the highest number of spatial units

#length(unique(splot_records_colonization_final_na.omit_EcM$numeric_id2))

# get info of spatial units that where excluded from resample_1_consensus

spatial_unit_complete_EcM<- unique(as.data.frame(splot_records_colonization_final_na.omit_EcM[[1]]))
colnames(spatial_unit_complete_EcM)<-"numeric_id2"

spatial_unit_resample_EcM<- unique(as.data.frame(splot_records_colonization_final_na.omit_resample_EcM[[1]]))
colnames(spatial_unit_resample_EcM)<-"numeric_id2"
spatial_unit_resample_EcM$T<-"TRUE"

join_EcM<- merge( spatial_unit_complete_EcM, spatial_unit_resample_EcM, all.x=TRUE)

join_EcM[is.na(join_EcM)] <- 1

SU_EcM<- subset(join_EcM, T==1)

SU2_EcM<- join(SU_EcM, splot_records_colonization_final_na.omit_EcM, by="numeric_id2")
SU2_EcM[[2]]<-NULL

spatial_unit_resample_completeEcM<-rbind(splot_records_colonization_final_na.omit_resample_EcM, SU2_EcM)

#write.csv(spatial_unit_resample_completeEcM, "C:/Users/barcelom/Desktop/PAPER 4/Splot/EcM_relative_cover.csv")

########### COLONIZATION MEAN ##############
###########################################


########AM#######


#Add relative coverage 

AM_col_cover<- join(AM2[,c(1,2,3,5,6,9)], spatial_unit_resample_completeAM[,c(1,2,12,13,14)], by="PlotObservationID")

AM_col_cover_growth_form<-join(AM_col_cover, growth_form_AM[,c(1,5,6,7)])

AM_col_cover_clean<-subset(AM_col_cover_growth_form, numeric_id2>0)

AM_col_cover_clean$Relative_cover_herb_AM_relative<-AM_col_cover_clean$Relative_cover_herb_AM/AM_col_cover_clean$Rel.cov.AM.Col

AM_col_cover_clean$Relative_cover_woody_AM_relative<-AM_col_cover_clean$Relative_cover_woody_AM/AM_col_cover_clean$Rel.cov.AM.Col

#calculate weighted colonization by cover and growth form

#without growth forms

AM_col_cover_clean$AM_colonization_mean_weight<-((AM_col_cover_clean$Relative_cover)/AM_col_cover_clean$Rel.cov.AM.Col)*
  AM_col_cover_clean$AM_colonization_mean

AM_col_cover_clean$number_occurences_weighted<-((AM_col_cover_clean$Relative_cover)/AM_col_cover_clean$Rel.cov.AM.Col)*
  AM_col_cover_clean$number_occurrences_colonization

AM_col_cover_clean_final<-aggregate(AM_colonization_mean_weight~ PlotObservationID, AM_col_cover_clean,function(x) sum(x) )

AM_col_cover_clean_col_mean<-aggregate(AM_colonization_mean_weight~ PlotObservationID ,AM_col_cover_clean,function(x) sum(x) )
AM_col_cover_clean_num_occurrences<-aggregate(number_occurences_weighted~ PlotObservationID,AM_col_cover_clean,function(x) sum(x))
AM_col_cover_clean_rel.cov<-aggregate(Rel.cov.AM.relative~ PlotObservationID,AM_col_cover_clean,function(x) mean(x))

AM_col_cover_clean_final<-cbind(AM_col_cover_clean_col_mean, AM_col_cover_clean_num_occurrences[,2], AM_col_cover_clean_rel.cov[,2])
colnames(AM_col_cover_clean_final)[[3]]<-"number_occurrences_colonization"
colnames(AM_col_cover_clean_final)[[4]]<-"Relative_cover"

#join with plot info

Splot_colonization_AM<-join(spatial_unit_resample_completeAM[, -c(8:11, 15:17)], AM_col_cover_clean_final, by="PlotObservationID")

#aggregate by spatial unit
Splot_AM_colonization_mean_spatial_unit<-aggregate(AM_colonization_mean_weight~ numeric_id2, Splot_colonization_AM,function(x) mean(x))
Splot_AM_colonization_sd_spatial_unit<-aggregate(AM_colonization_mean_weight~ numeric_id2, Splot_colonization_AM,function(x) sd(x))
Splot_AM_colonization_se_spatial_unit<-aggregate(AM_colonization_mean_weight~ numeric_id2, Splot_colonization_AM,function(x) std.error(x))
Splot_AM_num_occurrences_spatial_unit<-aggregate(number_occurrences_colonization~ numeric_id2, Splot_colonization_AM,function(x) mean(x))
Splot_AM_Relative_cover_spatial_unit<-aggregate(Relative_cover~ numeric_id2, Splot_colonization_AM,function(x) mean(x))
Splot_AM_number_Splot_spatial_unit<-aggregate(PlotObservationID~ numeric_id2, Splot_colonization_AM,function(x) length(x))

Splot_AM_colonization_spatial_unit<-cbind(Splot_AM_colonization_mean_spatial_unit,Splot_AM_colonization_sd_spatial_unit[[2]],
                                           Splot_AM_colonization_se_spatial_unit[[2]],Splot_AM_num_occurrences_spatial_unit[[2]],
                                           Splot_AM_Relative_cover_spatial_unit[[2]],Splot_AM_number_Splot_spatial_unit[[2]])
colnames(Splot_AM_colonization_spatial_unit)[[2]]<-"AM_colonization_mean"
colnames(Splot_AM_colonization_spatial_unit)[[3]]<-"AM_colonization_sd"
colnames(Splot_AM_colonization_spatial_unit)[[4]]<-"AM_colonization_se"
colnames(Splot_AM_colonization_spatial_unit)[[5]]<-"AM_number_occurrences_colonization"
colnames(Splot_AM_colonization_spatial_unit)[[6]]<-"AM_Relative_cover"
colnames(Splot_AM_colonization_spatial_unit)[[7]]<-"number_Splots"
#write.csv(Splot_AM_colonization_spatial_unit, "C:/Users/barcelom/Desktop/PAPER 4/Splot/ID_AM_Col_total.csv")

### herbs ###

AM_col_cover_clean_herb<-subset(AM_col_cover_clean, Corrected=="herb")

AM_col_cover_clean_herb$AM_colonization_mean_weight<-((AM_col_cover_clean_herb$Relative_cover)/AM_col_cover_clean_herb$Relative_cover_herb_AM)*
  AM_col_cover_clean_herb$AM_colonization_mean

AM_col_cover_clean_herb$growth_form<-"herb"

#join with plot info

Splot_colonization_AM_herbs<-join(spatial_unit_resample_completeAM[, -c(8:11, 15:17)], AM_col_cover_clean_herb, by="PlotObservationID")

#aggregate by spatial unit
Splot_AM_herbs_colonization_mean_spatial_unit<-aggregate(AM_colonization_mean_weight~ numeric_id2, Splot_colonization_AM_herbs,function(x) mean(x))
Splot_AM_herbs_colonization_sd_spatial_unit<-aggregate(AM_colonization_mean_weight~ numeric_id2, Splot_colonization_AM_herbs,function(x) sd(x))
Splot_AM_herbs_colonization_se_spatial_unit<-aggregate(AM_colonization_mean_weight~ numeric_id2, Splot_colonization_AM_herbs,function(x) std.error(x))

Splot_AM_herbs_colonization_spatial_unit<-cbind(Splot_AM_herbs_colonization_mean_spatial_unit,Splot_AM_herbs_colonization_sd_spatial_unit[[2]],
                                                Splot_AM_herbs_colonization_se_spatial_unit[[2]])
colnames(Splot_AM_herbs_colonization_spatial_unit)[[2]]<-"AM_herbs_colonization_mean"
colnames(Splot_AM_herbs_colonization_spatial_unit)[[3]]<-"AM_herbs_colonization_sd"
colnames(Splot_AM_herbs_colonization_spatial_unit)[[4]]<-"AM_herbs_colonization_se"

#write.csv(Splot_AM_herbs_colonization_spatial_unit, "C:/Users/barcelom/Desktop/PAPER 4/Splot/ID_AM_Col_herbs_reduced_Bailey&Land_cover.csv")

### Woody ###

AM_col_cover_clean_woody<-subset(AM_col_cover_clean, Corrected=="woody")

AM_col_cover_clean_woody$AM_colonization_mean_weight<-((AM_col_cover_clean_woody$Relative_cover)/AM_col_cover_clean_woody$Relative_cover_woody_AM)*
  AM_col_cover_clean_woody$AM_colonization_mean

AM_col_cover_clean_woody$growth_form<-"woody"

#join with plot info

Splot_colonization_AM_woody<-join(spatial_unit_resample_completeAM[, -c(8:11, 15:17)], AM_col_cover_clean_woody, by="PlotObservationID")

#aggregate by spatial unit
Splot_AM_woody_colonization_mean_spatial_unit<-aggregate(AM_colonization_mean_weight~ numeric_id2, Splot_colonization_AM_woody,function(x) mean(x))
Splot_AM_woody_colonization_sd_spatial_unit<-aggregate(AM_colonization_mean_weight~ numeric_id2, Splot_colonization_AM_woody,function(x) sd(x))
Splot_AM_woody_colonization_se_spatial_unit<-aggregate(AM_colonization_mean_weight~ numeric_id2, Splot_colonization_AM_woody,function(x) std.error(x))

Splot_AM_woody_colonization_spatial_unit<-cbind(Splot_AM_woody_colonization_mean_spatial_unit,Splot_AM_woody_colonization_sd_spatial_unit[[2]],
                                                Splot_AM_woody_colonization_se_spatial_unit[[2]])
colnames(Splot_AM_woody_colonization_spatial_unit)[[2]]<-"AM_woody_colonization_mean"
colnames(Splot_AM_woody_colonization_spatial_unit)[[3]]<-"AM_woody_colonization_sd"
colnames(Splot_AM_woody_colonization_spatial_unit)[[4]]<-"AM_woody_colonization_se"

#write.csv(Splot_AM_woody_colonization_spatial_unit, "C:/Users/barcelom/Desktop/PAPER 4/Splot/ID_AM_Col_woody_reduced_Bailey&Land_cover.csv")

########EcM#######

#Add relative coverage 

EcM_col_cover<- join(EcM2[,c(1,2,3,7,8,9)], spatial_unit_resample_completeEcM[,c(1,2,15,16,17)], by="PlotObservationID")

EcM_col_cover_growth_form<-join(EcM_col_cover, growth_form_EcM[,c(1,5,6,7)])

EcM_col_cover_clean<-subset(EcM_col_cover_growth_form, numeric_id2>0)

EcM_col_cover_clean$Relative_cover_herb_EcM_relative<-EcM_col_cover_clean$Relative_cover_herb_EcM/EcM_col_cover_clean$Rel.cov.EcM.Col

EcM_col_cover_clean$Relative_cover_woody_EcM_relative<-EcM_col_cover_clean$Relative_cover_woody_EcM/EcM_col_cover_clean$Rel.cov.EcM.Col

#calculate weighted colonization by cover and growth form

#without growth forms

EcM_col_cover_clean$EcM_colonization_mean_weight<-((EcM_col_cover_clean$Relative_cover)/EcM_col_cover_clean$Rel.cov.EcM.Col)*
  EcM_col_cover_clean$EcM_colonization_mean

EcM_col_cover_clean$number_occurences_weighted<-((EcM_col_cover_clean$Relative_cover)/EcM_col_cover_clean$Rel.cov.EcM.Col)*
  EcM_col_cover_clean$number_occurrences_colonization.1

EcM_col_cover_clean_final<-aggregate(EcM_colonization_mean_weight~ PlotObservationID, EcM_col_cover_clean,function(x) sum(x) )

EcM_col_cover_clean_col_mean<-aggregate(EcM_colonization_mean_weight~ PlotObservationID ,EcM_col_cover_clean,function(x) sum(x) )
EcM_col_cover_clean_num_occurrences<-aggregate(number_occurences_weighted~ PlotObservationID,EcM_col_cover_clean,function(x) sum(x))
EcM_col_cover_clean_rel.cov<-aggregate(Rel.cov.EcM.relative~ PlotObservationID,EcM_col_cover_clean,function(x) mean(x))

EcM_col_cover_clean_final<-cbind(EcM_col_cover_clean_col_mean, EcM_col_cover_clean_num_occurrences[,2], EcM_col_cover_clean_rel.cov[,2])
colnames(EcM_col_cover_clean_final)[[3]]<-"number_occurrences_colonization"
colnames(EcM_col_cover_clean_final)[[4]]<-"Relative_cover"

#join with plot info

Splot_colonization_EcM<-join(spatial_unit_resample_completeEcM[, -c(8:11, 15:17)], EcM_col_cover_clean_final, by="PlotObservationID")

#aggregate by spatial unit
Splot_EcM_colonization_mean_spatial_unit<-aggregate(EcM_colonization_mean_weight~ numeric_id2, Splot_colonization_EcM,function(x) mean(x))
Splot_EcM_colonization_sd_spatial_unit<-aggregate(EcM_colonization_mean_weight~ numeric_id2, Splot_colonization_EcM,function(x) sd(x))
Splot_EcM_colonization_se_spatial_unit<-aggregate(EcM_colonization_mean_weight~ numeric_id2, Splot_colonization_EcM,function(x) std.error(x))
Splot_EcM_num_occurrences_spatial_unit<-aggregate(number_occurrences_colonization~ numeric_id2, Splot_colonization_EcM,function(x) mean(x))
Splot_EcM_Relative_cover_spatial_unit<-aggregate(Relative_cover~ numeric_id2, Splot_colonization_EcM,function(x) mean(x))
Splot_EcM_number_Splot_spatial_unit<-aggregate(PlotObservationID~ numeric_id2, Splot_colonization_EcM,function(x) length(x))

Splot_EcM_colonization_spatial_unit<-cbind(Splot_EcM_colonization_mean_spatial_unit,Splot_EcM_colonization_sd_spatial_unit[[2]],
                                          Splot_EcM_colonization_se_spatial_unit[[2]],Splot_EcM_num_occurrences_spatial_unit[[2]],
                                          Splot_EcM_Relative_cover_spatial_unit[[2]],Splot_EcM_number_Splot_spatial_unit[[2]])
colnames(Splot_EcM_colonization_spatial_unit)[[2]]<-"EcM_colonization_mean"
colnames(Splot_EcM_colonization_spatial_unit)[[3]]<-"EcM_colonization_sd"
colnames(Splot_EcM_colonization_spatial_unit)[[4]]<-"EcM_colonization_se"
colnames(Splot_EcM_colonization_spatial_unit)[[5]]<-"EcM_number_occurrences_colonization"
colnames(Splot_EcM_colonization_spatial_unit)[[6]]<-"EcM_Relative_cover"
colnames(Splot_EcM_colonization_spatial_unit)[[7]]<-"number_Splots"

#write.csv(Splot_EcM_colonization_spatial_unit, "C:/Users/barcelom/Desktop/PAPER 4/Splot/ID_EcM_Col_total.csv")

### herbs ###

EcM_col_cover_clean_herb<-subset(EcM_col_cover_clean, Corrected=="herb")

EcM_col_cover_clean_herb$EcM_colonization_mean_weight<-((EcM_col_cover_clean_herb$Relative_cover)/EcM_col_cover_clean_herb$Relative_cover_herb_EcM)*
  EcM_col_cover_clean_herb$EcM_colonization_mean

EcM_col_cover_clean_herb_final<-aggregate(EcM_colonization_mean_weight~ PlotObservationID ,EcM_col_cover_clean_herb,function(x) sum(x) )
EcM_col_cover_clean_herb_final$growth_form<-"herb"

#join with plot info

Splot_colonization_EcM_herbs<-join(spatial_unit_resample_completeEcM[, -c(8:11, 15:17)], EcM_col_cover_clean_herb_final, by="PlotObservationID")

#aggregate by spatial unit
Splot_EcM_herbs_colonization_mean_spatial_unit<-aggregate(EcM_colonization_mean_weight~ numeric_id2, Splot_colonization_EcM_herbs,function(x) mean(x))
Splot_EcM_herbs_colonization_sd_spatial_unit<-aggregate(EcM_colonization_mean_weight~ numeric_id2, Splot_colonization_EcM_herbs,function(x) sd(x))
Splot_EcM_herbs_colonization_se_spatial_unit<-aggregate(EcM_colonization_mean_weight~ numeric_id2, Splot_colonization_EcM_herbs,function(x) std.error(x))
Splot_EcM_herbs_colonization_spatial_unit<-cbind(Splot_EcM_herbs_colonization_mean_spatial_unit,Splot_EcM_herbs_colonization_sd_spatial_unit[[2]],Splot_EcM_herbs_colonization_se_spatial_unit[[2]])
colnames(Splot_EcM_herbs_colonization_spatial_unit)[[2]]<-"EcM_herbs_colonization_mean"
colnames(Splot_EcM_herbs_colonization_spatial_unit)[[3]]<-"EcM_herbs_colonization_sd"
colnames(Splot_EcM_herbs_colonization_spatial_unit)[[4]]<-"EcM_herbs_colonization_se"

#write.csv(Splot_EcM_herbs_colonization_spatial_unit, "C:/Users/barcelom/Desktop/PAPER 4/Splot/ID_EcM_Col_herb_reduced_Bailey&Land_cover.csv")

### Woody ###

EcM_col_cover_clean_woody<-subset(EcM_col_cover_clean, Corrected=="woody")

EcM_col_cover_clean_woody$EcM_colonization_mean_weight<-((EcM_col_cover_clean_woody$Relative_cover)/EcM_col_cover_clean_woody$Relative_cover_woody_EcM)*
  EcM_col_cover_clean_woody$EcM_colonization_mean

EcM_col_cover_clean_woody_final<-aggregate(EcM_colonization_mean_weight~ PlotObservationID ,EcM_col_cover_clean_woody,function(x) sum(x) )
EcM_col_cover_clean_woody_final$growth_form<-"woody"

#join with plot info

Splot_colonization_EcM_woody<-join(spatial_unit_resample_completeEcM[, -c(8:11, 15:17)], EcM_col_cover_clean_woody_final, by="PlotObservationID")

#aggregate by spatial unit
Splot_EcM_woody_colonization_mean_spatial_unit<-aggregate(EcM_colonization_mean_weight~ numeric_id2, Splot_colonization_EcM_woody,function(x) mean(x))
Splot_EcM_woody_colonization_sd_spatial_unit<-aggregate(EcM_colonization_mean_weight~ numeric_id2, Splot_colonization_EcM_woody,function(x) sd(x))
Splot_EcM_woody_colonization_se_spatial_unit<-aggregate(EcM_colonization_mean_weight~ numeric_id2, Splot_colonization_EcM_woody,function(x) std.error(x))
Splot_EcM_woody_colonization_spatial_unit<-cbind(Splot_EcM_woody_colonization_mean_spatial_unit,Splot_EcM_woody_colonization_sd_spatial_unit[[2]],Splot_EcM_woody_colonization_se_spatial_unit[[2]])
colnames(Splot_EcM_woody_colonization_spatial_unit)[[2]]<-"EcM_woody_colonization_mean"
colnames(Splot_EcM_woody_colonization_spatial_unit)[[3]]<-"EcM_woody_colonization_sd"
colnames(Splot_EcM_woody_colonization_spatial_unit)[[4]]<-"EcM_woody_colonization_se"

#write.csv(Splot_EcM_woody_colonization_spatial_unit, "C:/Users/barcelom/Desktop/PAPER 4/Splot/ID_EcM_Col_woody_reduced_Bailey&Land_cover.csv")


SPlot_complete[!duplicated(my_data$Sepal.Width), ]
