
library(leaflet)
library(shiny)
library(flexdashboard)
library(shinydashboard)
library(shinyjs)
library(plotly)
library(htmltools)
library(shinycssloaders)
library(reshape2)
library(dplyr)
library(ggridges)
library(plotly)
library(tidyr)
library(tidyverse)
library(DT)



#mapdata <- readRDS("Data/mapdata05032020.rds")
#SpAggdata <- readRDS("Data/SpAggdata05032020.rds")
#sp_data_gp <- readRDS("Data/sp_data_gp_05032020.rds")


#########################################Main Page data######################################################
#############################################################################################################
data1=readRDS("Data/data1.RDS")

###############For SpAggdata################
dd<-data1%>% group_by(fldPrimeStation,Yr,LonDec,LatDec,Species)%>%summarise(CatchKg=sum(CatchKg))
dd<-dd[-which(is.na(dd$fldPrimeStation)),]

d0<-data1%>% group_by(fldPrimeStation,Yr,LonDec,LatDec)%>%summarise(CatchKg=sum(CatchKg))
d0<-d0[-which(is.na(d0$fldPrimeStation)),]

SpAggdata<- dcast(dd, fldPrimeStation +Yr+LonDec+LatDec ~ Species, value.var="CatchKg")
SpAggdata$CatchKg<-d0$CatchKg
###################################################
#######################For mapdata###########################

mapdata<-aggregate(data1[,c("AreaKmSq","CatchKg", "RaisedNo")], 
                           by=list(data1$fldPrimeStation,data1$LonDec, data1$LatDec,data1$Yr),FUN=sum,  na.rm=TRUE)
names(mapdata) = c("PrimeStation", "Longitude"  ,  "Latitude"   ,  "Year"   ,      "AreaKmSq" , 
                           "CatchKg"    ,  "RaisedNo")

##################################################
################sp_data_gr########################




temp<-aggregate(data1[,c("AreaKmSq","CatchKg", "RaisedNo")], 
                by=list(data1$fldPrimeStation,data1$LonDec, data1$LatDec,data1$Yr,data1$Species),FUN=sum,  na.rm=TRUE)
names(temp) = c("PrimeStation", "Longitude"  ,  "Latitude"   ,  "Year"   ,  "Species" ,    "AreaKmSq" , 
                "CatchKg"    ,  "RaisedNo")

temp$Group<-temp$Species

levels(temp$Group)[which(levels(temp$Group) %in% c("ARG", "BOF", "GSS", "HER" ,"HOM", "MAC", "SPR", "WHB"))]<-"Small pelagic" 
levels(temp$Group)[which(levels(temp$Group) %in% c("BLR" ,"CUR", "DFL", "DGS" ,"DII" ,"LSD", "PTR", "SAR", "SDR" ,"SHR", "SKT" ,"THR", "UNR"))]<-"Elasmobranch" 
levels(temp$Group)[which(levels(temp$Group) %in% c("COD", "ESB" ,"GUG", "HAD" ,"HKE", "JOD", "POK", "POL", "WHG"))]<-"Large demersal" 
levels(temp$Group)[which(levels(temp$Group) %in% c("DAB", "LBI", "MEG", "PLE", "SOL"))]<-"Flat fish" 
levels(temp$Group)[which(levels(temp$Group) %in% c("MON" ,"WAF"))]<-"Monkfish" 
levels(temp$Group)[which(levels(temp$Group) %in% c("NOP", "POD"))]<-"Small demersal" 
sp_data_gp<-temp


#Species selectInput
sp_names<-read.csv("Data/Sp_names.csv")


### Get Data - pull out valid stns
stn=readRDS("Data/stn.RDS")



dat=readRDS("Data/dat.RDS")
dat<-droplevels(dat[which(dat$Species %in% levels(data1$Species)),])
dat$symbSize <- sqrt( dat$Kg_Per_Hr/ pi )
dat$Year = as.numeric(substr(dat$Cruise,5,8))
maxyear=max(dat$Year)

dat1<-aggregate(dat[,c("Catch_Kg", "Kg_Per_Hr")], 
                by=list(dat$Year,dat$Cruise, dat$Haul, dat$Prime_Stn, dat$Lon, dat$Lat, dat$Species),FUN=sum,  na.rm=TRUE)
names(dat1) = c("Year", "Cruise", "Haul", "Prime_Stn", "Lon", "Lat","Species", "Catch_Kg", "Kg_Per_Hr")
dat1$symbSize <- sqrt( dat1$Kg_Per_Hr/ pi )


# Read in shapefiles
div <- geojsonio::geojson_read("Data/div_simple.geojson", what = "sp")
cont <- geojsonio::geojson_read("Data/cont1_simple.geojson", what = "sp")

#Used for Distribution no/km2 and Abundance

##calculating swept area estimates
catch_km2<-with(data1,(1/AreaKmSq*CatchKg))
No_km2<-with(data1,(1/AreaKmSq*RaisedNo))
Area<-ifelse(data1$ICESCODE=="VIa","VI","VII")
data1<-cbind(data1,catch_km2,No_km2, Area)
#Agregate by Haul and species to get unique haul data for skt
mapdataS<-aggregate(data1[,c("CatchKg", "RaisedNo", "catch_km2", "No_km2")], 
                   by=list(data1$Yr,data1$Haul,data1$LonDec, data1$LatDec, data1$Species),FUN=sum,  na.rm=TRUE)
names(mapdataS) = c("Year", "Haul", "LonDec", "LatDec","Species", "CatchKg", "RaisedNo", "catch_km2", "No_km2")
mapdataS$symbSize <- sqrt( mapdataS$No_km2/ pi )

#Length/Weight and Length/Age plots
LengthWeightAge=readRDS("Data/LengthWeightAge.RDS")
LengthWeightAge<-filter(LengthWeightAge,ICESCODE !="VIIa")
LengthWeightAge<-droplevels(LengthWeightAge[which(LengthWeightAge$fldMainSpeciesCode %in% levels(data1$Species)),])
LengthWeightAge<-filter(LengthWeightAge,fldGearDescription!="Griffin 43/53 - New IBTS prototype 2019 full size clean gear")
LengthWeightAge<-droplevels(LengthWeightAge)

#Used for Length Frequency and Total/Adults/Juvenile numbers
LengthData=readRDS("Data/LengthData.RDS")
LengthData<-droplevels(LengthData[which(LengthData$Species %in% levels(data1$Species)),])
LengthData<-filter(LengthData,fldGearDescription!="Griffin 43/53 - New IBTS prototype 2019 full size clean gear")
LengthData<-droplevels(LengthData)
LengthData$CatchNos30minHaul=LengthData$CatchNos/LengthData$fldTowDuration*30

#Total Numbers


#Total Numbers
if(dim(LengthData)[1]>0){
  TotalNumbersMap=aggregate(LengthData[,c("CatchNos", "CatchNos30minHaul")],
                            by=list(LengthData$Year,LengthData$fldCruiseStationNumber,
                                    LengthData$fldShotLonDecimalDegrees, LengthData$fldShotLatDecimalDegrees
                                    ,LengthData$Species),
                            FUN=sum,  na.rm=TRUE)
  names(TotalNumbersMap) = c("Year", "Haul", "LonDec", "LatDec","Species" ,"CatchNos", "CatchNos30minHaul")
  TotalNumbersMap$symbSize <- sqrt( TotalNumbersMap$CatchNos30minHaul/ pi )
}else{
  TotalNumbersMap=data.frame("Year"=maxyear,
                             "Haul"=NA,
                             "LonDec"=-9.558,
                             "LatDec"=55.109,
                             "Species"=NA,
                             "CatchNos"=NA,
                             "CatchNos30minHaul"=NA,
                             "symbSize"=NA)}

#Juveniles
Juveniles= filter(LengthData, AgeClassification=="Juvenile")
if(dim(Juveniles)[1]>0){
  JuvNumbersMap=aggregate(Juveniles[,c("CatchNos", "CatchNos30minHaul")],
                          by=list(Juveniles$Year,Juveniles$fldCruiseStationNumber,
                                  Juveniles$fldShotLonDecimalDegrees, Juveniles$fldShotLatDecimalDegrees,
                                  Juveniles$Species),
                          FUN=sum,  na.rm=TRUE)
  names(JuvNumbersMap) = c("Year", "Haul", "LonDec", "LatDec","Species", "CatchNos", "CatchNos30minHaul")
  JuvNumbersMap$symbSize <- sqrt( JuvNumbersMap$CatchNos30minHaul/ pi )
}else{
  JuvNumbersMap=data.frame("Year"=maxyear,
                           "Haul"=NA,
                           "LonDec"=-9.558,
                           "LatDec"=55.109,
                           "Species"=NA,
                           "CatchNos"=NA,
                           "CatchNos30minHaul"=NA,
                           "symbSize"=NA)}

#Adults
Adults= filter(LengthData, AgeClassification=="Adult")
if(dim(Adults)[1]>0){
  AdultNumbersMap=aggregate(Adults[,c("CatchNos", "CatchNos30minHaul")],
                            by=list(Adults$Year,Adults$fldCruiseStationNumber,
                                    Adults$fldShotLonDecimalDegrees, Adults$fldShotLatDecimalDegrees,
                                    Adults$Species),
                            FUN=sum,  na.rm=TRUE)
  names(AdultNumbersMap) = c("Year", "Haul", "LonDec", "LatDec","Species", "CatchNos", "CatchNos30minHaul")
  AdultNumbersMap$symbSize <- sqrt( AdultNumbersMap$CatchNos30minHaul/ pi )
}else{
  AdultNumbersMap=data.frame("Year"=maxyear,
                             "Haul"=NA,
                             "LonDec"=-9.558,
                             "LatDec"=55.109,
                             "Species"=NA,
                             "CatchNos"=NA,
                             "CatchNos30minHaul"=NA,
                             "symbSize"=NA)}



#############define colours#################


def<-c("#F8766D","#00BFC4","#B79F00","#619CFF","#00BA38","#F564E3")


################################################
#source("VBGM_coeff/coeff_Length_Age.R")
coeff_L_A<-read.csv("Data/coeff_L_A.csv")

############################################
vbTyp = function(age, Linf, K, t0)Linf*(1-exp(-K*(age-t0)))