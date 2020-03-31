library(devtools)

#devtools::install_github('droglenc/FSAsim')

library(FSAsim)
library(FSA)
library(magrittr)
library(dplyr)
library(nlstools)
library(tidyverse)
library(broom)
library(lhmixr)
library(tidyverse)

coeff_Length_Age<-function(LWA){
sp_names<-read.csv("Data/Sp_names.csv")
fish<-filter(sp_names,age=="yes")$speciesFAO
fish<-droplevels(fish)
LWA_missingremoved=filter(LWA, !is.na(age) & age>=0  & ICESCODE !="VIIa")
LWA_fish=filter(LWA_missingremoved, fldMainSpeciesCode %in% fish)
LWA_fish<-droplevels(LWA_fish)
#################################################################################################
vbTyp = function(age, Linf, K, t0) Linf*(1-exp(-K*(age-t0)))
#################################################################################################
#################################All data########################################################
#################################################################################################
dfSample = LWA_fish %>%group_by(fldMainSpeciesCode)%>%
do(fitSample = try(nls(length ~ vbTyp(age, Linf, K, t0), data=.,
                       start=vbStarts(length ~ age,data=.))))
coeffs=NULL
for(i in 1:dim(dfSample)[1]){
  if(substring(dfSample$fitSample[[i]][1],1,5)=="Error"){
    coeffs=rbind(coeffs, c(paste(dfSample$fldMainSpeciesCode[[i]]),"All data","None",NA,"No", NA, NA,NA,NA))
  }else{
    coeffs=rbind(coeffs, cbind(paste(dfSample$fldMainSpeciesCode[[i]]),"All data","None",NA,"No",
                               coef(dfSample$fitSample[[i]])[[1]], coef(dfSample$fitSample[[i]])[[2]],
                               coef(dfSample$fitSample[[i]])[[3]],NA))
 }
}
coeffs1=as.data.frame(coeffs)
colnames(coeffs1)=c("Species","Data","Parameter","Year","Cohort","Linf", "K", "t0","Level")


###########################################################################################################
###########################################All sex########################################################
#############################################################################################################
LWA_fishA<-filter(LWA_fish,obs.sex!="unclassified")
LWA_fishA<-droplevels(LWA_fishA)
dfSample = LWA_fishA %>%group_by(fldMainSpeciesCode,obs.sex)%>%
  do(fitSample = try(nls(length ~ vbTyp(age, Linf, K, t0), data=.,
                         start=vbStarts(length ~ age,data=.))))
coeffs=NULL
for(i in 1:dim(dfSample)[1]){
  if(substring(dfSample$fitSample[[i]][1],1,5)=="Error"){
    coeffs=rbind(coeffs, c(paste(dfSample$fldMainSpeciesCode[[i]]),paste("All",dfSample$obs.sex[[i]],"data"),"Sex",NA,"No", NA, NA, NA,paste(dfSample$obs.sex[[i]])))
  }else{
    coeffs=rbind(coeffs, cbind(paste(dfSample$fldMainSpeciesCode[[i]]),paste("All",dfSample$obs.sex[[i]],"data"),"Sex",NA,"No",
                               coef(dfSample$fitSample[[i]])[[1]], coef(dfSample$fitSample[[i]])[[2]],
                               coef(dfSample$fitSample[[i]])[[3]],paste(dfSample$obs.sex[[i]])))
  }
}
coeffs2=as.data.frame(coeffs)
colnames(coeffs2)=c("Species","Data","Parameter","Year","Cohort", "Linf","K", "t0","Level")

###########################################################################################################
###########################################All gear########################################################
###############################################################################################################
LWA_fishGG<-filter(LWA_fish,fldGearDescription!="Griffin 43/53 - New IBTS prototype 2019 full size clean gear")
LWA_fishG<-droplevels(LWA_fishGG)

dfSample = LWA_fishG %>%group_by(fldMainSpeciesCode,fldGearDescription)%>%
  do(fitSample = try(nls(length ~ vbTyp(age, Linf, K, t0), data=.,
                         start=vbStarts(length ~ age,data=.))))
coeffs=NULL
for(i in 1:dim(dfSample)[1]){
  if(substring(dfSample$fitSample[[i]][1],1,5)=="Error"){
    coeffs=rbind(coeffs, c(paste(dfSample$fldMainSpeciesCode[[i]]),paste("All",dfSample$fldGearDescription[[i]],"data"),
                           "Gear", NA,"No", NA, NA,NA,paste(dfSample$fldGearDescription[[i]])))
  }else{
    coeffs=rbind(coeffs, cbind(paste(dfSample$fldMainSpeciesCode[[i]]),paste("All",dfSample$fldGearDescription[[i]],"data"),"Gear",NA,"No",
                               coef(dfSample$fitSample[[i]])[[1]], coef(dfSample$fitSample[[i]])[[2]],
                               coef(dfSample$fitSample[[i]])[[3]],paste(dfSample$fldGearDescription[[i]])))
  }
}
coeffs3=as.data.frame(coeffs)
colnames(coeffs3)=c("Species","Data","Parameter","Year","Cohort", "Linf","K", "t0","Level")

###########################################################################################################
###########################################################################################################
###########################################All division########################################################
###############################################################################################################
dfSample = LWA_fish%>%group_by(fldMainSpeciesCode,ICESCODE)%>%
  do(fitSample = try(nls(length ~ vbTyp(age, Linf, K, t0), data=.,
                         start=vbStarts(length ~ age,data=.))))
coeffs=NULL
for(i in 1:dim(dfSample)[1]){
  if(substring(dfSample$fitSample[[i]][1],1,5)=="Error"){
    coeffs=rbind(coeffs, c(paste(dfSample$fldMainSpeciesCode[[i]]),paste("All",dfSample$ICESCODE[[i]],"data"),
                           "Division", NA,"No", NA, NA,NA,paste(dfSample$ICESCODE[[i]])))
  }else{
    coeffs=rbind(coeffs, cbind(paste(dfSample$fldMainSpeciesCode[[i]]),paste("All",dfSample$ICESCODE[[i]],"data"),
                               "Division",NA,"No",
                               coef(dfSample$fitSample[[i]])[[1]], coef(dfSample$fitSample[[i]])[[2]],
                               coef(dfSample$fitSample[[i]])[[3]],paste(dfSample$ICESCODE[[i]])))
  }
}
coeffs4=as.data.frame(coeffs)
colnames(coeffs4)=c("Species","Data","Parameter","Year","Cohort", "Linf","K", "t0","Level")


###########################################################################################################
###########################################All data by year#################################################
############################################################################################################
dfSample = LWA_fish %>%group_by(fldMainSpeciesCode,Year)%>%
  do(fitSample = try(nls(length ~ vbTyp(age, Linf, K, t0), data=.,
                         start=vbStarts(length ~ age,data=.))))
coeffs=NULL
for(i in 1:dim(dfSample)[1]){
  if(substring(dfSample$fitSample[[i]][1],1,5)=="Error"){
    coeffs=rbind(coeffs, c(paste(dfSample$fldMainSpeciesCode[[i]]),paste(dfSample$Year[[i]],"data"),"None", dfSample$Year[[i]],"No", NA, NA,NA,NA))
  }else{
    coeffs=rbind(coeffs, cbind(paste(dfSample$fldMainSpeciesCode[[i]]),paste(dfSample$Year[[i]],"data"),"None",dfSample$Year[[i]],"No",
                               coef(dfSample$fitSample[[i]])[[1]], coef(dfSample$fitSample[[i]])[[2]],
                               coef(dfSample$fitSample[[i]])[[3]],NA))
  }
}
coeffs5=as.data.frame(coeffs)
colnames(coeffs5)=c("Species","Data","Parameter","Year","Cohort", "Linf","K", "t0","Level")

######################################################################################################
######################################################################################################
###########################################sex by year################################################
######################################################################################################

dfSample = LWA_fishA %>%group_by(fldMainSpeciesCode,obs.sex,Year)%>%
  do(fitSample = try(nls(length ~ vbTyp(age, Linf, K, t0), data=.,
                         start=vbStarts(length ~ age,data=.))))

coeffs=NULL
for(i in 1:dim(dfSample)[1]){
  if(substring(dfSample$fitSample[[i]][1],1,5)=="Error"){
    coeffs=rbind(coeffs, c(paste(dfSample$fldMainSpeciesCode[[i]]),
                           paste(dfSample$Year[[i]],dfSample$obs.sex[[i]],"data"),"Sex",
                           dfSample$Year[[i]],"No", NA, NA, NA,paste(dfSample$obs.sex[[i]])))
  }else{
    coeffs=rbind(coeffs, cbind(paste(dfSample$fldMainSpeciesCode[[i]]),paste(dfSample$Year[[i]],dfSample$obs.sex[[i]],"data"),
                               "Sex",dfSample$Year[[i]],"No",
                               coef(dfSample$fitSample[[i]])[[1]], coef(dfSample$fitSample[[i]])[[2]],
                               coef(dfSample$fitSample[[i]])[[3]],paste(dfSample$obs.sex[[i]])))
  }
}
coeffs6=as.data.frame(coeffs)
colnames(coeffs6)=c("Species","Data","Parameter","Year","Cohort", "Linf","K", "t0","Level")


######################################################################################################
######################################################################################################
###########################################gear by year################################################
###################################################################################################
dfSample = LWA_fishG %>%group_by(fldMainSpeciesCode,fldGearDescription,Year)%>%
  do(fitSample = try(nls(length ~ vbTyp(age, Linf, K, t0), data=.,
                         start=vbStarts(length ~ age,data=.))))

coeffs=NULL
for(i in 1:dim(dfSample)[1]){
  if(substring(dfSample$fitSample[[i]][1],1,5)=="Error"){
    coeffs=rbind(coeffs, c(paste(dfSample$fldMainSpeciesCode[[i]]),paste(dfSample$Year[[i]],dfSample$fldGearDescription[[i]],"data"),
                           "Gear",dfSample$Year[[i]],"No", NA, NA,NA,paste(dfSample$fldGearDescription[[i]])))
  }else{
    coeffs=rbind(coeffs, cbind(paste(dfSample$fldMainSpeciesCode[[i]]),paste(dfSample$Year[[i]],dfSample$fldGearDescription[[i]],"data"),"Gear",
                               dfSample$Year[[i]],"No",
                               coef(dfSample$fitSample[[i]])[[1]], coef(dfSample$fitSample[[i]])[[2]],
                               coef(dfSample$fitSample[[i]])[[3]],paste(dfSample$fldGearDescription[[i]])))
  }
}
coeffs7=as.data.frame(coeffs)
colnames(coeffs7)=c("Species","Data","Parameter","Year","Cohort", "Linf","K", "t0","Level")


######################################################################################################
######################################################################################################
###########################################Div by year################################################
###################################################################################################
dfSample = LWA_fish%>%group_by(fldMainSpeciesCode,ICESCODE,Year)%>%
  do(fitSample = try(nls(length ~ vbTyp(age, Linf, K, t0), data=.,
                         start=vbStarts(length ~ age,data=.))))

coeffs=NULL
for(i in 1:dim(dfSample)[1]){
  if(substring(dfSample$fitSample[[i]][1],1,5)=="Error"){
    coeffs=rbind(coeffs, c(paste(dfSample$fldMainSpeciesCode[[i]]),paste(dfSample$Year[[i]],dfSample$ICESCODE[[i]],"data"),
                           "Division",dfSample$Year[[i]],"No", NA, NA,NA,paste(dfSample$ICESCODE[[i]])))
  }else{
    coeffs=rbind(coeffs, cbind(paste(dfSample$fldMainSpeciesCode[[i]]),paste(dfSample$Year[[i]],dfSample$ICESCODE[[i]],"data")
                               ,"Division",dfSample$Year[[i]],"No",
                               coef(dfSample$fitSample[[i]])[[1]], coef(dfSample$fitSample[[i]])[[2]],
                               coef(dfSample$fitSample[[i]])[[3]],paste(dfSample$ICESCODE[[i]])))
  }
}
coeffs8=as.data.frame(coeffs)
colnames(coeffs8)=c("Species","Data","Parameter","Year","Cohort", "Linf","K", "t0","Level")


########################################################################################
###########################Cohort##############################################################
                              
dfSample = LWA_fish %>%
  filter(Cohort>2002)%>%
  group_by(fldMainSpeciesCode,Cohort)%>%
  do(fitSample = try(nls(length ~ vbTyp(age, Linf, K, t0), data=.,
                         start=vbStarts(length ~ age,data=.))))
coeffs=NULL
for(i in 1:dim(dfSample)[1]){
  if(substring(dfSample$fitSample[[i]][1],1,5)=="Error"){
    coeffs=rbind(coeffs, c(paste(dfSample$fldMainSpeciesCode[[i]]),paste("Cohort",dfSample$Cohort[[i]]),"None", dfSample$Cohort[[i]],"Yes",NA, NA,NA,NA))
  }else{
    coeffs=rbind(coeffs, cbind(paste(dfSample$fldMainSpeciesCode[[i]]),paste("Cohort",dfSample$Cohort[[i]]),"None",dfSample$Cohort[[i]],"Yes",
                               coef(dfSample$fitSample[[i]])[[1]], coef(dfSample$fitSample[[i]])[[2]],
                               coef(dfSample$fitSample[[i]])[[3]],NA))
  }
}
coeffs9=as.data.frame(coeffs)
colnames(coeffs9)=c("Species","Data","Parameter","Year","Cohort", "Linf","K", "t0","Level")


########################################################################################
###########################Cohort by sex#################################################


dfSample = LWA_fishA%>%group_by(fldMainSpeciesCode,Cohort,obs.sex)%>%
  filter(Cohort>2002)%>%
  do(fitSample = try(nls(length ~ vbTyp(age, Linf, K, t0), data=.,
                         start=vbStarts(length ~ age,data=.))))
coeffs=NULL
for(i in 1:dim(dfSample)[1]){
  if(substring(dfSample$fitSample[[i]][1],1,5)=="Error"){
    coeffs=rbind(coeffs, c(paste(dfSample$fldMainSpeciesCode[[i]]),paste("Cohort",dfSample$Cohort[[i]],dfSample$obs.sex[[i]]),
                           "Sex", dfSample$Cohort[[i]],"Yes",NA, NA,NA,paste(dfSample$obs.sex[[i]])))
  }else{
    coeffs=rbind(coeffs, cbind(paste(dfSample$fldMainSpeciesCode[[i]]),paste("Cohort",dfSample$Cohort[[i]],dfSample$obs.sex[[i]]),
                               "Sex",dfSample$Cohort[[i]],"Yes",
                               coef(dfSample$fitSample[[i]])[[1]], coef(dfSample$fitSample[[i]])[[2]],
                               coef(dfSample$fitSample[[i]])[[3]],paste(dfSample$obs.sex[[i]])))
  }
}
coeffs10=as.data.frame(coeffs)
colnames(coeffs10)=c("Species","Data","Parameter","Year","Cohort", "Linf","K", "t0","Level")



########################################################################################
###########################Cohort by gear#################################################


dfSample = LWA_fishG%>%
  filter(Cohort>2002)%>%
  group_by(fldMainSpeciesCode,Cohort,fldGearDescription)%>%
  do(fitSample = try(nls(length ~ vbTyp(age, Linf, K, t0), data=.,
                         start=vbStarts(length ~ age,data=.))))
coeffs=NULL
for(i in 1:dim(dfSample)[1]){
  if(substring(dfSample$fitSample[[i]][1],1,5)=="Error"){
    coeffs=rbind(coeffs, c(paste(dfSample$fldMainSpeciesCode[[i]]),paste("Cohort",dfSample$Cohort[[i]],dfSample$fldGearDescription[[i]]),
                           "Gear", dfSample$Cohort[[i]],"Yes",NA, NA,NA,paste(dfSample$fldGearDescription[[i]])))
  }else{
    coeffs=rbind(coeffs, cbind(paste(dfSample$fldMainSpeciesCode[[i]]),paste("Cohort",dfSample$Cohort[[i]],dfSample$fldGearDescription[[i]]),
                               "Gear",dfSample$Cohort[[i]],"Yes",
                               coef(dfSample$fitSample[[i]])[[1]], coef(dfSample$fitSample[[i]])[[2]],
                               coef(dfSample$fitSample[[i]])[[3]],paste(dfSample$fldGearDescription[[i]])))
  }
}
coeffs11=as.data.frame(coeffs)
colnames(coeffs11)=c("Species","Data","Parameter","Year","Cohort", "Linf","K", "t0","Level")



########################################################################################
###########################Cohort by division#################################################


dfSample = LWA_fish%>%
  filter(Cohort>2002)%>%
  group_by(fldMainSpeciesCode,Cohort,ICESCODE)%>%
  do(fitSample = try(nls(length ~ vbTyp(age, Linf, K, t0), data=.,
                         start=vbStarts(length ~ age,data=.))))
coeffs=NULL
for(i in 1:dim(dfSample)[1]){
  if(substring(dfSample$fitSample[[i]][1],1,5)=="Error"){
    coeffs=rbind(coeffs, c(paste(dfSample$fldMainSpeciesCode[[i]]),
                           paste("Cohort",dfSample$Cohort[[i]],dfSample$ICESCODE[[i]]),
                           "Division", dfSample$Cohort[[i]],"Yes",NA, NA,NA,paste(dfSample$ICESCODE[[i]])))
  }else{
    coeffs=rbind(coeffs, cbind(paste(dfSample$fldMainSpeciesCode[[i]]),paste("Cohort",dfSample$Cohort[[i]],dfSample$ICESCODE[[i]]),
                               "Division",dfSample$Cohort[[i]],"Yes",
                               coef(dfSample$fitSample[[i]])[[1]], coef(dfSample$fitSample[[i]])[[2]],
                               coef(dfSample$fitSample[[i]])[[3]],paste(dfSample$ICESCODE[[i]])))
  }
}
coeffs12=as.data.frame(coeffs)
colnames(coeffs12)=c("Species","Data","Parameter","Year","Cohort", "Linf","K", "t0","Level")



################combined

all_data<-do.call(rbind, lapply(paste0("coeffs", 1:12) , get))
unlink("Data/coeff_L_A.csv")
write.csv(all_data,"Data/coeff_L_A.csv",row.names = F)
}


#run
#source("VBGM_coeff/coeff_Length_Age.R")
#coeff_Length_Age(LWA<-readRDS("Data/LengthWeightAge.RDS"))