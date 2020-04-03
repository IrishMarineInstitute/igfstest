library(RODBC)
library(dplyr)

channel <- odbcDriverConnect("Driver=SQL Server; 
                             Server=vmfssdev02;")
Q1 <- readChar('NEP/IGFS_Nep_Survey_LFD_MW_Final.sql',1e6)
data<- sqlQuery(channel,Q1)
stn<-readRDS("Data/stn.RDS")
stnNep<-stn%>%select( "fldCruiseName" ,"fldCruiseStationNumber", "fldShotLatDecimalDegrees" ,"fldShotLonDecimalDegrees",
                    "fldHaulLatDecimalDegrees","fldHaulLonDecimalDegrees" ,"Gear_Type", "TowDurationMin"  ,"DoorSweptAreaKmSq"  )

names(data)[c(8,9)]<-c("fldCruiseName"   ,         "fldCruiseStationNumber")
dat1<-merge(data, stnNep, by.x=c("fldCruiseName"   ,"fldCruiseStationNumber"))
dat1<-dat1[c(3:9,1,2,10:21)]
names(dat1)[c(8,9,21)]<-c("Survey_Code","Haul","AreaKmSq")
write.csv(dat1,"Data/datNep.csv",row.names = FALSE)
