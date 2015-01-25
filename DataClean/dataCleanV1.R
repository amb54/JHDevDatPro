library(plyr)
library(reshape2)

##Loading data into R
totPop<-read.csv("rawData/sp.pop.totl_Indicator_en_csv_v2.csv",skip=2,header=TRUE, stringsAsFactors = FALSE)
metadataTotPop<-read.csv("rawData/Metadata_Country_sp.pop.totl_Indicator_en_csv_v2.csv", header=TRUE,stringsAsFactors = FALSE)

agrRurDev<-read.csv("rawData/ag.lnd.arbl.ha.pc_Indicator_en_csv_v2.csv",skip=2,header=TRUE, stringsAsFactors = FALSE)
metadataAgrRurDev<-read.csv("rawData/Metadata_Country_ag.lnd.arbl.ha.pc_Indicator_en_csv_v2.csv", header=TRUE,stringsAsFactors = FALSE)

accToEl<-read.csv("rawData/eg.elc.accs.zs_Indicator_en_csv_v2.csv",skip=2,header=TRUE, stringsAsFactors = FALSE)
metadataAccToEl<-read.csv("rawData/Metadata_Country_eg.elc.accs.zs_Indicator_en_csv_v2.csv", header=TRUE,stringsAsFactors = FALSE
                          )
intUse<-read.csv("rawData/it.net.user.p2_Indicator_en_csv_v2.csv",skip=2,header=TRUE, stringsAsFactors = FALSE)
metadataIntUse<-read.csv("rawData/Metadata_Country_it.net.user.p2_Indicator_en_csv_v2.csv", header=TRUE,stringsAsFactors = FALSE)

h2oRU<-read.csv("rawData/sh.h2o.safe.ru.zs_Indicator_en_csv_v2.csv",skip=2,header=TRUE, stringsAsFactors = FALSE)
metadataH2oRU<-read.csv("rawData/Metadata_Country_sh.h2o.safe.ru.zs_Indicator_en_csv_v2.csv", header=TRUE,stringsAsFactors = FALSE)

h2oUR<-read.csv("rawData/sh.h2o.safe.ur.zs_Indicator_en_csv_v2.csv",skip=2,header=TRUE, stringsAsFactors = FALSE)
metadataH2oUR<-read.csv("rawData/Metadata_Country_sh.h2o.safe.ur.zs_Indicator_en_csv_v2.csv", header=TRUE,stringsAsFactors = FALSE)

gnpPC<-read.csv("rawData/ny.gnp.pcap.pp.cd_Indicator_en_csv_v2.csv",skip=2,header=TRUE, stringsAsFactors = FALSE)
metadataGnpPC<-read.csv("rawData/Metadata_Country_ny.gnp.pcap.pp.cd_Indicator_en_csv_v2.csv", header=TRUE,stringsAsFactors = FALSE)

ferRate<-read.csv("rawData/sp.dyn.tfrt.in_Indicator_en_csv_v2.csv",skip=2,header=TRUE, stringsAsFactors = FALSE)
metadataFerRate<-read.csv("rawData/Metadata_Country_sp.dyn.tfrt.in_Indicator_en_csv_v2.csv", header=TRUE,stringsAsFactors = FALSE)

motVeh<-read.csv("rawData/is.veh.nveh.p3_Indicator_en_csv_v2.csv",skip=2,header=TRUE, stringsAsFactors = FALSE)
metadataMotVeh<-read.csv("rawData/Metadata_Country_is.veh.nveh.p3_Indicator_en_csv_v2.csv", header=TRUE,stringsAsFactors = FALSE)

mortBaby<-read.csv("rawData/sh.dyn.mort_Indicator_en_csv_v2.csv",skip=2,header=TRUE, stringsAsFactors = FALSE)
metadataMortBaby<-read.csv("rawData/Metadata_Country_sh.dyn.mort_Indicator_en_csv_v2.csv", header=TRUE,stringsAsFactors = FALSE)




##Merge data with metadata
totalPopulation<-merge(metadataTotPop[,c(2:3,5)],totPop,by.x="Country.Code",by.y="Country.Code",all=TRUE)
arableLand<-merge(metadataAgrRurDev[,c(2:3,5)],agrRurDev[,-5],by.x="Country.Code",by.y="Country.Code",all=TRUE)
accessToElectricity<-merge(metadataAccToEl[,c(2:3,5)],accToEl,by.x="Country.Code",by.y="Country.Code",all=TRUE)
internetUse<-merge(metadataIntUse[,c(2:3,5)],intUse,by.x="Country.Code",by.y="Country.Code",all=TRUE)
freshWaterRU<-merge(metadataH2oRU[,c(2:3,5)],h2oRU[,-5],by.x="Country.Code",by.y="Country.Code",all=TRUE)
freshWaterUR<-merge(metadataH2oUR[,c(2:3,5)],h2oUR[,-5],by.x="Country.Code",by.y="Country.Code",all=TRUE)
gnpPerCapita<-merge(metadataGnpPC[,c(2:3,5)],gnpPC[,-5],by.x="Country.Code",by.y="Country.Code",all=TRUE)
fertilityRate<-merge(metadataFerRate[,c(2:3,5)],ferRate,by.x="Country.Code",by.y="Country.Code",all=TRUE)
motorVehicle<-merge(metadataMotVeh[,c(2:3,5)],motVeh[,-5],by.x="Country.Code",by.y="Country.Code",all=TRUE)
mortalityBaby<-merge(metadataMortBaby[,c(2:3,5)],mortBaby[,-5],by.x="Country.Code",by.y="Country.Code",all=TRUE)


##Merge data
data0<-rbind(totalPopulation,arableLand,accessToElectricity,internetUse,freshWaterRU,freshWaterUR,gnpPerCapita,fertilityRate,motorVehicle,mortalityBaby)


##CleanData
data1<-data0[,1:58]
indWLD <- data1[,1] %in% "WLD" 
data1[indWLD,2]<-"World"
indEUU <- data1[,1] %in% "EUU" 
data1[indEUU,2]<-"European Union"
data2<-data1[which(data1$Region!=""),]
data3<-data2[which(data2$Region!="NA"),]

##Change variable names
colNames0<-colnames(data3)
colNames1<-gsub(".","",colNames0,fixed=TRUE)
colNames2<-gsub("X","",colNames1,fixed=TRUE)
colnames(data3)<-colNames2
countryNames<-data.frame(Country=data3[1:214,4],stringsAsFactors=FALSE)
library(plyr)
countries<-arrange(countryNames,Country)
write.csv(countries,"countries.csv")

##reshaping and save as csv-file
library(reshape2)
dataMelt<-melt(data3,id=c(1:6),measure.vars=c(7:58))
write.csv(dataMelt,"dataMelt.csv")







