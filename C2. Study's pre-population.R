## C2. Population for the study
## Script to define the study's population
## Alejandro I. Trejo-Castro 09/03/2022

#install.packages("dplyr")
#install.packages("stringi")
library(dplyr)
library(stringi)

  #Load database with ADNI's diagnosis and demographic data
  data<-read.csv("Datos/2. Selección de pacientes/DB0.Adni diagnosis and dem.csv", stringsAsFactors = FALSE)
    #Classification of patients, mci stable = 0, mci progressive = 1
  data$class<-ifelse(data$RID%in%data$RID[which(data$DX==3)],1,0)
  
  #Creation of MCI-stable dataset
  mcidata<-data[which(data$class==0),]
    #Create column Last visit
  mcidata$LV<-stri_sub(mcidata$VISCODE2,-2,-1)
    #Change bl for a 0 in Last visit
  mcidata$LV<-ifelse(mcidata$LV=="bl",0,mcidata$LV)
    #Classification of patients LV>=60 =1 ; O.c.=0
  mcidata$class2<-ifelse(mcidata$RID%in%mcidata$RID[which(mcidata$LV>=60)],1,0)
    #Keep MCI patiens who not progress to AD in 5 years or more
  mcidata<-mcidata[which(mcidata$class2==1),]
    #Keep only the baseline visit
  mcidata<-mcidata[which(mcidata$VISCODE2=="bl"),]
    #Eliminate columns class2 and LV 
  mcidata<-subset(mcidata, select = c(-LV,-class2))
  
  
  #Creation of MCI-progressive data set
  addata<-data[which(data$class==1),]
    #Create column Last Visit
  addata$LV<-addata$VISCODE2
    #Leave only the number in the last visit
  addata$LV<-ifelse(addata$LV=="bl",0,ifelse(addata$LV%in%c("m108","m120","m132","m144"),
                                             as.numeric(stri_sub(addata$LV,-3,-1)), as.numeric(
                                               stri_sub(addata$LV,-2,-1))))
  
    #Find the first diagnosis of AD for each patient
  DX3<-addata[which(addata$DX==3),]
  DX3<-group_by(DX3,RID)
  DX3<-slice(DX3,1)
    #Eliminate DX3 patients with diagnosis with a date different than 24 months
  DX3<-DX3[-which(DX3$LV!=24),]
    #Keep only the new baseline visit for the addata original
  addata<-addata[which(addata$VISCODE2=="bl"),]
    #Keep patients that are in DX3
  addata$classDX3<-ifelse(addata$RID%in%DX3$RID,1,0)
  addata<-addata[which(addata$classDX3==1),]
    #Erase the Last Visit column and classDX3
  addata<-subset(addata, select = -c(LV, classDX3))
  
  #Create Population database
  population<-rbind(addata,mcidata)
  #Keep patients with an age below 80 and above 70
  population <- population[which(population$age>65),]
  
  population<-population[order(population$RID),]

  
  write.csv(population,"Datos/2. Selección de pacientes/DB1.Study's pre-population.csv", row.names = FALSE)  
  