## C1. Diagnostic and demographic data
## Script to gather diagnostic information with demographic data
## Alejandro I. Trejo-Castro 09/03/2022

#install.packages("dplyr")
library(dplyr)

  #Read DX data
  dx<-read.csv("Datos/1. ADNI's originales/DXSUM_PDXCONV_ADNIALL.csv", stringsAsFactors = FALSE)
    #Select RID, Viscode2, Examdate and Attributes with Diagnostic Information
  dx<-select(dx,c("RID","VISCODE2","EXAMDATE","DXCHANGE","DXCURREN","DIAGNOSIS"))
  dx<-dx[order(dx$RID,dx$EXAMDATE),]
    #DXCHANGE CN(1,7,9)=1 MCI(2,4,8)=2 AD(3,5,6)=3
  dx$DXCHANGE2<-ifelse(dx$DXCHANGE==1|dx$DXCHANGE==7|dx$DXCHANGE==9,1,ifelse(dx$DXCHANGE==2|dx$DXCHANGE==4|dx$DXCHANGE==8,2,ifelse(dx$DXCHANGE==3|dx$DXCHANGE==5|dx$DXCHANGE==6,3,NA)))
    #Create standardized column of DX
  dx$DX<-ifelse(!is.na(dx$DXCHANGE2),dx$DXCHANGE2,ifelse(!is.na(dx$DXCURREN),dx$DXCURREN,dx$DIAGNOSIS))
    #Select RID Viscode2 ExamDate DX
  dx<-select(dx,c(1:3,8))
    #Remove viscode2 with "sc", NA and "uns1" values
  dx<-dx[-which(dx$VISCODE2=="sc" | dx$VISCODE2=="uns1" | dx$VISCODE2=="NA"),]

  #Read demographics data
  dem<-read.csv("Datos/1. ADNI's originales/PTDEMOG.csv",colClasses = c(rep("NULL",2),NA,rep("NULL",2),NA,rep("NULL",3),rep(NA,3),rep("NULL",2),NA,rep("NULL",17)),stringsAsFactors = 0)
    #Create column of DOB
  dem$dob<-as.Date(ifelse(is.na(dem$PTDOBMM) | is.na(dem$PTDOBYY),NA,paste0(dem$PTDOBYY,"-",dem$PTDOBMM,"-15")))
  
  #Match dem to dx
  dx$dob<-dem$dob[match(dx$RID,dem$RID)]
  dx$gender<-dem$PTGENDER[match(dx$RID,dem$RID)]
  dx$educat<-dem$PTEDUCAT[match(dx$RID,dem$RID)]
  
  #Remove subjects without data
  dx<-dx[which(!is.na(dx$gender)),]
  dx<-dx[which(!is.na(dx$DX)),]
  dx<-dx[which(!is.na(dx$educat)),]
  dx<-dx[which(!is.na(dx$EXAMDATE)),]
  dx<-dx[which(!is.na(dx$dob)),]
  
  #Include only subjects with an MCI diagnosis at BL
  mciBl<-dx[which(dx$RID%in%dx$RID[which(dx$VISCODE2=="bl" & dx$DX==2)]),]
  
  #Remove subjects with only one visit
  diag<-mciBl[-which(mciBl$RID%in%as.numeric(names(which(table(mciBl$RID)==1)))),]

  #Get age
  diag$age<-round(as.numeric(as.Date(diag$EXAMDATE)-diag$dob)/365,1)
  diag<-subset(diag, select = c(-dob))
  
  #remove subjects with a DX=1 after a BL DX=2
  diag<-diag[-which(diag$RID%in%diag$RID[which(diag$DX==1 )]),]
  
  #Save database
  write.csv(diag, file="Datos/2. Selección de pacientes/DB0.Adni diagnosis and dem.csv",col.names = TRUE,row.names = FALSE)
  