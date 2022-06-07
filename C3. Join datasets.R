## C3. Join all the parts of the final dataset
## Script to define the final dataset including metabolomics and imaging
## Alejandro I. Trejo-Castro 11/03/2022

library(dplyr)

#Load Population data set
population<-read.csv("Datos/2. Selección de pacientes/DB1.Study's pre-population.csv", stringsAsFactors = FALSE)

#Load Gut Metabolites and Lipidomics data
metabolites<-read.csv("Datos/1. ADNI's originales/Gut Metabolites/ADMCGUTMETABOLITESLONG_12_13_21_CLEAN.csv",stringsAsFactors = FALSE)
#imaging<-read.csv("Datos/1. ADNI's originales/FreeSurfer/UCSFFSL51_03_01_22_CLEAN.csv",stringsAsFactors = FALSE)
biomarkers<-read.csv("Datos/1. ADNI's originales/Biomarkers/UPENNBIOMK_MASTER_CLEAN.csv",stringsAsFactors = FALSE)

#Prepare data sets
#First gut metabolites
metabolites<-metabolites[which(metabolites$VISCODE2=="bl"),]
metabolites$flag<-ifelse(metabolites$RID%in%metabolites$RID[which(metabolites$RID%in%population$RID)],1,0)    
metabolites<-metabolites[which(metabolites$flag==1),]
metabolites<-subset(metabolites,select=c(-flag,-VISCODE2,-EXAMDATE))  

#Second imaging
#imaging<-imaging[which(imaging$VISCODE2=="scmri"),]
#imaging$flag<-ifelse(imaging$RID%in%imaging$RID[which(imaging$RID%in%population$RID)],1,0)    
#imaging<-imaging[which(imaging$flag==1),]
#imaging<-subset(imaging,select=c(-flag,-VISCODE2,-EXAMDATE))

biomarkers<-biomarkers[which(biomarkers$VISCODE=="bl"),]
biomarkers$flag<-ifelse(biomarkers$RID%in%biomarkers$RID[which(biomarkers$RID%in%population$RID)],1,0)
biomarkers<-biomarkers[which(biomarkers$flag==1),]
biomarkers<-subset(biomarkers,select=c(-flag,-VISCODE,-BATCH))

#Get the different experiment data sets
#Join population and metabolites data set
for(i in 2:length(metabolites)){
  population[i+7]<-metabolites[i][match(population$RID,metabolites$RID),]
  colnames(population)[i+7]<-colnames(metabolites)[i]
}

for(i in 2:length(biomarkers)){
  population[i+111]<-biomarkers[i][match(population$RID,biomarkers$RID),]
  colnames(population)[i+111]<-colnames(biomarkers)[i]
}

#Keep patients that have all the information
f_dataset<-population
f_dataset<-f_dataset[complete.cases(f_dataset),]  

write.csv(f_dataset,"Datos/2. Selección de pacientes/DB2.Final-dataset.csv",row.names = FALSE)
