## C5. Population Demography
## Script to define the population's demography
## Alejandro I. Trejo-Castro 11/03/2022


#Demographic stats for the entire population
population<-read.csv("Datos/2. Selección de pacientes/DB2.Final-dataset.csv",stringsAsFactors = 0)
#Gender stats Male=1 Female=2
Popul_Num<-length(unique(population$RID))
Popul_Male<-length(unique(population$RID[which(population$gender==1)]))
Popul_Female<-length(unique(population$RID[which(population$gender==2)]))
#Age stats mean and sd
Popul_Age_mean<-round(mean(population$age),digits=1)
Popul_Age_sd<-round(sd(population$age),digits=1)
#Education stats mean and sd
Popul_Edu_mean<-round(mean(population$educat),digits=1)
Popul_Edu_sd<-round(sd(population$educat),digits=1)

#Demographic stats for MCI=5y patients
MCI<-population[which(population$class==0),]
#Gender stats Male=1 Female=2
MCI_Num<-length(unique(MCI$RID))
MCI_Male<-length(unique(MCI$RID[which(MCI$gender==1)]))
MCI_Female<-length(unique(MCI$RID[which(MCI$gender==2)]))
#Age stats mean and sd
MCI_Age_mean<-round(mean(MCI$age),digits=1)
MCI_Age_sd<-round(sd(MCI$age),digits=1)
#Education stats mean and sd
MCI_Edu_mean<-round(mean(MCI$educat),digits=1)
MCI_Edu_sd<-round(sd(MCI$educat),digits=1)

#Demographic stats for AD to the 2y patients
AD<-population[which(population$class==1),]
#Gender stats Male=1 Female=2
AD_Num<-length(unique(AD$RID))
AD_Male<-length(unique(AD$RID[which(AD$gender==1)]))
AD_Female<-length(unique(AD$RID[which(AD$gender==2)]))
#Age stats mean and sd
AD_Age_mean<-round(mean(AD$age),digits=1)
AD_Age_sd<-round(sd(AD$age),digits=1)
#Education stats mean and sd
AD_Edu_mean<-round(mean(AD$educat),digits=1)
AD_Edu_sd<-round(sd(AD$educat),digits=1)

#p-values Age and Education between groups
p_Age<-wilcox.test(MCI$age,AD$age)
p_Edu<-wilcox.test(MCI$educat,AD$educat)

p_Male<-chisq.test(population$class,population$gender)

#Create dataframe with information
Datos_Poblacion<-data.frame( Group_of_study = c("Number of subjects","Age(years)","Education(years)"), 
                             All= c(paste(Popul_Num,"(",Popul_Male,")"),paste(Popul_Age_mean,"+-",Popul_Age_sd),paste(Popul_Edu_mean,"+-",Popul_Edu_sd)), 
                             MCI_S=c(paste(MCI_Num,"(",MCI_Male,")"),paste(MCI_Age_mean,"+-",MCI_Age_sd),paste(MCI_Edu_mean,"+-",MCI_Edu_sd)),
                             MCI_P=c(paste(AD_Num,"(",AD_Male,")"),paste(AD_Age_mean,"+-",AD_Age_sd),paste(AD_Edu_mean,"+-",AD_Edu_sd)),
                             P_VALUE=c( p_Male$p.value ,p_Age$p.value,p_Edu$p.value))

write.csv(Datos_Poblacion,file="Datos/2. Selección de pacientes/DB4.Population_demography.csv",row.names = FALSE)                              

