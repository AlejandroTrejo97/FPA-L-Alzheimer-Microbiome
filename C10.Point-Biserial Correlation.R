## C10. Point-Biserial Correlation
## Script for the point-biserial calculation
## Alejandro I. Trejo-Castro 15/03/2022

data<-read.csv("Datos/2. Selección de pacientes/DB2.Final-dataset.csv",stringsAsFactors = FALSE)

data<-data[-c(1:7)]
data<-lapply(data,as.numeric)
data<-as.data.frame(data)


resultados<-matrix(0,nrow = 891,ncol = 3)
coln<-c("Feature","Correlation","p-value")
colnames(resultados)<-coln

for (i in 2:length(data)){
  resultados[i-1,1]<-colnames(data)[i]
  correlation<-cor.test(data[,i],data[,1])
  resultados[i-1,2]<-correlation$estimate
  resultados[i-1,3]<-correlation$p.value
}

resultados<-as.data.frame(resultados)
features<-resultados$Feature
resultados<-lapply(resultados,as.numeric)
resultados<-as.data.frame(resultados)
resultados$Feature<-features
resultados$corrabs<-round(abs(resultados$Correlation),3)

resultados<-resultados[order(-resultados$corrabs),]  

write.csv(resultados,"Datos/3. Análisis de datos/DB7.Point-Biserial Correlation.csv",row.names = FALSE)
