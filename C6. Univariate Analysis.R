## C6. Univariate analysis
## Script to run hypothesis testing
## Alejandro I. Trejo-Castro 11/03/2022
  
  #Load data
  data<-read.csv("Datos/2. Selección de pacientes/DB2.Final-dataset.csv",stringsAsFactors = FALSE)
  data<-data[-c(1:7)]
  
  names<-names(data)  
  names<-names[-1]  
  
  #Hypothesis testing
  ht<-c()
  for(i in 2:length(data)){
    ht[i-1]<-wilcox.test(data[,i][which(data$class==0)],data[,i][which(data$class==1)])$p.value
  }
  
  tests<-data.frame(Feature=names,p_value=ht)
  tests<-tests[order(tests$p_value),]
  
  #p-value adjustment with BH method
  tests$q_value<-p.adjust(tests$p_value,method="BH")
  tests$q_value_round<-round(tests$q_value,3)
  
  write.csv(tests,"Datos/3. Análisis de datos/DB5.Univariate analysis.csv",row.names = FALSE)
  