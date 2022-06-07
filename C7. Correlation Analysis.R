## C7. Correlation analysis
## Script to compute correlations
## Alejandro I. Trejo-Castro 11/03/2022

  #Load data
  data<-read.csv("Datos/2. Selección de pacientes/DB2.Final-dataset.csv",stringsAsFactors = FALSE)
  data<-data[-c(1:8)]
  
  #Correlation matrix with significance levels
  #install.packages("Hmisc")
  library(Hmisc)
  correlations<-rcorr(as.matrix(data),type = c("spearman"))  
  
  #Function to format correlation matrix provided by http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software
  flattenCorrMatrix <- function(cormat, pmat) {
    ut <- upper.tri(cormat)
    data.frame(
      row = rownames(cormat)[row(cormat)[ut]],
      column = rownames(cormat)[col(cormat)[ut]],
      cor  =(cormat)[ut],
      p = pmat[ut]
    )
  }
  
  cor_results<-flattenCorrMatrix(correlations$r,correlations$P)
  cor_results$corrabs<-round(abs(cor_results$cor),3)  
  cor_results$p_val_round<-round(cor_results$p,3)    
  cor_results$bh<-round(p.adjust(cor_results$p,method = "BH"),3)  
  cor_results<-cor_results[order(-cor_results$corrabs),]  
  
  #correlations2<-cor(data)
  #col<-colorRampPalette(c("blue","white","red"))(20)
  #heatmap(x=correlations2,col=col,symn=TRUE)
  
  write.csv(cor_results,"Datos/3. Análisis de datos/DB6.Correlations.csv",row.names = FALSE)