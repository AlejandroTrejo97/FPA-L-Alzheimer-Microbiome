data<-read.csv("Datos/2. Selección de pacientes/DB2.Final-dataset.csv",stringsAsFactors = FALSE)
data<-data[-c(1:7)]
library(Hmisc)

data_p<-data[which(data$class==1),]
data_p<-data_p[-1]
 
data_s<-data[which(data$class==0),] 
data_s<-data_s[-1]
  
correlations_p<-rcorr(as.matrix(data_p),type = c("spearman")) 
correlations_s<-rcorr(as.matrix(data_s),type=c("spearman"))

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

cor_results_p<-flattenCorrMatrix(correlations_p$r,correlations_p$P)
cor_results_s<-flattenCorrMatrix(correlations_s$r,correlations_s$P)

cor_results<-cbind(cor_results_p$row,cor_results_p$column,cor_results_p$cor,cor_results_s$cor)
colnames(cor_results)<-c("row","column","corr_p","corr_s")
cor_results<-as.data.frame(cor_results)
cor_results$newmetric<-abs(as.numeric(cor_results$corr_p)-(as.numeric(cor_results$corr_s)))
cor_results$newmetric<-round(cor_results$newmetric,3)
cor_results<-cor_results[order(-cor_results$newmetric),]

write.csv(cor_results,"Datos/3. Análisis de datos/DB7.Correlations_newmetric.csv",row.names = FALSE)
