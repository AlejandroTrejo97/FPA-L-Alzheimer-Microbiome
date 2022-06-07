## C8. Plots for seeing patterns in the regressions
## Script to compute correlations
## Alejandro I. Trejo-Castro 11/03/2022
#install.packages("ggplot2")
library(ggplot2)

correlations<-read.csv("Datos/3. Análisis de datos/DB6.Correlations.csv", stringsAsFactors = FALSE)
correlations<-correlations[1:30,]

df<-read.csv("Datos/2. Selección de pacientes/DB2.Final-dataset.csv",stringsAsFactors = FALSE)

for(i in 1:nrow(correlations)){
  name<-paste(i,".",correlations[i,1],"-",correlations[i,2])
  x_axis<-correlations[i,1]
  y_axis<-correlations[i,2]
  corr<-round(correlations[i,3],3)
  text1<-paste("corr:",corr)
  
  ggplot(df, aes(x = df[,x_axis], y = df[,y_axis])) +
    geom_point(aes(colour = factor(class))) +
    stat_smooth()+ xlab(x_axis)+ylab(y_axis)+labs(tag=text1)+theme(plot.tag.position = c(0.5,0.95))
  ggsave(paste(name,".png"))
}
