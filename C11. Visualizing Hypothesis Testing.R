## C11. Plots for seeing patterns in the point-biserial correlation
## Script to compute correlations
## Alejandro I. Trejo-Castro 11/03/2022
#install.packages("ggplot2")
library(ggplot2)

#correlations<-read.csv("Datos/3. Análisis de datos/DB7.Point-Biserial Correlation.csv", stringsAsFactors = FALSE)
#correlations<-correlations[1:50,]

results<-read.csv("Datos/3. Análisis de datos/DB5.Univariate analysis.csv",stringsAsFactors = FALSE)
results$p_valuero<-round(results$p_value,4)
results<-results[1:50,]

df<-read.csv("Datos/2. Selección de pacientes/DB2.Final-dataset.csv",stringsAsFactors = FALSE)
df$class<-as.factor(df$class)
# for(i in 1:nrow(correlations)){
#   name<-paste(i,".",correlations[i,1])
#   x_axis<-"class"
#   y_axis<-correlations[i,1]
#   corr<-round(correlations[i,2],3)
#   text1<-paste("corr:",corr)
#   
#   ggplot(df, aes(x = df[,x_axis], y = df[,y_axis])) +
#     geom_point() +
#     stat_smooth()+ xlab(x_axis)+ylab(y_axis)+labs(tag=text1)+theme(plot.tag.position = c(0.5,0.95))
#   ggsave(paste(name,".png"))
# }

for(i in 1:nrow(results)){
  name<-paste(i,".",results[i,1])
  x_axis<-"class"
  y_axis<-results[i,1]
  ggplot(df,aes(x=df[,x_axis], y=df[,y_axis], fill=class))+
    geom_violin()+geom_boxplot(width=0.1)+xlab(x_axis)+ylab(y_axis)
  ggsave(paste(name,".png"))
    #stat_summary(fun.y = mean,geom="point",size=2,shape=23)
}