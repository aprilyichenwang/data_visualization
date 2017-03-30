library(dplyr)
library(gridExtra)

get_data<-function(){
  glmnet<-read.csv('results-glmnet-cross-table.csv')
  pgb_cross<-read.csv('results-pgboost-cross-table.csv')
  
  glmnet2<-mutate(glmnet, label='glmnet')
  pgb_cross2<-mutate(pgb_cross, label='boosting')
  
  df<-rbind(glmnet2, pgb_cross2)  
}



make_plot<-function(df){
  g1<-ggplot(df, aes(label, rmse))+geom_boxplot()
  g1<-g+labs(title='RMSE',x='',y='')+ylim(c(1.2,3.5))+
    theme(plot.title = element_text(hjust = 0.5))
  g2<-ggplot(df, aes(label, mederr))+geom_boxplot()
  g2<-g2+labs(title='MedErr',x='',y='')+ylim(c(0.225,0.425))+
    theme(plot.title = element_text(hjust = 0.5))
  g2
  grid.arrange(g2, g1, ncol=2, top='Comparing glmnet and gradient boosting')
}

df<-get_data()
make_plot(df)
