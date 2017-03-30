library(ggplot2)

df<-read.csv('results-gradient-boosting.csv')

line_df<-data_frame(
  slope=c(1,1,1),
  intercept=c(5, 0, -5),
  linetype=factor(
    x=c(5,1,6),
    levels=c(1,5,6), 
    labels=c('Measured = Predicted',
             'Measured = Predicted + 3%',
             'Measured = Predicted - 3%')))
  # mapping levels to labels, 1=predicted, 5=prdicted+3%, 6=predicted-3%
  # x, content in the line_df

g<-ggplot(df,aes(pred, y,alpha=0.3) )+geom_point(show.legend = FALSE)+
  labs(x='Predicted', y='Measured', 
       title='Measured vs Predicted Passing Rate')+
  geom_abline(aes(intercept = intercept, 
                  slope=slope, 
                  linetype=linetype), 
              data=line_df,
              color='grey50')+
  theme(plot.title = element_text(hjust=0.5),
        legend.title = element_blank(),
        legend.position = c(0.8,0.2))
  
plot(g)






