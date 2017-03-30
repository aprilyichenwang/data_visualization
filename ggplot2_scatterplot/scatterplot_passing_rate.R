library(ggplot2)


df<-read.csv('results-gradient-boosting.csv')
head(df)

g<-ggplot(df,aes(pred, y,alpha=0.3) )+geom_point(show.legend = FALSE)+
  labs(x='Predicted', y='Measured', 
       title='Measured vs Predicted Passing Rate')+
  scale_linetype_manual(labels = c('Measured = Predicted',
                                'Measured = Predicted - 3%',
                                'Measured = Predicted +3%'),
                 values=c(5,1,6))+
  
  # geom_abline(intercept = 0, slope=1,color='grey50', show.legend = TRUE)+
  # geom_abline(intercept = 3, slope=1,color='grey50', linetype=5, show.legend = TRUE)+
  # geom_abline(intercept = -3, slope=1,color='grey50', linetype=5, show.legend = TRUE)+
  geom_abline(aes(intercept = intercept, 
                  slope=slope, 
                  linetype=linetype), 
              data=line_df,
              color='grey50')+
  theme(plot.title = element_text(hjust=0.5),
        legend.title = element_blank(),
        legend.position = c(0.8,0.2))
  
plot(g)


line_df<-data_frame(c(1,2,3),
                    slope=c(1,1,1),
                    intercept=c(5, 0, -5),
                    linetype=factor(c(5,1,6), 
                            labels=c('Measured = Predicted',
                                          'Measured = Predicted - 3%',
                                          'Measured = Predicted +3%')))

line_df


