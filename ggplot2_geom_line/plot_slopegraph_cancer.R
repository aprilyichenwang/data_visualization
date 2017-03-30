library(dplyr)
library(ggplot2)
library(ggthemes)
library(tidyr)

data_preprocess<-function() {
  # preprocess data, subset data, clean data
  # gather/melt data into CancerType, Years, Survival,
  # with Years, Survival- numeric
  # return prprocessed data
  
  data <- read.csv(
    'cancer_survival_rate.csv', check.name=FALSE,
    stringsAsFactors = FALSE)
  
  data<-dplyr::rename(data, CancerType = `Cancer type`) %>%
    tidyr::gather('Years', 'Survival', -CancerType) %>%
    dplyr::mutate(
      Survival = as.numeric(Survival),
      Years = gsub("year", "years", Years),
      NumericYear = as.numeric(gsub("\\ years", "", Years))) %>%
    dplyr::arrange(NumericYear, desc(Survival)) %>%
    dplyr::filter(Years %in% c("5 years", "10 years", "20 years"))
  
  return(data)
}

plot_data2 <- function(dataTable) {
  dataTable$Years <- factor(
    x = dataTable$Years,
    levels = unique(as.character(dataTable$Years)),
    ordered = TRUE)
  
  # labels is a df that maps cancertype with the survival rate of 5 years
  labels <-
    dplyr::filter(dataTable, Years == "5 years") %>%
    dplyr::select(Survival, CancerType)

  # so the labels don't overlap for survival rate = 62
  labels[which(labels$Survival == 62), "Survival"] <- c(62, 61)
  
  # make initial segment line plot given after-processed data
  segmentPlot <-
    ggplot(dataTable, aes(x = Years, y = Survival,
                          group = CancerType,
                          label = Survival)) +
    ggtitle('Cancer Survival Rates') +
    geom_line(size = 0.25,color='grey') +
    geom_label(size = 2.2, label.size = 0) +
    annotate("text", size = 2.2, x = 0.7, y = labels$Survival, 
             label = labels$CancerType) +
    scale_x_discrete('', position = 'top') +
    scale_y_discrete('') +
    theme_tufte() +
    theme(plot.title = element_text(hjust = 0.5, size=20),
          axis.text.x=element_text(size=15))
  
  return(segmentPlot)
}

write_figure <- function(figure, filepath) {
  ggsave(
    filename = filepath,
    plot = figure,
    device = "png",
    height = 13,
    width = 7,
    units = "in")
}

dataTable <- data_preprocess()
figure <- plot_data2(dataTable)
# write_figure(figure, filepath = "cancer_survival_rates.png")
plot(figure)
