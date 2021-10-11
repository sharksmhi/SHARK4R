#' Visualize range of data for specific parameters
#' Uses data from national marine monitoring for the last 5 years to identify outliers 
#' Ranges and IQR (interquantile range) for specific parameters is adapted to each datatype
#' @param data for tibble be be checked
#' @return ggplot and tibble of data with outliers 
#' @export
outlier_plot <- function(data, parameter, mild_outlier, extreme_outlier) {
  mild.threshold.upper = mild_outlier                                                    
  extreme.threshold.upper = extreme_outlier
data_vis = data %>% 
  filter(parameter == parameter) %>% 
  select(value)
p = ggplot(data_vis,aes(y = value))+
  geom_boxplot(outlier.colour = "red")+
  geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
  geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
  ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
fig <- ggplotly(p)
print(fig)

}

#' Visualize range of data for specific parameters
#' Uses data from within the dataset to identify outliers 
#' Ranges and IQR (interquantile range) for specific parameters is adapted to each dataset
#' @param data for tibble be be checked
#' @return ggplot and tibble of data with outliers 
#' @export
internal_outlier_plot <- function(data, parameter) {
  data_vis = data %>% 
    filter(parameter == parameter) %>% 
    select(value)
  p = ggplot(data_vis,aes(y = value))+
    geom_boxplot(outlier.colour = "red")+
    theme_bw()+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  fig <- ggplotly(p)
  print(fig)
  
}