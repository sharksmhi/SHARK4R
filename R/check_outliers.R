#' Check range of data for specific parameters
#' Uses data from national marine monitoring for the last 5 years to identify outliers 
#' Ranges and IQR (interquantile range) for specific parameters is adapted to each datatype
#' @param data for tibble be be checked
#' @return ggplot and tibble of data with outliers 
#' @export

check_bacterial_production <- function(data) {
  mild.threshold.upper = 765842329                      
  extreme.threshold.upper = 1200706084              
  
  if (any(data$parameter=="Bacterial production") == FALSE) {
    message("Parameter Bacterial Production not found")
  }
  if (any(data$value[which(data$parameter=="Bacterial production")] > extreme.threshold.upper)) {
    data_vis = data %>% 
      filter(parameter == "Bacterial production") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter Bacterial Production, measurement(s) is outside range please check for outliers!")
    extreme.outliers = data %>%
      filter(parameter == "Bacterial production", value > extreme.threshold.upper) %>% 
      select(station_name, sample_date, sample_id, shark_sample_id_md5, sample_min_depth_m, sample_max_depth_m, value)
    print(kable(extreme.outliers))
  }
  else {
    data_vis = data %>% 
      filter(parameter == "Bacterial production") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter Bacterial Production, measurement(s) is within range")
  }
  
}

check_bacterial_concentration <- function(data) {
  mild.threshold.upper = 4686343500                      
  extreme.threshold.upper = 6779382000              
  
  if (any(data$parameter=="Bacterial concentration") == FALSE) {
    message("Parameter Bacterial concentration not found")
  }
  if (any(data$value[which(data$parameter=="Bacterial concentration")] > extreme.threshold.upper)) {
    data_vis = data %>% 
      filter(parameter == "Bacterial concentration") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter Bacterial concentration, measurement(s) is outside range please check for outliers!")
    extreme.outliers = data %>%
      filter(parameter == "Bacterial concentration", value > extreme.threshold.upper) %>% 
      select(station_name, sample_date, sample_id, shark_sample_id_md5, sample_min_depth_m, sample_max_depth_m, value)
    print(kable(extreme.outliers))
  }
  else {
    data_vis = data %>% 
      filter(parameter == "Bacterial concentration") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter Bacterial concentration, measurement(s) is within range")
  }
}

check_bacterial_carbon <- function(data) {
  mild.threshold.upper = 18.96                      
  extreme.threshold.upper = 20.76              
  
  if (any(data$parameter=="Bacterial cell carbon content") == FALSE) {
    message("Parameter Bacterial cell carbon content not found")
  }
  if (any(data$value[which(data$parameter=="Bacterial cell carbon content")] > extreme.threshold.upper)) {
    data_vis = data %>% 
      filter(parameter == "Bacterial cell carbon content") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter Bacterial cell carbon content, measurement(s) is outside range please check for outliers!")
    extreme.outliers = data %>%
      filter(parameter == "Bacterial cell carbon content", value > extreme.threshold.upper) %>% 
      select(station_name, sample_date, sample_id, shark_sample_id_md5, sample_min_depth_m, sample_max_depth_m, value)
    print(kable(extreme.outliers))
  }
  else {
    data_vis = data %>% 
      filter(parameter == "Bacterial cell carbon content") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter Bacterial cell carbon content, measurement(s) is within range")
  }
}

check_picoplankton_abundance <- function(data) {
  mild.threshold.upper = 85155831                                  
  extreme.threshold.upper = 133564616              
  
  if (any(data$parameter=="Abundance") == FALSE) {
    message("Parameter Abundance not found")
  }
  if (any(data$value[which(data$parameter=="Abundance")] > extreme.threshold.upper)) {
    data_vis = data %>% 
      filter(parameter == "Abundance") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter Abundance, measurement(s) is outside range please check for outliers!")
    extreme.outliers = data %>%
      filter(parameter == "Abundance", value > extreme.threshold.upper) %>% 
      select(station_name, sample_date, sample_id, shark_sample_id_md5, sample_min_depth_m, sample_max_depth_m, value)
    print(kable(extreme.outliers))
  }
  else {
    data_vis = data %>% 
      filter(parameter == "Abundance") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter Abundance, measurement(s) is within range")
  }
}

check_picoplankton_biovol <- function(data) {
  mild.threshold.upper = 0.05928825                                  
  extreme.threshold.upper = 0.09323008              
  
  if (any(data$parameter=="Biovolume concentration") == FALSE) {
    message("Parameter Biovolume concentration not found")
  }
  if (any(data$value[which(data$parameter=="Biovolume concentration")] > extreme.threshold.upper)) {
    data_vis = data %>% 
      filter(parameter == "Biovolume concentration") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter Biovolume concentration, measurement(s) is outside range please check for outliers!")
    extreme.outliers = data %>%
      filter(parameter == "Abundance", value > extreme.threshold.upper) %>% 
      select(station_name, sample_date, sample_id, shark_sample_id_md5, sample_min_depth_m, sample_max_depth_m, value)
    print(kable(extreme.outliers))
  }
  else {
    data_vis = data %>% 
      filter(parameter == "Biovolume concentration") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter Biovolume concentration, measurement(s) is within range")
  }
}

check_picoplankton_carbon <- function(data) {
  mild.threshold.upper = 13.27727                                  
  extreme.threshold.upper = 20.85692              
  
  if (any(data$parameter=="Carbon concentration") == FALSE) {
    message("Parameter Carbon concentration not found")
  }
  if (any(data$value[which(data$parameter=="Carbon concentration")] > extreme.threshold.upper)) {
    data_vis = data %>% 
      filter(parameter == "Carbon concentration") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter Carbon concentration, measurement(s) is outside range please check for outliers!")
    extreme.outliers = data %>%
      filter(parameter == "Abundance", value > extreme.threshold.upper) %>% 
      select(station_name, sample_date, sample_id, shark_sample_id_md5, sample_min_depth_m, sample_max_depth_m, value)
    print(kable(extreme.outliers))
  }
  else {
    data_vis = data %>% 
      filter(parameter == "Carbon concentration") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter Carbon concentration, measurement(s) is within range")
  }
}

check_picoplankton_counted <- function(data) {
  mild.threshold.upper = 461.5                                  
  extreme.threshold.upper = 733              
  
  if (any(data$parameter=="# counted") == FALSE) {
    message("Parameter # counted not found")
  }
  if (any(data$value[which(data$parameter=="# counted")] > extreme.threshold.upper)) {
    data_vis = data %>% 
      filter(parameter == "# counted") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter # counted, measurement(s) is outside range please check for outliers!")
    extreme.outliers = data %>%
      filter(parameter == "Abundance", value > extreme.threshold.upper) %>% 
      select(station_name, sample_date, sample_id, shark_sample_id_md5, sample_min_depth_m, sample_max_depth_m, value)
    print(kable(extreme.outliers))
  }
  else {
    data_vis = data %>% 
      filter(parameter == "# counted") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter # counted, measurement(s) is within range")
  }
}

check_zooplankton_abund <- function(data) {
  mild.threshold.upper = 1092.02                                  
  extreme.threshold.upper = 1731.232              
  
  if (any(data$parameter=="Abundance") == FALSE) {
    message("Parameter Abundance not found")
  }
  if (any(data$value[which(data$parameter=="Abundance")] > extreme.threshold.upper)) {
    data_vis = data %>% 
      filter(parameter == "Abundance") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter Abundance, measurement(s) is outside range please check for outliers!")
    extreme.outliers = data %>%
      filter(parameter == "Abundance", value > extreme.threshold.upper) %>% 
      select(station_name, sample_date, sample_id, shark_sample_id_md5, sample_min_depth_m, sample_max_depth_m, value)
    print(kable(extreme.outliers))
  }
  else {
    data_vis = data %>% 
      filter(parameter == "Abundance") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter Abundance, measurement(s) is within range")
  }
}

check_zooplankton_counted <- function(data) {
  mild.threshold.upper = 54.5                                  
  extreme.threshold.upper = 86              
  
  if (any(data$parameter=="# counted") == FALSE) {
    message("Parameter # counted not found")
  }
  if (any(data$value[which(data$parameter=="# counted")] > extreme.threshold.upper)) {
    data_vis = data %>% 
      filter(parameter == "# counted") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter # counted, measurement(s) is outside range please check for outliers!")
    extreme.outliers = data %>%
      filter(parameter == "# counted", value > extreme.threshold.upper) %>% 
      select(station_name, sample_date, sample_id, shark_sample_id_md5, sample_min_depth_m, sample_max_depth_m, value)
    print(kable(extreme.outliers))
  }
  else {
    data_vis = data %>% 
      filter(parameter == "# counted") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter # counted, measurement(s) is within range")
  }
}

check_zooplankton_length_mean <- function(data) {
  mild.threshold.upper = 1286.662                                  
  extreme.threshold.upper = 1898.325              
  
  if (any(data$parameter=="Length (mean)") == FALSE) {
    message("Parameter Length (mean) not found")
  }
  if (any(data$value[which(data$parameter=="Length (mean)")] > extreme.threshold.upper)) {
    data_vis = data %>% 
      filter(parameter == "Length (mean)") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter Length (mean), measurement(s) is outside range please check for outliers!")
    extreme.outliers = data %>%
      filter(parameter == "Length (mean)", value > extreme.threshold.upper) %>% 
      select(station_name, sample_date, sample_id, shark_sample_id_md5, sample_min_depth_m, sample_max_depth_m, value)
    print(kable(extreme.outliers))
  }
  else {
    data_vis = data %>% 
      filter(parameter == "Length (mean)") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter Length (mean), measurement(s) is within range")
  }
}

check_zooplankton_length_median <- function(data) {
  mild.threshold.upper = 1287                                  
  extreme.threshold.upper = 1899              
  
  if (any(data$parameter=="Length (median)") == FALSE) {
    message("Parameter Length (median) not found")
  }
  if (any(data$value[which(data$parameter=="Length (median)")] > extreme.threshold.upper)) {
    data_vis = data %>% 
      filter(parameter == "Length (median)") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter Length (median), measurement(s) is outside range please check for outliers!")
    extreme.outliers = data %>%
      filter(parameter == "Length (median)", value > extreme.threshold.upper) %>% 
      select(station_name, sample_date, sample_id, shark_sample_id_md5, sample_min_depth_m, sample_max_depth_m, value)
    print(kable(extreme.outliers))
  }
  else {
    data_vis = data %>% 
      filter(parameter == "Length (median)") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter Length (median), measurement(s) is within range")
  }
}

check_zooplankton_wetweight <- function(data) {
  mild.threshold.upper = 0.82                                  
  extreme.threshold.upper = 1.3              
  
  if (any(data$parameter=="Wet weight") == FALSE) {
    message("Parameter Wet weight not found")
  }
  if (any(data$value[which(data$parameter=="Wet weight")] > extreme.threshold.upper)) {
    data_vis = data %>% 
      filter(parameter == "Wet weight") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter Wet weight, measurement(s) is outside range please check for outliers!")
    extreme.outliers = data %>%
      filter(parameter == "Wet weight", value > extreme.threshold.upper) %>% 
      select(station_name, sample_date, sample_id, shark_sample_id_md5, sample_min_depth_m, sample_max_depth_m, value)
    print(kable(extreme.outliers))
  }
  else {
    data_vis = data %>% 
      filter(parameter == "Wet weight") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter Wet weight, measurement(s) is within range")
  }
}

check_zooplankton_carbon <- function(data) {
  mild.threshold.upper = 3.88                                  
  extreme.threshold.upper = 6.16              
  
  if (any(data$parameter=="Carbon content") == FALSE) {
    message("Parameter Carbon content not found")
  }
  if (any(data$value[which(data$parameter=="Carbon content")] > extreme.threshold.upper)) {
    data_vis = data %>% 
      filter(parameter == "Carbon content") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter Carbon content, measurement(s) is outside range please check for outliers!")
    extreme.outliers = data %>%
      filter(parameter == "Carbon content", value > extreme.threshold.upper) %>% 
      select(station_name, sample_date, sample_id, shark_sample_id_md5, sample_min_depth_m, sample_max_depth_m, value)
    print(kable(extreme.outliers))
  }
  else {
    data_vis = data %>% 
      filter(parameter == "Carbon content") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter Carbon content, measurement(s) is within range")
  }
}

check_zooplankton_wetweight_volume <- function(data) {
  mild.threshold.upper = 9.816648                                  
  extreme.threshold.upper = 15.54263              
  
  if (any(data$parameter=="Wet weight/volume") == FALSE) {
    message("Parameter Wet weight/volume not found")
  }
  if (any(data$value[which(data$parameter=="Wet weight/volume")] > extreme.threshold.upper)) {
    data_vis = data %>% 
      filter(parameter == "Wet weight/volume") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter Wet weight/volume, measurement(s) is outside range please check for outliers!")
    extreme.outliers = data %>%
      filter(parameter == "Carbon content", value > extreme.threshold.upper) %>% 
      select(station_name, sample_date, sample_id, shark_sample_id_md5, sample_min_depth_m, sample_max_depth_m, value)
    print(kable(extreme.outliers))
  }
  else {
    data_vis = data %>% 
      filter(parameter == "Wet weight/volume") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter Wet weight/volume, measurement(s) is within range")
  }
}

check_zooplankton_wetweight_area <- function(data) {
  mild.threshold.upper = 372.6163                                  
  extreme.threshold.upper = 593.9886              
  
  if (any(data$parameter=="Wet weight/area") == FALSE) {
    message("Parameter Wet weight/area not found")
  }
  if (any(data$value[which(data$parameter=="Wet weight/area")] > extreme.threshold.upper)) {
    data_vis = data %>% 
      filter(parameter == "Wet weight/area") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter Wet weight/area, measurement(s) is outside range please check for outliers!")
    extreme.outliers = data %>%
      filter(parameter == "Wet weight/area", value > extreme.threshold.upper) %>% 
      select(station_name, sample_date, sample_id, shark_sample_id_md5, sample_min_depth_m, sample_max_depth_m, value)
    print(kable(extreme.outliers))
  }
  else {
    data_vis = data %>% 
      filter(parameter == "Wet weight/area") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter Wet weight/area, measurement(s) is within range")
  }
}

check_phytoplankton_abund <- function(data) {
  mild.threshold.upper = 39460                                  
  extreme.threshold.upper = 62920              
  
  if (any(data$parameter=="Abundance") == FALSE) {
    message("Parameter Abundance not found")
  }
  if (any(data$value[which(data$parameter=="Abundance")] > extreme.threshold.upper)) {
    data_vis = data %>% 
      filter(parameter == "Abundance") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter Abundance, measurement(s) is outside range please check for outliers!")
    extreme.outliers = data %>%
      filter(parameter == "Abundance", value > extreme.threshold.upper) %>% 
      select(station_name, sample_date, sample_id, shark_sample_id_md5, sample_min_depth_m, sample_max_depth_m, value)
    print(kable(extreme.outliers))
  }
  else {
    data_vis = data %>% 
      filter(parameter == "Abundance") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter Abundance, measurement(s) is within range")
  }
}

check_phytoplankton_biovol <- function(data) {
  mild.threshold.upper = 0.01514523                                  
  extreme.threshold.upper = 0.02397705              
  
  if (any(data$parameter=="Biovolume concentration") == FALSE) {
    message("Parameter Biovolume concentration not found")
  }
  if (any(data$value[which(data$parameter=="Biovolume concentration")] > extreme.threshold.upper)) {
    data_vis = data %>% 
      filter(parameter == "Biovolume concentration") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter Biovolume concentration, measurement(s) is outside range please check for outliers!")
    extreme.outliers = data %>%
      filter(parameter == "Biovolume concentration", value > extreme.threshold.upper) %>% 
      select(station_name, sample_date, sample_id, shark_sample_id_md5, sample_min_depth_m, sample_max_depth_m, value)
    print(kable(extreme.outliers))
  }
  else {
    data_vis = data %>% 
      filter(parameter == "Biovolume concentration") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter Biovolume concentration, measurement(s) is within range")
  }
}

check_phytoplankton_carbon <- function(data) {
  mild.threshold.upper = 1.679784                                  
  extreme.threshold.upper = 2.653602              
  
  if (any(data$parameter=="Carbon concentration") == FALSE) {
    message("Parameter Carbon concentration not found")
  }
  if (any(data$value[which(data$parameter=="Carbon concentration")] > extreme.threshold.upper)) {
    data_vis = data %>% 
      filter(parameter == "Carbon concentration") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter Carbon concentration, measurement(s) is outside range please check for outliers!")
    extreme.outliers = data %>%
      filter(parameter == "Carbon concentration", value > extreme.threshold.upper) %>% 
      select(station_name, sample_date, sample_id, shark_sample_id_md5, sample_min_depth_m, sample_max_depth_m, value)
    print(kable(extreme.outliers))
  }
  else {
    data_vis = data %>% 
      filter(parameter == "Carbon concentration") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter Carbon concentration, measurement(s) is within range")
  }
}

check_phytoplankton_carbon <- function(data) {
  mild.threshold.upper = 52                                  
  extreme.threshold.upper = 82              
  
  if (any(data$parameter=="# counted") == FALSE) {
    message("Parameter # counted not found")
  }
  if (any(data$value[which(data$parameter=="# counted")] > extreme.threshold.upper)) {
    data_vis = data %>% 
      filter(parameter == "# counted") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter # counted, measurement(s) is outside range please check for outliers!")
    extreme.outliers = data %>%
      filter(parameter == "# counted", value > extreme.threshold.upper) %>% 
      select(station_name, sample_date, sample_id, shark_sample_id_md5, sample_min_depth_m, sample_max_depth_m, value)
    print(kable(extreme.outliers))
  }
  else {
    data_vis = data %>% 
      filter(parameter == "# counted") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter # counted, measurement(s) is within range")
  }
}

check_primaryproduction_carbonprod <- function(data) {
  mild.threshold.upper = 36.6904                                  
  extreme.threshold.upper = 58.41079              
  
  if (any(data$parameter=="Carbon production") == FALSE) {
    message("Parameter Carbon production not found")
  }
  if (any(data$value[which(data$parameter=="Carbon production")] > extreme.threshold.upper)) {
    data_vis = data %>% 
      filter(parameter == "Carbon production") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter Carbon production, measurement(s) is outside range please check for outliers!")
    extreme.outliers = data %>%
      filter(parameter == "Carbon production", value > extreme.threshold.upper) %>% 
      select(station_name, sample_date, sample_id, shark_sample_id_md5, sample_min_depth_m, sample_max_depth_m, value)
    print(kable(extreme.outliers))
  }
  else {
    data_vis = data %>% 
      filter(parameter == "Carbon production") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter Carbon production, measurement(s) is within range")
  }
}

check_primaryproduction_carbonprodlight <- function(data) {
  mild.threshold.upper = 36.6904                                  
  extreme.threshold.upper = 58.41079              
  
  if (any(data$parameter=="Carbon prod in light") == FALSE) {
    message("Parameter Carbon prod in light not found")
  }
  if (any(data$value[which(data$parameter=="Carbon prod in light")] > extreme.threshold.upper)) {
    data_vis = data %>% 
      filter(parameter == "Carbon prod in light") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter Carbon prod in light, measurement(s) is outside range please check for outliers!")
    extreme.outliers = data %>%
      filter(parameter == "Carbon prod in light", value > extreme.threshold.upper) %>% 
      select(station_name, sample_date, sample_id, shark_sample_id_md5, sample_min_depth_m, sample_max_depth_m, value)
    print(kable(extreme.outliers))
  }
  else {
    data_vis = data %>% 
      filter(parameter == "Carbon prod in light") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter Carbon prod in light, measurement(s) is within range")
  }
}

check_primaryproduction_carbonprod_hour <- function(data) {
  mild.threshold.upper = 11.86375                                  
  extreme.threshold.upper = 18.6775              
  
  if (any(data$parameter=="Carbon production/hour") == FALSE) {
    message("Parameter Carbon production/hour not found")
  }
  if (any(data$value[which(data$parameter=="Carbon production/hour")] > extreme.threshold.upper)) {
    data_vis = data %>% 
      filter(parameter == "Carbon production/hour") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter Carbon production/hour, measurement(s) is outside range please check for outliers!")
    extreme.outliers = data %>%
      filter(parameter == "Carbon production/hour", value > extreme.threshold.upper) %>% 
      select(station_name, sample_date, sample_id, shark_sample_id_md5, sample_min_depth_m, sample_max_depth_m, value)
    print(kable(extreme.outliers))
  }
  else {
    data_vis = data %>% 
      filter(parameter == "Carbon production/hour") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter Carbon production/hour, measurement(s) is within range")
  }
}

check_epibenthos_counted <- function(data) {
  mild.threshold.upper = 87                                  
  extreme.threshold.upper = 138              
  
  if (any(data$parameter=="# counted") == FALSE) {
    message("Parameter # counted not found")
  }
  if (any(data$value[which(data$parameter=="# counted")] > extreme.threshold.upper)) {
    data_vis = data %>% 
      filter(parameter == "# counted") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter # counted, measurement(s) is outside range please check for outliers!")
    extreme.outliers = data %>%
      filter(parameter == "# counted", value > extreme.threshold.upper) %>% 
      select(station_name, sample_date, sample_id, shark_sample_id_md5, sample_min_depth_m, sample_max_depth_m, value)
    print(kable(extreme.outliers))
  }
  else {
    data_vis = data %>% 
      filter(parameter == "# counted") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter # counted, measurement(s) is within range")
  }
}

check_epibenthos_dryweight <- function(data) {
  mild.threshold.upper = 0.2303094                                  
  extreme.threshold.upper = 0.367895              
  
  if (any(data$parameter=="Dry weight") == FALSE) {
    message("Parameter Dry weight not found")
  }
  if (any(data$value[which(data$parameter=="Dry weight")] > extreme.threshold.upper)) {
    data_vis = data %>% 
      filter(parameter == "Dry weight") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter Dry weight, measurement(s) is outside range please check for outliers!")
    extreme.outliers = data %>%
      filter(parameter == "Dry weight", value > extreme.threshold.upper) %>% 
      select(station_name, sample_date, sample_id, shark_sample_id_md5, sample_min_depth_m, sample_max_depth_m, value)
    print(kable(extreme.outliers))
  }
  else {
    data_vis = data %>% 
      filter(parameter == "Dry weight") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter Dry weight, measurement(s) is within range")
  }
}

check_epibenthos_specdistr_maxdepth <- function(data) {
  mild.threshold.upper = 29.3125                                  
  extreme.threshold.upper = 44.425              
  
  if (any(data$parameter=="Species distribution max depth") == FALSE) {
    message("Parameter Species distribution max depth not found")
  }
  if (any(data$value[which(data$parameter=="Species distribution max depth")] > extreme.threshold.upper)) {
    data_vis = data %>% 
      filter(parameter == "Species distribution max depth") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter Species distribution max depth, measurement(s) is outside range please check for outliers!")
    extreme.outliers = data %>%
      filter(parameter == "Dry weight", value > extreme.threshold.upper) %>% 
      select(station_name, sample_date, sample_id, shark_sample_id_md5, sample_min_depth_m, sample_max_depth_m, value)
    print(kable(extreme.outliers))
  }
  else {
    data_vis = data %>% 
      filter(parameter == "Species distribution max depth") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter Species distribution max depth, measurement(s) is within range")
  }
}

check_epibenthos_specdistr_mindepth <- function(data) {
  mild.threshold.upper = 13.075                                  
  extreme.threshold.upper = 20.65              
  
  if (any(data$parameter=="Species distribution min depth") == FALSE) {
    message("Parameter Species distribution min depth not found")
  }
  if (any(data$value[which(data$parameter=="Species distribution min depth")] > extreme.threshold.upper)) {
    data_vis = data %>% 
      filter(parameter == "Species distribution min depth") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter Species distribution min depth, measurement(s) is outside range please check for outliers!")
    extreme.outliers = data %>%
      filter(parameter == "Species distribution min depth", value > extreme.threshold.upper) %>% 
      select(station_name, sample_date, sample_id, shark_sample_id_md5, sample_min_depth_m, sample_max_depth_m, value)
    print(kable(extreme.outliers))
  }
  else {
    data_vis = data %>% 
      filter(parameter == "Species distribution min depth") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter Species distribution min depth, measurement(s) is within range")
  }
}

check_harbourseal_counted <- function(data) {
  mild.threshold.upper = 162.5                                  
  extreme.threshold.upper = 260              
  
  if (any(data$parameter=="# counted") == FALSE) {
    message("Parameter # counted not found")
  }
  if (any(data$value[which(data$parameter=="# counted")] > extreme.threshold.upper)) {
    data_vis = data %>% 
      filter(parameter == "# counted") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter # counted, measurement(s) is outside range please check for outliers!")
    extreme.outliers = data %>%
      filter(parameter == "Species distribution min depth", value > extreme.threshold.upper) %>% 
      select(station_name, sample_date, sample_id, shark_sample_id_md5, sample_min_depth_m, sample_max_depth_m, value)
    print(kable(extreme.outliers))
  }
  else {
    data_vis = data %>% 
      filter(parameter == "# counted") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter # counted, measurement(s) is within range")
  }
}

check_greyseal_counted <- function(data) {
  mild.threshold.upper = 397.25                                  
  extreme.threshold.upper = 632              
  
  if (any(data$parameter=="# counted") == FALSE) {
    message("Parameter # counted not found")
  }
  if (any(data$value[which(data$parameter=="# counted")] > extreme.threshold.upper)) {
    data_vis = data %>% 
      filter(parameter == "# counted") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter # counted, measurement(s) is outside range please check for outliers!")
    extreme.outliers = data %>%
      filter(parameter == "Species distribution min depth", value > extreme.threshold.upper) %>% 
      select(station_name, sample_date, sample_id, shark_sample_id_md5, sample_min_depth_m, sample_max_depth_m, value)
    print(kable(extreme.outliers))
  }
  else {
    data_vis = data %>% 
      filter(parameter == "# counted") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter # counted, measurement(s) is within range")
  }
}

check_zoobenthos_BQIm <- function(data) {
  mild.threshold.upper = 18.52294                                  
  extreme.threshold.upper = 26.96423              
  
  if (any(data$parameter=="BQIm") == FALSE) {
    message("Parameter BQIm not found")
  }
  if (any(data$value[which(data$parameter=="BQIm")] > extreme.threshold.upper)) {
    data_vis = data %>% 
      filter(parameter == "BQIm") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter BQIm, measurement(s) is outside range please check for outliers!")
    extreme.outliers = data %>%
      filter(parameter == "BQIm", value > extreme.threshold.upper) %>% 
      select(station_name, sample_date, sample_id, shark_sample_id_md5, sample_min_depth_m, sample_max_depth_m, value)
    print(kable(extreme.outliers))
  }
  else {
    data_vis = data %>% 
      filter(parameter == "BQIm") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter BQIm, measurement(s) is within range")
  }
}

check_zoobenthos_abund <- function(data) {
  mild.threshold.upper = 185                                                    
  extreme.threshold.upper = 290              
  
  if (any(data$parameter=="Abundance") == FALSE) {
    message("Parameter Abundance not found")
  }
  if (any(data$value[which(data$parameter=="Abundance")] > extreme.threshold.upper)) {
    data_vis = data %>% 
      filter(parameter == "Abundance") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter Abundance, measurement(s) is outside range please check for outliers!")
    extreme.outliers = data %>%
      filter(parameter == "Abundance", value > extreme.threshold.upper) %>% 
      select(station_name, sample_date, sample_id, shark_sample_id_md5, sample_min_depth_m, sample_max_depth_m, value)
    print(kable(extreme.outliers))
  }
  else {
    data_vis = data %>% 
      filter(parameter == "Abundance") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter Abundance, measurement(s) is within range")
  }
}

check_zoobenthos_counted <- function(data) {
  mild.threshold.upper = 21                                                    
  extreme.threshold.upper = 33              
  
  if (any(data$parameter=="# counted") == FALSE) {
    message("Parameter # counted not found")
  }
  if (any(data$value[which(data$parameter=="# counted")] > extreme.threshold.upper)) {
    data_vis = data %>% 
      filter(parameter == "# counted") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter # counted, measurement(s) is outside range please check for outliers!")
    extreme.outliers = data %>%
      filter(parameter == "# counted", value > extreme.threshold.upper) %>% 
      select(station_name, sample_date, sample_id, shark_sample_id_md5, sample_min_depth_m, sample_max_depth_m, value)
    print(kable(extreme.outliers))
  }
  else {
    data_vis = data %>% 
      filter(parameter == "# counted") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter # counted, measurement(s) is within range")
  }
}

check_zoobenthos_wetweight <- function(data) {
  mild.threshold.upper = 0.5395                                                    
  extreme.threshold.upper = 0.859              
  
  if (any(data$parameter=="Wet weight") == FALSE) {
    message("Parameter Wet weight not found")
  }
  if (any(data$value[which(data$parameter=="Wet weight")] > extreme.threshold.upper)) {
    data_vis = data %>% 
      filter(parameter == "Wet weight") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter Wet weight, measurement(s) is outside range please check for outliers!")
    extreme.outliers = data %>%
      filter(parameter == "# counted", value > extreme.threshold.upper) %>% 
      select(station_name, sample_date, sample_id, shark_sample_id_md5, sample_min_depth_m, sample_max_depth_m, value)
    print(kable(extreme.outliers))
  }
  else {
    data_vis = data %>% 
      filter(parameter == "Wet weight") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter Wet weight, measurement(s) is within range")
  }
}

check_ringedseal_calccounted <- function(data) {
  mild.threshold.upper = 28.247                                                    
  extreme.threshold.upper = 41.6792              
  
  if (any(data$parameter=="Calculated # counted") == FALSE) {
    message("Parameter Calculated # counted not found")
  }
  if (any(data$value[which(data$parameter=="Calculated # counted")] > extreme.threshold.upper)) {
    data_vis = data %>% 
      filter(parameter == "Calculated # counted") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter Calculated # counted, measurement(s) is outside range please check for outliers!")
    extreme.outliers = data %>%
      filter(parameter == "# counted", value > extreme.threshold.upper) %>% 
      select(station_name, sample_date, sample_id, shark_sample_id_md5, sample_min_depth_m, sample_max_depth_m, value)
    print(kable(extreme.outliers))
  }
  else {
    data_vis = data %>% 
      filter(parameter == "Calculated # counted") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter Calculated # counted, measurement(s) is within range")
  }
}

check_harbporp_positivemin <- function(data) {
  mild.threshold.upper = 189.5                                                    
  extreme.threshold.upper = 299              
  
  if (any(data$parameter=="Porpoise positive minutes") == FALSE) {
    message("Parameter Porpoise positive minutes not found")
  }
  if (any(data$value[which(data$parameter=="Porpoise positive minutes")] > extreme.threshold.upper)) {
    data_vis = data %>% 
      filter(parameter == "Porpoise positive minutes") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter Porpoise positive minutes, measurement(s) is outside range please check for outliers!")
    extreme.outliers = data %>%
      filter(parameter == "# counted", value > extreme.threshold.upper) %>% 
      select(station_name, sample_date, sample_id, shark_sample_id_md5, sample_min_depth_m, sample_max_depth_m, value)
    print(kable(extreme.outliers))
  }
  else {
    data_vis = data %>% 
      filter(parameter == "Porpoise positive minutes") %>% 
      select(value)
    p = ggplot(data_vis,aes(y = value))+
      geom_boxplot(outlier.colour = "red")+
      geom_hline(yintercept = mild.threshold.upper, colour = "yellow")+
      geom_hline(yintercept = extreme.threshold.upper, colour = "red")+
      ggtitle(label = NULL, subtitle = "yellow line = mild outlier \n red line = extreme outlier")+

      
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    print(p)
    message("Parameter Porpoise positive minutes, measurement(s) is within range")
  }
}