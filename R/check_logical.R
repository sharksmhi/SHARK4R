#' Check logical assumptions of data for specific variables and parameters
#' @param data for tibble be be checked
#' @return logical test with true or false results 
#' @export

check_value_logical <- function(data) {
  
  if (any(is.character(data$value)==TRUE)) {
    message("ERROR: Expected numerical value is formatted as character")
    message("Common characters are e.g. less than "<" sign:")
    toMatch <- c("<", ">", "[A-z]")
    matches <- unique(grep(paste(toMatch,collapse="|"), 
                            data$value, value=TRUE))
    if (length(matches) > 0) {
      print(matches)
    }
    else {
      message("Common characters not found, please check other characters or errors")
    }
    logical.error = data %>%
      filter(parameter == "value") %>% 
      is_character("value") %>% 
      select(station_name, sample_date, sample_id, shark_sample_id_md5, sample_min_depth_m, sample_max_depth_m, value)
    print(kable(logical.error))
  }
  else {
    message("Expected numerical value is formatted as numerical and no character values were found")
  }
}

check_epibenthos_totcover_logical <- function(data) {

  if (any(data$parameter=="Total cover of all species (%)") == FALSE) {
    message("Parameter Total cover of all species (%) not found")
  }
  if (any(data$value[which(data$parameter=="Total cover of all species (%)")] > 100)) {
    message("Parameter Total cover of all species (%), measurement(s) is not as expected between 0-100 %")
    logical.error = data %>%
      filter(parameter == "Total cover of all species (%)", value > 100) %>% 
      select(station_name, sample_date, sample_id, shark_sample_id_md5, sample_min_depth_m, sample_max_depth_m, value)
    print(kable(logical.error))
  }
  else {
    message("Parameter Total cover of all species (%), measurement(s) is between 0-100%")
  }
}

check_epibenthos_coverpercent_logical <- function(data) {
  
  if (any(data$parameter=="Cover (%)") == FALSE) {
    message("Parameter Cover (%) not found")
  }
  if (any(data$value[which(data$parameter=="Cover (%)")] > 100)) {
    message("Parameter Cover (%), measurement(s) is not as expected between 0-100 %")
    logical.error = data %>%
      filter(parameter == "Cover (%)", value > 100) %>% 
      select(station_name, sample_date, sample_id, shark_sample_id_md5, sample_min_depth_m, sample_max_depth_m, value)
    print(kable(logical.error))
  }
  else {
    message("Parameter Cover (%), measurement(s) is between 0-100%")
  }
}

check_epibenthos_cover_logical <- function(data) {
  
  if (any(data$parameter=="Cover") == FALSE) {
    message("Parameter Cover not found")
  }
  if (any(data$value[which(data$parameter=="Cover")] > 100)) {
    message("Parameter Cover, measurement(s) is not as expected between 0-100 %")
    logical.error = data %>%
      filter(parameter == "Cover", value > 100) %>% 
      select(station_name, sample_date, sample_id, shark_sample_id_md5, sample_min_depth_m, sample_max_depth_m, value)
    print(kable(logical.error))
  }
  else {
    message("Parameter Cover, measurement(s) is between 0-100%")
  }
}

check_epibenthos_coverclass_logical <- function(data) {
  
  if (any(data$parameter=="Cover class") == FALSE) {
    message("Parameter Cover class not found")
  }
  if (any(data$value[which(data$parameter=="Cover class")] > 10)) {
    message("Parameter Cover class, measurement(s) is not as expected between 0-10")
    logical.error = data %>%
      filter(parameter == "Cover class", value > 10) %>% 
      select(station_name, sample_date, sample_id, shark_sample_id_md5, sample_min_depth_m, sample_max_depth_m, value)
    print(kable(logical.error))
  }
  else {
    message("Parameter Cover class, measurement(s) is between 0-10")
  }
}

check_epibenthos_sedimentdepos_logical <- function(data) {
  
  if (any(data$parameter=="Sediment deposition cover (%)") == FALSE) {
    message("Parameter Sediment deposition cover (%) not found")
  }
  if (any(data$value[which(data$parameter=="Sediment deposition cover (%)")] > 100)) {
    message("Parameter Sediment deposition cover (%), measurement(s) is not as expected between 0-100 %")
    logical.error = data %>%
      filter(parameter == "Sediment deposition cover (%)", value > 100) %>% 
      select(station_name, sample_date, sample_id, shark_sample_id_md5, sample_min_depth_m, sample_max_depth_m, value)
    print(kable(logical.error))
  }
  else {
    message("Parameter Sediment deposition cover (%), measurement(s) is between 0-100%")
  }
}

check_epibenthos_abundclass_logical <- function(data) {
  
  if (any(data$parameter=="Abundance class") == FALSE) {
    message("Parameter Abundance class not found")
  }
  if (any(data$value[which(data$parameter=="Abundance class")] > 10)) {
    message("Parameter Abundance class, measurement(s) is not as expected between 0-10")
    logical.error = data %>%
      filter(parameter == "Abundance class", value > 10) %>% 
      select(station_name, sample_date, sample_id, shark_sample_id_md5, sample_min_depth_m, sample_max_depth_m, value)
    print(kable(logical.error))
  }
  else {
    message("Parameter Abundance class, measurement(s) is between 0-10")
  }
}

check_zoobenthos_BQIm_logical <- function(data) {
  
  if (any(data$parameter=="BQIm") == FALSE) {
    message("Parameter BQIm not found")
  }
  if (data$value[which(data$parameter=="Abundance")] == 0 & data$value[which(data$parameter=="BQIm")] > 0) {
    message("Parameter BQIm, measurement(s) does not follow logical assumption of abundance < 0 then BQIm should be <0")
    logical.error = data %>%
      filter(parameter == "Abundance", value == 0) %>% 
      filter(parameter == "BQIm", value > 0) %>% 
      select(station_name, sample_date, sample_id, shark_sample_id_md5, sample_min_depth_m, sample_max_depth_m, value)
    print(kable(logical.error))
  }
  else {
    message("Parameter BQIm, measurement(s) follow logical assumption of abundance < 0 then BQIm should be < 0")
  }
}

check_zoobenthos_wetweight_logical <- function(data) {
  
  if (any(data$parameter=="Wet weight") == FALSE) {
    message("Parameter Wet weight not found")
  }
  if (any(data$value[which(data$parameter=="Wet weight")] == 0)) {
    message("Parameter Wet weight, measurement(s) does not follow logical assumption of wet weight should not be = 0")
    logical.error = data %>%
      filter(parameter == "Wet weight", value == 0) %>% 
      select(station_name, sample_date, sample_id, shark_sample_id_md5, sample_min_depth_m, sample_max_depth_m, value)
    print(kable(logical.error))
  }
  else {
    message("Parameter Wet weight, measurement(s) follow logical assumption of wet weight should not be = 0")
  }
}