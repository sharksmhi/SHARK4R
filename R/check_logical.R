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
    return(DT::datatable(matches))
  }
  else {
    message("Expected numerical value is formatted as numerical and no character values were found")
  }
}

#' Check logical assumptions of data for specific variables and parameters
#' @param data for tibble be be checked
#' @return logical test with true or false results 
#' @export

check_zero_value <- function(data) {
  
  if (any(data$value == 0)) {
    message("ERROR: Value contain zeroes (0). Please check zero values!")
    zero_values = data %>%
      filter(value == 0) %>% 
      select(station_name, sample_date, sample_id, shark_sample_id_md5, sample_min_depth_m, sample_max_depth_m, value)
    return(DT::datatable(zero_values))
  }
  else {
    message("No zero values were found")
  }
}

#' Check logical assumptions of data for station positions
#' @param data for tibble be be checked
#' @return logical test with true or false results 
#' @export

check_zero_positions <- function(data) {
  
  if (any(data$sample_longitude_dd == 0)) {
    message("ERROR: Positions contain zeroes (0). Please check station coordinates with zero values!")
    zero_positions = data %>%
      filter(value == 0) %>% 
      select(station_name, sample_date, sample_id, shark_sample_id_md5, sample_min_depth_m, sample_max_depth_m, value)
    return(DT::datatable(zero_positions))
  }
  else {
    message("No zero positions were found")
  }
}

#' Check logical assumptions of data for specific variables and parameters
#' @param data for tibble be be checked
#' @return logical test with true or false results 
#' @export
#' @importFrom DT datatable
check_epibenthos_totcover_logical <- function(data) {

  if (any(data$value[which(data$parameter=="Total cover of all species (%)")] > 100)) {
    message("Parameter Total cover of all species (%), measurement(s) is not as expected between 0-100 %")
    logical.error = data %>%
      filter(parameter == "Total cover of all species (%)", value > 100) %>% 
      select(station_name, sample_date, sample_id, shark_sample_id_md5, sample_min_depth_m, sample_max_depth_m, value)
    return(DT::datatable(logical.error))
  }
  else {
    message("Parameter Total cover of all species (%), measurement(s) is between 0-100%")
  }
}
#' Check logical assumptions of data for specific variables and parameters
#' @param data for tibble be be checked
#' @return logical test with true or false results 
#' @export
check_epibenthos_coverpercent_logical <- function(data) {
  
  if (any(data$value[which(data$parameter=="Cover (%)")] > 100)) {
    message("Parameter Cover (%), measurement(s) is not as expected between 0-100 %")
    logical.error = data %>%
      filter(parameter == "Cover (%)", value > 100) %>% 
      select(station_name, sample_date, sample_id, shark_sample_id_md5, sample_min_depth_m, sample_max_depth_m, value)
    return(DT:datatable(logical.error))
  }
  else {
    message("Parameter Cover (%), measurement(s) is between 0-100%")
  }
}
#' Check logical assumptions of data for specific variables and parameters
#' @param data for tibble be be checked
#' @return logical test with true or false results 
#' @export
check_epibenthos_cover_logical <- function(data) {
  
  if (any(data$value[which(data$parameter=="Cover")] > 100)) {
    message("Parameter Cover, measurement(s) is not as expected between 0-100 %")
    logical.error = data %>%
      filter(parameter == "Cover", value > 100) %>% 
      select(station_name, sample_date, sample_id, shark_sample_id_md5, sample_min_depth_m, sample_max_depth_m, value)
    return(DT::datatable(logical.error))
  }
  else {
    message("Parameter Cover, measurement(s) is between 0-100%")
  }
}
#' Check logical assumptions of data for specific variables and parameters
#' @param data for tibble be be checked
#' @return logical test with true or false results 
#' @export
check_epibenthos_coverclass_logical <- function(data) {
  
  if (any(data$value[which(data$parameter=="Cover class")] > 10)) {
    message("Parameter Cover class, measurement(s) is not as expected between 0-10")
    logical.error = data %>%
      filter(parameter == "Cover class", value > 10) %>% 
      select(station_name, sample_date, sample_id, shark_sample_id_md5, sample_min_depth_m, sample_max_depth_m, value)
    return(DT::datatable(logical.error))
  }
  else {
    message("Parameter Cover class, measurement(s) is between 0-10")
  }
}
#' Check logical assumptions of data for specific variables and parameters
#' @param data for tibble be be checked
#' @return logical test with true or false results 
#' @export
check_epibenthos_sedimentdepos_logical <- function(data) {
  
  if (any(data$value[which(data$parameter=="Sediment deposition cover (%)")] > 100)) {
    message("Parameter Sediment deposition cover (%), measurement(s) is not as expected between 0-100 %")
    logical.error = data %>%
      filter(parameter == "Sediment deposition cover (%)", value > 100) %>% 
      select(station_name, sample_date, sample_id, shark_sample_id_md5, sample_min_depth_m, sample_max_depth_m, value)
    return(DT::datatable(logical.error))
  }
  else {
    message("Parameter Sediment deposition cover (%), measurement(s) is between 0-100%")
  }
}

#' Check logical assumptions of data for specific variables and parameters
#' @param data for tibble be be checked
#' @return logical test with true or false results 
#' @export
check_epibenthos_abundclass_logical <- function(data) {
  
  if (any(data$value[which(data$parameter=="Abundance class")] > 10)) {
    message("Parameter Abundance class, measurement(s) is not as expected between 0-10")
    logical.error = data %>%
      filter(parameter == "Abundance class", value > 10) %>% 
      select(station_name, sample_date, sample_id, shark_sample_id_md5, sample_min_depth_m, sample_max_depth_m, value)
    return(DT::datatable(logical.error))
  }
  else {
    message("Parameter Abundance class, measurement(s) is between 0-10")
  }
}

#' Check logical assumptions of data for specific variables and parameters
#' @param data for tibble be be checked
#' @return logical test with true or false results 
#' @export
check_zoobenthos_BQIm_logical <- function(data) {
  
  if (data$value[which(data$parameter=="Abundance")] == 0 & data$value[which(data$parameter=="BQIm")] > 0) {
    message("Parameter BQIm, measurement(s) does not follow logical assumption of abundance < 0 then BQIm should be <0")
    logical.error = data %>%
      filter(parameter == "Abundance", value == 0) %>% 
      filter(parameter == "BQIm", value > 0) %>% 
      select(station_name, sample_date, sample_id, shark_sample_id_md5, sample_min_depth_m, sample_max_depth_m, value)
    return(DT::datatable(logical.error))
  }
  else {
    message("Parameter BQIm, measurement(s) follow logical assumption of abundance < 0 then BQIm should be < 0")
  }
}

#' Check logical assumptions of data for specific variables and parameters
#' @param data for tibble be be checked
#' @return logical test with true or false results 
#' @export
check_zoobenthos_wetweight_logical <- function(data) {
  
  if (any(data$value[which(data$parameter=="Wet weight")] == 0)) {
    message("Parameter Wet weight, measurement(s) does not follow logical assumption of wet weight should not be = 0")
    logical.error = data %>%
      filter(parameter == "Wet weight", value == 0) %>% 
      select(station_name, sample_date, sample_id, shark_sample_id_md5, sample_min_depth_m, sample_max_depth_m, value)
    return(DT::datatable(logical.error))
  }
  else {
    message("Parameter Wet weight, measurement(s) follow logical assumption of wet weight should not be = 0")
  }
}