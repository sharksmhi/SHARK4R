#' Check if the required and recommended global and datatype-specific SHARK system fields (different between different datatypes) are present.
#'
#' Missing or empty required fields are reported as errors,
#' missing or empty recommended fields are reported as warnings.
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export

check_datatype <- function(data, level = "error") {
  
  errors <- tibble()
  required <- c("visit_year", "station_name", "sample_project_name_sv", "sample_orderer_name_sv", "platform_code", "sample_date", "sample_latitude_dd", "sample_longitude_dd", "positioning_system_code", "water_depth_m")
  recommended <- c("monitoring_station_type_code", "monitoring_purpose_code", "monitoring_program_code", "reporting_institute_name_sv", "analytical_laboratory_name_sv")
  
  # find missing required fields
  
  fields <- missing_fields(data, required)
  if (length(fields) > 0) {
    errors <- bind_rows(errors, data_frame(
      level = "error",
      field = fields,
      row = NA,
      message = paste0("Required field ", fields, " is missing")
    ))
  }
  else {
    message("All required fields present")
  }
  
  # find empty values for required fields
  
  for (field in required) {
    if (field %in% names(data)) {
      rows <- missing_values(data[,field])
      if (length(which(rows)) > 0) {
        errors <- bind_rows(errors, data_frame(
          level = "error",
          field = field,
          row = which(rows),
          message = paste0("Empty value for required field ", field)
        ))
      }
    }
  }
  
  # recommended fields
  
  if (level == "warning") {
    
    # find missing recommended fields
    
    fields <- missing_fields(data, recommended)
    if (length(fields) > 0) {
      errors <- bind_rows(errors, data_frame(
        field = fields,
        level = "warning",
        message = paste0("Recommended field ", fields, " is missing")
      ))
    }
    else {
      message("All recommended fields present")
    }
    
    # find empty values for recommended fields
    
    for (field in recommended) {
      if (field %in% names(data)) {
        rows <- missing_values(data[,field])
        if (length(which(rows)) > 0) {
          errors <- bind_rows(errors, data_frame(
            level = "warning",
            field = field,
            row = which(rows),
            message = paste0("Empty value for recommended field ", field)
          ))
        }
      }
    }
    
  }
  
  return(errors)
}

#' Check if the required and recommended datatype-specific SHARK system fields (different between different datatypes) are present.
#'
#' Missing or empty required fields are reported as errors,
#' missing or empty recommended fields are reported as warnings.
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export

check_Bacterioplankton <- function(data, level = "error") {
  
  errors <- tibble()
  required <- c("visit_year","station_name","sample_project_name_sv","sample_orderer_name_sv","platform_code","sample_date","sample_time","sample_latitude_dd","sample_longitude_dd","water_depth_m","sample_min_depth_m","sample_max_depth_m","sampling_laboratory_name_sv","sampling_laboratory_accreditated","sampler_type_code","sampled_volume_l","scientific_name","value","quality_flag","analysis_method_code","method_reference_code","analytical_laboratory_name_sv","analytical_laboratory_accreditated","analysed_volume_cm3","preservation_method_code","counted_portions","reporting_institute_name_sv")
  recommended <- c("monitoring_program_code")
  
  # find missing required fields
  
  fields <- missing_fields(data, required)
  if (length(fields) > 0) {
    errors <- bind_rows(errors, data_frame(
      level = "error",
      field = fields,
      row = NA,
      message = paste0("Required field ", fields, " is missing")
    ))
  }
  
  # find empty values for required fields
  
  for (field in required) {
    if (field %in% names(data)) {
      rows <- missing_values(data[,field])
      if (length(which(rows)) > 0) {
        errors <- bind_rows(errors, data_frame(
          level = "error",
          field = field,
          row = which(rows),
          message = paste0("Empty value for required field ", field)
        ))
      }
    }
  }
  
  # recommended fields
  
  if (level == "warning") {
    
    # find missing recommended fields
    
    fields <- missing_fields(data, recommended)
    if (length(fields) > 0) {
      errors <- bind_rows(errors, data_frame(
        field = fields,
        level = "warning",
        message = paste0("Recommended field ", fields, " is missing")
      ))
    }
    
    # find empty values for recommended fields
    
    for (field in recommended) {
      if (field %in% names(data)) {
        rows <- missing_values(data[,field])
        if (length(which(rows)) > 0) {
          errors <- bind_rows(errors, data_frame(
            level = "warning",
            field = field,
            row = which(rows),
            message = paste0("Empty value for recommended field ", field)
          ))
        }
      }
    }
    
  }
  
  return(errors)
}

#' Check if the required and recommended datatype-specific SHARK system fields (different between different datatypes) are present.
#'
#' Missing or empty required fields are reported as errors,
#' missing or empty recommended fields are reported as warnings.
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export

check_Chlorophyll <- function(data, level = "error") {
  
  errors <- tibble()
  required <- c("visit_year","station_name","reported_station_name","sample_project_name_sv","sample_orderer_name_sv","platform_code","sample_date","sample_time","sample_enddate","sample_endtime","sample_latitude_dd","sample_longitude_dd","positioning_system_code","water_depth_m","secchi_depth_m","secchi_depth_quality_flag","sample_min_depth_m","sample_max_depth_m","sampling_laboratory_name_sv","sampling_laboratory_accreditated","sampler_type_code","sampled_volume_l","value","quality_flag","analysis_method_code","method_documentation","method_reference_code","estimation_uncertainty","method_calculation_uncertainty","quantification_limit","detection_limit","analysis_range","analytical_laboratory_name_sv","analytical_laboratory_accreditated","analysis_date","reporting_institute_name_sv")
  recommended <- c("monitoring_purpose_code","monitoring_program_code")
  
  # find missing required fields
  
  fields <- missing_fields(data, required)
  if (length(fields) > 0) {
    errors <- bind_rows(errors, data_frame(
      level = "error",
      field = fields,
      row = NA,
      message = paste0("Required field ", fields, " is missing")
    ))
  }
  
  # find empty values for required fields
  
  for (field in required) {
    if (field %in% names(data)) {
      rows <- missing_values(data[,field])
      if (length(which(rows)) > 0) {
        errors <- bind_rows(errors, data_frame(
          level = "error",
          field = field,
          row = which(rows),
          message = paste0("Empty value for required field ", field)
        ))
      }
    }
  }
  
  # recommended fields
  
  if (level == "warning") {
    
    # find missing recommended fields
    
    fields <- missing_fields(data, recommended)
    if (length(fields) > 0) {
      errors <- bind_rows(errors, data_frame(
        field = fields,
        level = "warning",
        message = paste0("Recommended field ", fields, " is missing")
      ))
    }
    
    # find empty values for recommended fields
    
    for (field in recommended) {
      if (field %in% names(data)) {
        rows <- missing_values(data[,field])
        if (length(which(rows)) > 0) {
          errors <- bind_rows(errors, data_frame(
            level = "warning",
            field = field,
            row = which(rows),
            message = paste0("Empty value for recommended field ", field)
          ))
        }
      }
    }
    
  }
  
  return(errors)
}

#' Check if the required and recommended datatype-specific SHARK system fields (different between different datatypes) are present.
#'
#' Missing or empty required fields are reported as errors,
#' missing or empty recommended fields are reported as warnings.
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export

check_Epibenthos <- function(data, level = "error") {
  
  errors <- tibble()
  required <- c("visit_year","station_name","reported_station_name","sample_project_name_sv","sample_orderer_name_sv","platform_code","sample_date","sample_latitude_dd","sample_longitude_dd","positioning_system_code","sample_min_depth_m","sample_max_depth_m","sampling_laboratory_name_sv","sampler_type_code","sampler_area_m2","sampler_area_cm2","scientific_name","dyntaxa_id","value","size_class","analysis_method_code","method_documentation","image_id","sediment_deposition_code","video_interpreted","sample_photo_code","variable_comment","analytical_laboratory_name_sv","analysed_by","transect_start_latitude_dd","transect_start_longitude_dd","transect_end_latitude_dd","transect_end_longitude_dd","transect_max_depth_m","transect_min_depth_m","transect_max_distance_m","transect_min_distance_m","transect_video","transect_width_m","sample_substrate_cover_boulder","sample_substrate_comnt_boulder","sample_substrate_cover_rock","sample_substrate_comnt_rock","sample_substrate_cover_softbottom","sample_substrate_comnt_softbottom","sample_substrate_cover_stone","sample_substrate_comnt_stone","sample_substrate_cover_gravel","sample_substrate_comnt_gravel","sample_substrate_cover_sand","sample_substrate_comnt_sand","section_bare_substrate","section_comment","section_substrate_cover_boulder","section_substrate_comnt_boulder","section_substrate_cover_gravel","section_substrate_comnt_gravel","section_substrate_cover_rock","section_substrate_comnt_rock","section_substrate_cover_sand","section_substrate_comnt_sand","section_substrate_cover_softbottom","section_substrate_comnt_softbottom","section_substrate_cover_stone","section_substrate_comnt_stone","section_debris_cover","section_start_latitude_dd","section_start_longitude_dd","section_end_latitude_dd","section_end_longitude_dd","section_distance_start_m","section_distance_end_m","section_fauna_flora_found","section_start_depth_m","section_end_depth_m","reported_scientific_name","reported_value","reporting_institute_name_sv")
  recommended <- c("monitoring_station_type_code","monitoring_purpose_code","monitoring_program_code")
  
  # find missing required fields
  
  fields <- missing_fields(data, required)
  if (length(fields) > 0) {
    errors <- bind_rows(errors, data_frame(
      level = "error",
      field = fields,
      row = NA,
      message = paste0("Required field ", fields, " is missing")
    ))
  }
  
  # find empty values for required fields
  
  for (field in required) {
    if (field %in% names(data)) {
      rows <- missing_values(data[,field])
      if (length(which(rows)) > 0) {
        errors <- bind_rows(errors, data_frame(
          level = "error",
          field = field,
          row = which(rows),
          message = paste0("Empty value for required field ", field)
        ))
      }
    }
  }
  
  # recommended fields
  
  if (level == "warning") {
    
    # find missing recommended fields
    
    fields <- missing_fields(data, recommended)
    if (length(fields) > 0) {
      errors <- bind_rows(errors, data_frame(
        field = fields,
        level = "warning",
        message = paste0("Recommended field ", fields, " is missing")
      ))
    }
    
    # find empty values for recommended fields
    
    for (field in recommended) {
      if (field %in% names(data)) {
        rows <- missing_values(data[,field])
        if (length(which(rows)) > 0) {
          errors <- bind_rows(errors, data_frame(
            level = "warning",
            field = field,
            row = which(rows),
            message = paste0("Empty value for recommended field ", field)
          ))
        }
      }
    }
    
  }
  
  return(errors)
}

#' Check if the required and recommended datatype-specific SHARK system fields (different between different datatypes) are present.
#'
#' Missing or empty required fields are reported as errors,
#' missing or empty recommended fields are reported as warnings.
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export

check_EpibenthosDropvideo <- function(data, level = "error") {
  
  errors <- tibble()
  required <- c("visit_year","station_name","reported_station_name","sample_project_name_sv","sample_orderer_name_sv","platform_code","sample_date","sample_latitude_dd","sample_longitude_dd","positioning_system_code","secchi_depth_m","secchi_depth_quality_flag","sample_min_depth_m","sample_max_depth_m","sampling_laboratory_name_sv","sampler_type_code","sampler_area_m2","sampler_area_cm2","scientific_name","dyntaxa_id","value","taxonomist","analysis_method_code","method_documentation","method_comment","image_id","video_interpreted","analytical_laboratory_name_sv","analysed_by","transect_start_latitude_dd","transect_start_longitude_dd","transect_end_latitude_dd","transect_end_longitude_dd","sample_substrate_cover_boulder","sample_substrate_comnt_boulder","sample_substrate_cover_rock","sample_substrate_comnt_rock","sample_substrate_cover_softbottom","sample_substrate_comnt_softbottom","sample_substrate_cover_stone","sample_substrate_comnt_stone","sample_substrate_cover_gravel","sample_substrate_comnt_gravel","sample_substrate_cover_sand","sample_substrate_comnt_sand","section_bare_substrate","section_comment","section_substrate_cover_boulder","section_substrate_comnt_boulder","section_substrate_cover_gravel","section_substrate_comnt_gravel","section_substrate_cover_rock","section_substrate_comnt_rock","section_substrate_cover_sand","section_substrate_comnt_sand","section_substrate_cover_softbottom","section_substrate_comnt_softbottom","section_substrate_cover_stone","section_substrate_comnt_stone","section_debris_cover","section_start_latitude_dd","section_start_longitude_dd","section_end_latitude_dd","section_end_longitude_dd","section_distance_start_m","section_distance_end_m","section_start_depth_m","section_end_depth_m","reported_scientific_name","reported_value","reporting_institute_name_sv")
  recommended <- c("monitoring_program_code")
  
  # find missing required fields
  
  fields <- missing_fields(data, required)
  if (length(fields) > 0) {
    errors <- bind_rows(errors, data_frame(
      level = "error",
      field = fields,
      row = NA,
      message = paste0("Required field ", fields, " is missing")
    ))
  }
  
  # find empty values for required fields
  
  for (field in required) {
    if (field %in% names(data)) {
      rows <- missing_values(data[,field])
      if (length(which(rows)) > 0) {
        errors <- bind_rows(errors, data_frame(
          level = "error",
          field = field,
          row = which(rows),
          message = paste0("Empty value for required field ", field)
        ))
      }
    }
  }
  
  # recommended fields
  
  if (level == "warning") {
    
    # find missing recommended fields
    
    fields <- missing_fields(data, recommended)
    if (length(fields) > 0) {
      errors <- bind_rows(errors, data_frame(
        field = fields,
        level = "warning",
        message = paste0("Recommended field ", fields, " is missing")
      ))
    }
    
    # find empty values for recommended fields
    
    for (field in recommended) {
      if (field %in% names(data)) {
        rows <- missing_values(data[,field])
        if (length(which(rows)) > 0) {
          errors <- bind_rows(errors, data_frame(
            level = "warning",
            field = field,
            row = which(rows),
            message = paste0("Empty value for recommended field ", field)
          ))
        }
      }
    }
    
  }
  
  return(errors)
}

#' Check if the required and recommended datatype-specific SHARK system fields (different between different datatypes) are present.
#'
#' Missing or empty required fields are reported as errors,
#' missing or empty recommended fields are reported as warnings.
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export

check_GreySeal <- function(data, level = "error") {
  
  errors <- tibble()
  required <- c("visit_year","station_name","reported_station_name","sample_project_name_sv","sample_orderer_name_sv","visit_date","sample_date","sample_time","sample_enddate","sample_endtime","sample_latitude_dd","sample_longitude_dd","water_depth_m","sample_min_depth_m","sample_max_depth_m","sampling_laboratory_name_sv","scientific_name","dyntaxa_id","value","unit","quality_flag","sex_code","dev_stage_code","trophic_type_code","size_class","method_documentation","method_reference_code","analytical_laboratory_name_sv","reported_scientific_name","reported_value")
  recommended <- c()
  
  # find missing required fields
  
  fields <- missing_fields(data, required)
  if (length(fields) > 0) {
    errors <- bind_rows(errors, data_frame(
      level = "error",
      field = fields,
      row = NA,
      message = paste0("Required field ", fields, " is missing")
    ))
  }
  
  # find empty values for required fields
  
  for (field in required) {
    if (field %in% names(data)) {
      rows <- missing_values(data[,field])
      if (length(which(rows)) > 0) {
        errors <- bind_rows(errors, data_frame(
          level = "error",
          field = field,
          row = which(rows),
          message = paste0("Empty value for required field ", field)
        ))
      }
    }
  }
  
  # recommended fields
  
  if (level == "warning") {
    
    # find missing recommended fields
    
    fields <- missing_fields(data, recommended)
    if (length(fields) > 0) {
      errors <- bind_rows(errors, data_frame(
        field = fields,
        level = "warning",
        message = paste0("Recommended field ", fields, " is missing")
      ))
    }
    
    # find empty values for recommended fields
    
    for (field in recommended) {
      if (field %in% names(data)) {
        rows <- missing_values(data[,field])
        if (length(which(rows)) > 0) {
          errors <- bind_rows(errors, data_frame(
            level = "warning",
            field = field,
            row = which(rows),
            message = paste0("Empty value for recommended field ", field)
          ))
        }
      }
    }
    
  }
  
  return(errors)
}

#' Check if the required and recommended datatype-specific SHARK system fields (different between different datatypes) are present.
#'
#' Missing or empty required fields are reported as errors,
#' missing or empty recommended fields are reported as warnings.
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export

check_HarbourPorpoise <- function(data, level = "error") {
  
  errors <- tibble()
  required <- c("visit_year","station_name","reported_station_name","sample_project_name_sv","sample_orderer_name_sv","visit_date","sample_date","sample_time","sample_enddate","sample_endtime","sample_latitude_dd","sample_longitude_dd","water_depth_m","sample_min_depth_m","sample_max_depth_m","sampling_laboratory_name_sv","scientific_name","dyntaxa_id","value","quality_flag","sex_code","dev_stage_code","trophic_type_code","size_class","method_documentation","method_reference_code","analytical_laboratory_name_sv","reported_scientific_name","reported_value")
  recommended <- c()
  
  # find missing required fields
  
  fields <- missing_fields(data, required)
  if (length(fields) > 0) {
    errors <- bind_rows(errors, data_frame(
      level = "error",
      field = fields,
      row = NA,
      message = paste0("Required field ", fields, " is missing")
    ))
  }
  
  # find empty values for required fields
  
  for (field in required) {
    if (field %in% names(data)) {
      rows <- missing_values(data[,field])
      if (length(which(rows)) > 0) {
        errors <- bind_rows(errors, data_frame(
          level = "error",
          field = field,
          row = which(rows),
          message = paste0("Empty value for required field ", field)
        ))
      }
    }
  }
  
  # recommended fields
  
  if (level == "warning") {
    
    # find missing recommended fields
    
    fields <- missing_fields(data, recommended)
    if (length(fields) > 0) {
      errors <- bind_rows(errors, data_frame(
        field = fields,
        level = "warning",
        message = paste0("Recommended field ", fields, " is missing")
      ))
    }
    
    # find empty values for recommended fields
    
    for (field in recommended) {
      if (field %in% names(data)) {
        rows <- missing_values(data[,field])
        if (length(which(rows)) > 0) {
          errors <- bind_rows(errors, data_frame(
            level = "warning",
            field = field,
            row = which(rows),
            message = paste0("Empty value for recommended field ", field)
          ))
        }
      }
    }
    
  }
  
  return(errors)
}

#' Check if the required and recommended datatype-specific SHARK system fields (different between different datatypes) are present.
#'
#' Missing or empty required fields are reported as errors,
#' missing or empty recommended fields are reported as warnings.
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export

check_HarbourSeal <- function(data, level = "error") {
  
  errors <- tibble()
  required <- c("visit_year","station_name","reported_station_name","sample_project_name_sv","sample_orderer_name_sv","visit_date","sample_date","sample_time","sample_enddate","sample_endtime","sample_latitude_dd","sample_longitude_dd","water_depth_m","sample_min_depth_m","sample_max_depth_m","sampling_laboratory_name_sv","scientific_name","dyntaxa_id","value","quality_flag","sex_code","dev_stage_code","trophic_type_code","size_class","method_documentation","method_reference_code","analytical_laboratory_name_sv","reported_scientific_name","reported_value")
  recommended <- c()
  
  # find missing required fields
  
  fields <- missing_fields(data, required)
  if (length(fields) > 0) {
    errors <- bind_rows(errors, data_frame(
      level = "error",
      field = fields,
      row = NA,
      message = paste0("Required field ", fields, " is missing")
    ))
  }
  
  # find empty values for required fields
  
  for (field in required) {
    if (field %in% names(data)) {
      rows <- missing_values(data[,field])
      if (length(which(rows)) > 0) {
        errors <- bind_rows(errors, data_frame(
          level = "error",
          field = field,
          row = which(rows),
          message = paste0("Empty value for required field ", field)
        ))
      }
    }
  }
  
  # recommended fields
  
  if (level == "warning") {
    
    # find missing recommended fields
    
    fields <- missing_fields(data, recommended)
    if (length(fields) > 0) {
      errors <- bind_rows(errors, data_frame(
        field = fields,
        level = "warning",
        message = paste0("Recommended field ", fields, " is missing")
      ))
    }
    
    # find empty values for recommended fields
    
    for (field in recommended) {
      if (field %in% names(data)) {
        rows <- missing_values(data[,field])
        if (length(which(rows)) > 0) {
          errors <- bind_rows(errors, data_frame(
            level = "warning",
            field = field,
            row = which(rows),
            message = paste0("Empty value for recommended field ", field)
          ))
        }
      }
    }
    
  }
  
  return(errors)
}

#' Check if the required and recommended datatype-specific SHARK system fields (different between different datatypes) are present.
#'
#' Missing or empty required fields are reported as errors,
#' missing or empty recommended fields are reported as warnings.
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export

check_PhysicalChemical <- function(data, level = "error") {
  
  errors <- tibble()
  required <- c("visit_year","station_name","reported_station_name","sample_project_name_sv","sample_orderer_name_sv","platform_code","visit_date","sample_time","sample_enddate","sample_endtime","sample_latitude_dd","sample_longitude_dd","positioning_system_code","water_depth_m","secchi_depth_m","secchi_depth_quality_flag","sample_depth_m","value","quality_flag","analysis_method_code","method_reference_code","estimation_uncertainty","method_calculation_uncertainty","quantification_limit","detection_limit","analysis_range","analytical_laboratory_name_sv","analytical_laboratory_accreditated","sampler_type_code_phyche","sampling_method_reference_code_phyche","sampling_method_comment_phyche","sampling_laboratory_code_phyche","sampling_laboratory_accreditated_phyche","reporting_institute_name_sv")
  recommended <- c("monitoring_program_code")
  
  # find missing required fields
  
  fields <- missing_fields(data, required)
  if (length(fields) > 0) {
    errors <- bind_rows(errors, data_frame(
      level = "error",
      field = fields,
      row = NA,
      message = paste0("Required field ", fields, " is missing")
    ))
  }
  
  # find empty values for required fields
  
  for (field in required) {
    if (field %in% names(data)) {
      rows <- missing_values(data[,field])
      if (length(which(rows)) > 0) {
        errors <- bind_rows(errors, data_frame(
          level = "error",
          field = field,
          row = which(rows),
          message = paste0("Empty value for required field ", field)
        ))
      }
    }
  }
  
  # recommended fields
  
  if (level == "warning") {
    
    # find missing recommended fields
    
    fields <- missing_fields(data, recommended)
    if (length(fields) > 0) {
      errors <- bind_rows(errors, data_frame(
        field = fields,
        level = "warning",
        message = paste0("Recommended field ", fields, " is missing")
      ))
    }
    
    # find empty values for recommended fields
    
    for (field in recommended) {
      if (field %in% names(data)) {
        rows <- missing_values(data[,field])
        if (length(which(rows)) > 0) {
          errors <- bind_rows(errors, data_frame(
            level = "warning",
            field = field,
            row = which(rows),
            message = paste0("Empty value for recommended field ", field)
          ))
        }
      }
    }
    
  }
  
  return(errors)
}

#' Check if the required and recommended datatype-specific SHARK system fields (different between different datatypes) are present.
#'
#' Missing or empty required fields are reported as errors,
#' missing or empty recommended fields are reported as warnings.
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export

check_Phytoplankton <- function(data, level = "error") {
  
  errors <- tibble()
  required <- c("visit_year","station_name","reported_station_name","sample_project_name_sv","sample_orderer_name_sv","platform_code","sample_date","sample_time","sample_latitude_dd","sample_longitude_dd","positioning_system_code","water_depth_m","secchi_depth_m","secchi_depth_quality_flag","sample_min_depth_m","sample_max_depth_m","sampling_laboratory_name_sv","sampling_laboratory_accreditated","sampler_type_code","sampled_volume_l","plankton_sampling_method_code","scientific_name","dyntaxa_id","value","quality_flag","trophic_type_code","size_class","size_class_ref_list","reported_cell_volume_um3","taxonomist","analysis_method_code","method_documentation","method_reference_code","method_comment","analytical_laboratory_name_sv","analytical_laboratory_accreditated","analysis_date","preservation_method_code","mesh_size_um","reported_scientific_name","reported_value","reporting_institute_name_sv")
  recommended <- c("monitoring_purpose_code","monitoring_program_code")
  
  # find missing required fields
  
  fields <- missing_fields(data, required)
  if (length(fields) > 0) {
    errors <- bind_rows(errors, data_frame(
      level = "error",
      field = fields,
      row = NA,
      message = paste0("Required field ", fields, " is missing")
    ))
  }
  
  # find empty values for required fields
  
  for (field in required) {
    if (field %in% names(data)) {
      rows <- missing_values(data[,field])
      if (length(which(rows)) > 0) {
        errors <- bind_rows(errors, data_frame(
          level = "error",
          field = field,
          row = which(rows),
          message = paste0("Empty value for required field ", field)
        ))
      }
    }
  }
  
  # recommended fields
  
  if (level == "warning") {
    
    # find missing recommended fields
    
    fields <- missing_fields(data, recommended)
    if (length(fields) > 0) {
      errors <- bind_rows(errors, data_frame(
        field = fields,
        level = "warning",
        message = paste0("Recommended field ", fields, " is missing")
      ))
    }
    
    # find empty values for recommended fields
    
    for (field in recommended) {
      if (field %in% names(data)) {
        rows <- missing_values(data[,field])
        if (length(which(rows)) > 0) {
          errors <- bind_rows(errors, data_frame(
            level = "warning",
            field = field,
            row = which(rows),
            message = paste0("Empty value for recommended field ", field)
          ))
        }
      }
    }
    
  }
  
  return(errors)
}

#' Check if the required and recommended datatype-specific SHARK system fields (different between different datatypes) are present.
#'
#' Missing or empty required fields are reported as errors,
#' missing or empty recommended fields are reported as warnings.
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export

check_Picoplankton <- function(data, level = "error") {
  
  errors <- tibble()
  required <- c("visit_year","station_name","reported_station_name","sample_project_name_sv","sample_orderer_name_sv","platform_code","sample_date","sample_time","sample_latitude_dd","sample_longitude_dd","positioning_system_code","water_depth_m","secchi_depth_m","secchi_depth_quality_flag","sample_min_depth_m","sample_max_depth_m","sampling_laboratory_name_sv","sampling_laboratory_accreditated","sampler_type_code","sampled_volume_l","plankton_sampling_method_code","scientific_name","dyntaxa_id","value","quality_flag","trophic_type_code","size_class","size_class_ref_list","reported_cell_volume_um3","taxonomist","analysis_method_code","method_documentation","method_reference_code","analytical_laboratory_name_sv","analytical_laboratory_accreditated","analysis_date","preservation_method_code","reported_scientific_name","reported_value","reporting_institute_name_sv")
  recommended <- c("monitoring_station_type_code","monitoring_purpose_code","monitoring_program_code")
  
  # find missing required fields
  
  fields <- missing_fields(data, required)
  if (length(fields) > 0) {
    errors <- bind_rows(errors, data_frame(
      level = "error",
      field = fields,
      row = NA,
      message = paste0("Required field ", fields, " is missing")
    ))
  }
  
  # find empty values for required fields
  
  for (field in required) {
    if (field %in% names(data)) {
      rows <- missing_values(data[,field])
      if (length(which(rows)) > 0) {
        errors <- bind_rows(errors, data_frame(
          level = "error",
          field = field,
          row = which(rows),
          message = paste0("Empty value for required field ", field)
        ))
      }
    }
  }
  
  # recommended fields
  
  if (level == "warning") {
    
    # find missing recommended fields
    
    fields <- missing_fields(data, recommended)
    if (length(fields) > 0) {
      errors <- bind_rows(errors, data_frame(
        field = fields,
        level = "warning",
        message = paste0("Recommended field ", fields, " is missing")
      ))
    }
    
    # find empty values for recommended fields
    
    for (field in recommended) {
      if (field %in% names(data)) {
        rows <- missing_values(data[,field])
        if (length(which(rows)) > 0) {
          errors <- bind_rows(errors, data_frame(
            level = "warning",
            field = field,
            row = which(rows),
            message = paste0("Empty value for recommended field ", field)
          ))
        }
      }
    }
    
  }
  
  return(errors)
}

#' Check if the required and recommended datatype-specific SHARK system fields (different between different datatypes) are present.
#'
#' Missing or empty required fields are reported as errors,
#' missing or empty recommended fields are reported as warnings.
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export

check_PrimaryProduction <- function(data, level = "error") {
  
  errors <- tibble()
  required <- c("visit_year","station_name","reported_station_name","sample_project_name_sv","sample_orderer_name_sv","platform_code","sample_date","sample_time","sample_latitude_dd","sample_longitude_dd","positioning_system_code","water_depth_m","sample_min_depth_m","sample_max_depth_m","sampling_laboratory_name_sv","sampling_laboratory_accreditated","sampler_type_code","sample_comment","DPM_added","DPM_sample","DPM_darkness","value","quality_flag","analysis_method_code","method_documentation","method_reference_code","analytical_laboratory_name_sv","analytical_laboratory_accreditated","incubation_start_time","incubation_end_time","incubation_time_h","reporting_institute_name_sv")
  recommended <- c("monitoring_station_type_code","monitoring_purpose_code","monitoring_program_code")
  
  # find missing required fields
  
  fields <- missing_fields(data, required)
  if (length(fields) > 0) {
    errors <- bind_rows(errors, data_frame(
      level = "error",
      field = fields,
      row = NA,
      message = paste0("Required field ", fields, " is missing")
    ))
  }
  
  # find empty values for required fields
  
  for (field in required) {
    if (field %in% names(data)) {
      rows <- missing_values(data[,field])
      if (length(which(rows)) > 0) {
        errors <- bind_rows(errors, data_frame(
          level = "error",
          field = field,
          row = which(rows),
          message = paste0("Empty value for required field ", field)
        ))
      }
    }
  }
  
  # recommended fields
  
  if (level == "warning") {
    
    # find missing recommended fields
    
    fields <- missing_fields(data, recommended)
    if (length(fields) > 0) {
      errors <- bind_rows(errors, data_frame(
        field = fields,
        level = "warning",
        message = paste0("Recommended field ", fields, " is missing")
      ))
    }
    
    # find empty values for recommended fields
    
    for (field in recommended) {
      if (field %in% names(data)) {
        rows <- missing_values(data[,field])
        if (length(which(rows)) > 0) {
          errors <- bind_rows(errors, data_frame(
            level = "warning",
            field = field,
            row = which(rows),
            message = paste0("Empty value for recommended field ", field)
          ))
        }
      }
    }
    
  }
  
  return(errors)
}

#' Check if the required and recommended datatype-specific SHARK system fields (different between different datatypes) are present.
#'
#' Missing or empty required fields are reported as errors,
#' missing or empty recommended fields are reported as warnings.
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export

check_RingedSeal <- function(data, level = "error") {
  
  errors <- tibble()
  required <- c("visit_year","station_name","reported_station_name","sample_project_name_sv","sample_orderer_name_sv","sample_date","sample_time","sample_enddate","sample_endtime","sample_latitude_dd","sample_longitude_dd","water_depth_m","sample_min_depth_m","sample_max_depth_m","sampling_laboratory_name_sv","scientific_name","dyntaxa_id","value","quality_flag","sex_code","dev_stage_code","trophic_type_code","size_class","method_documentation","method_reference_code","analytical_laboratory_name_sv","reported_scientific_name","reported_value")
  recommended <- c()
  
  # find missing required fields
  
  fields <- missing_fields(data, required)
  if (length(fields) > 0) {
    errors <- bind_rows(errors, data_frame(
      level = "error",
      field = fields,
      row = NA,
      message = paste0("Required field ", fields, " is missing")
    ))
  }
  
  # find empty values for required fields
  
  for (field in required) {
    if (field %in% names(data)) {
      rows <- missing_values(data[,field])
      if (length(which(rows)) > 0) {
        errors <- bind_rows(errors, data_frame(
          level = "error",
          field = field,
          row = which(rows),
          message = paste0("Empty value for required field ", field)
        ))
      }
    }
  }
  
  # recommended fields
  
  if (level == "warning") {
    
    # find missing recommended fields
    
    fields <- missing_fields(data, recommended)
    if (length(fields) > 0) {
      errors <- bind_rows(errors, data_frame(
        field = fields,
        level = "warning",
        message = paste0("Recommended field ", fields, " is missing")
      ))
    }
    
    # find empty values for recommended fields
    
    for (field in recommended) {
      if (field %in% names(data)) {
        rows <- missing_values(data[,field])
        if (length(which(rows)) > 0) {
          errors <- bind_rows(errors, data_frame(
            level = "warning",
            field = field,
            row = which(rows),
            message = paste0("Empty value for recommended field ", field)
          ))
        }
      }
    }
    
  }
  
  return(errors)
}

#' Check if the required and recommended datatype-specific SHARK system fields (different between different datatypes) are present.
#'
#' Missing or empty required fields are reported as errors,
#' missing or empty recommended fields are reported as warnings.
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export

check_SealPathology <- function(data, level = "error") {
  
  errors <- tibble()
  required <- c("visit_year","station_name","reported_station_name","sample_project_name_sv","sample_orderer_name_sv","sample_date","sample_time","sample_enddate","sample_endtime","sample_latitude_dd","sample_longitude_dd","water_depth_m","sample_min_depth_m","sample_max_depth_m","sampling_laboratory_name_sv","scientific_name","dyntaxa_id","value","quality_flag","sex_code","dev_stage_code","trophic_type_code","size_class","method_documentation","method_reference_code","analytical_laboratory_name_sv","reported_scientific_name","reported_value")
  recommended <- c()
  
  # find missing required fields
  
  fields <- missing_fields(data, required)
  if (length(fields) > 0) {
    errors <- bind_rows(errors, data_frame(
      level = "error",
      field = fields,
      row = NA,
      message = paste0("Required field ", fields, " is missing")
    ))
  }
  
  # find empty values for required fields
  
  for (field in required) {
    if (field %in% names(data)) {
      rows <- missing_values(data[,field])
      if (length(which(rows)) > 0) {
        errors <- bind_rows(errors, data_frame(
          level = "error",
          field = field,
          row = which(rows),
          message = paste0("Empty value for required field ", field)
        ))
      }
    }
  }
  
  # recommended fields
  
  if (level == "warning") {
    
    # find missing recommended fields
    
    fields <- missing_fields(data, recommended)
    if (length(fields) > 0) {
      errors <- bind_rows(errors, data_frame(
        field = fields,
        level = "warning",
        message = paste0("Recommended field ", fields, " is missing")
      ))
    }
    
    # find empty values for recommended fields
    
    for (field in recommended) {
      if (field %in% names(data)) {
        rows <- missing_values(data[,field])
        if (length(which(rows)) > 0) {
          errors <- bind_rows(errors, data_frame(
            level = "warning",
            field = field,
            row = which(rows),
            message = paste0("Empty value for recommended field ", field)
          ))
        }
      }
    }
    
  }
  
  return(errors)
}

#' Check if the required and recommended datatype-specific SHARK system fields (different between different datatypes) are present.
#'
#' Missing or empty required fields are reported as errors,
#' missing or empty recommended fields are reported as warnings.
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export

check_Sedimentation <- function(data, level = "error") {
  
  errors <- tibble()
  required <- c("visit_year","station_name","reported_station_name","sample_project_name_sv","sample_orderer_name_sv","platform_code","sample_date","sample_time","sample_enddate","sample_latitude_dd","sample_longitude_dd","positioning_system_code","water_depth_m","sample_min_depth_m","sample_max_depth_m","sample_depth_quality_flag","sampling_laboratory_name_sv","sampling_laboratory_accreditated","sampler_type_code","value","quality_flag","method_documentation","method_reference_code","aggregated_subsamples","analytical_laboratory_name_sv","analytical_laboratory_accreditated","preservation_method_code","mesh_size_um","method_incubation","incubation_start_time","incubation_end_time","incubation_time_h","salinity_correction","reporting_institute_name_sv")
  recommended <- c("monitoring_program_code")
  
  # find missing required fields
  
  fields <- missing_fields(data, required)
  if (length(fields) > 0) {
    errors <- bind_rows(errors, data_frame(
      level = "error",
      field = fields,
      row = NA,
      message = paste0("Required field ", fields, " is missing")
    ))
  }
  
  # find empty values for required fields
  
  for (field in required) {
    if (field %in% names(data)) {
      rows <- missing_values(data[,field])
      if (length(which(rows)) > 0) {
        errors <- bind_rows(errors, data_frame(
          level = "error",
          field = field,
          row = which(rows),
          message = paste0("Empty value for required field ", field)
        ))
      }
    }
  }
  
  # recommended fields
  
  if (level == "warning") {
    
    # find missing recommended fields
    
    fields <- missing_fields(data, recommended)
    if (length(fields) > 0) {
      errors <- bind_rows(errors, data_frame(
        field = fields,
        level = "warning",
        message = paste0("Recommended field ", fields, " is missing")
      ))
    }
    
    # find empty values for recommended fields
    
    for (field in recommended) {
      if (field %in% names(data)) {
        rows <- missing_values(data[,field])
        if (length(which(rows)) > 0) {
          errors <- bind_rows(errors, data_frame(
            level = "warning",
            field = field,
            row = which(rows),
            message = paste0("Empty value for recommended field ", field)
          ))
        }
      }
    }
    
  }
  
  return(errors)
}

#' Check if the required and recommended datatype-specific SHARK system fields (different between different datatypes) are present.
#'
#' Missing or empty required fields are reported as errors,
#' missing or empty recommended fields are reported as warnings.
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export

check_Zoobenthos <- function(data, level = "error") {
  
  errors <- tibble()
  required <- c("delivery_datatype","check_status_sv","data_checked_by_sv","visit_year","station_name","sample_project_name_sv","sample_orderer_name_sv","platform_code","sample_date","sample_time","sample_latitude_dd","sample_longitude_dd","positioning_system_code","water_depth_m","secchi_depth_m","secchi_depth_quality_flag","sediment_type","sample_min_depth_m","sample_max_depth_m","sampling_laboratory_name_sv","sampling_laboratory_accreditated","sampler_type_code","sampled_volume_l","sampler_area_cm2","scientific_name","dyntaxa_id","value","quality_flag","taxonomist","method_documentation","method_reference_code","analytical_laboratory_name_sv","analytical_laboratory_accreditated","analysed_by","analysis_date","preservation_method_code","upper_mesh_size_um","lower_mesh_size_um","reported_scientific_name","reported_value","reporting_institute_name_sv")
  recommended <- c("monitoring_purpose_code","monitoring_program_code")
  
  # find missing required fields
  
  fields <- missing_fields(data, required)
  if (length(fields) > 0) {
    errors <- bind_rows(errors, data_frame(
      level = "error",
      field = fields,
      row = NA,
      message = paste0("Required field ", fields, " is missing")
    ))
  }
  
  # find empty values for required fields
  
  for (field in required) {
    if (field %in% names(data)) {
      rows <- missing_values(data[,field])
      if (length(which(rows)) > 0) {
        errors <- bind_rows(errors, data_frame(
          level = "error",
          field = field,
          row = which(rows),
          message = paste0("Empty value for required field ", field)
        ))
      }
    }
  }
  
  # recommended fields
  
  if (level == "warning") {
    
    # find missing recommended fields
    
    fields <- missing_fields(data, recommended)
    if (length(fields) > 0) {
      errors <- bind_rows(errors, data_frame(
        field = fields,
        level = "warning",
        message = paste0("Recommended field ", fields, " is missing")
      ))
    }
    
    # find empty values for recommended fields
    
    for (field in recommended) {
      if (field %in% names(data)) {
        rows <- missing_values(data[,field])
        if (length(which(rows)) > 0) {
          errors <- bind_rows(errors, data_frame(
            level = "warning",
            field = field,
            row = which(rows),
            message = paste0("Empty value for recommended field ", field)
          ))
        }
      }
    }
    
  }
  
  return(errors)
}

#' Check if the required and recommended datatype-specific SHARK system fields (different between different datatypes) are present.
#'
#' Missing or empty required fields are reported as errors,
#' missing or empty recommended fields are reported as warnings.
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export

check_Zooplankton <- function(data, level = "error") {
  
  errors <- tibble()
  required <- c("delivery_datatype","check_status_sv","data_checked_by_sv","visit_year","station_name","sample_project_name_sv","sample_orderer_name_sv","platform_code","sample_date","sample_time","sample_latitude_dd","sample_longitude_dd","positioning_system_code","water_depth_m","secchi_depth_m","secchi_depth_quality_flag","sample_min_depth_m","sample_max_depth_m","sampling_laboratory_name_sv","sampling_laboratory_accreditated","sampler_type_code","sampled_volume_l","sampler_area_cm2","plankton_sampling_method_code","scientific_name","dyntaxa_id","value","quality_flag","sex_code","dev_stage_code","size_class","size_min_um","size_max_um","taxonomist","method_documentation","method_reference_code","calculation_method","analytical_laboratory_name_sv","analytical_laboratory_accreditated","analysed_by","analysis_date","preservation_method_code","mesh_size_um","reported_scientific_name","reported_value","reporting_institute_name_sv")
  recommended <- c("monitoring_purpose_code","monitoring_program_code")
  
  # find missing required fields
  
  fields <- missing_fields(data, required)
  if (length(fields) > 0) {
    errors <- bind_rows(errors, data_frame(
      level = "error",
      field = fields,
      row = NA,
      message = paste0("Required field ", fields, " is missing")
    ))
  }
  
  # find empty values for required fields
  
  for (field in required) {
    if (field %in% names(data)) {
      rows <- missing_values(data[,field])
      if (length(which(rows)) > 0) {
        errors <- bind_rows(errors, data_frame(
          level = "error",
          field = field,
          row = which(rows),
          message = paste0("Empty value for required field ", field)
        ))
      }
    }
  }
  
  # recommended fields
  
  if (level == "warning") {
    
    # find missing recommended fields
    
    fields <- missing_fields(data, recommended)
    if (length(fields) > 0) {
      errors <- bind_rows(errors, data_frame(
        field = fields,
        level = "warning",
        message = paste0("Recommended field ", fields, " is missing")
      ))
    }
    
    # find empty values for recommended fields
    
    for (field in recommended) {
      if (field %in% names(data)) {
        rows <- missing_values(data[,field])
        if (length(which(rows)) > 0) {
          errors <- bind_rows(errors, data_frame(
            level = "warning",
            field = field,
            row = which(rows),
            message = paste0("Empty value for recommended field ", field)
          ))
        }
      }
    }
    
  }
  
  return(errors)
}

#' Check if the required and recommended global and datatype-specific SHARK system fields (different between different datatypes) are present in intial deliveries of data (delivery validation).
#'
#' Missing or empty required fields are reported as errors,
#' missing or empty recommended fields are reported as warnings.
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export

check_deliv_Bacterioplankton <- function(data, level = "error") {
  
  errors <- tibble()
  required <- c("MYEAR","STATN","PROJ","ORDERER","SHIPC","SERNO","SDATE","LATIT","LONGI","POSYS","WADEP","MSTAT", "MNDEP", "MXDEP", "SLABO", "ACKR_SMP", "SMTYP", "SMVOL", "CHLA_CONC", "QFLAG", "UNCERT", "METCU", "DETLI", "LMQNT", "RANA", "ALABO", "ACKR_ANA", "ANADATE", "METDC")
  recommended <- c("CRUISE_NO", "COMNT_VISIT", "WINDIR", "WINSP", "AIRTEMP", "AIRPRES", "WEATH", "CLOUD", "WAVES", "ICEOB", "SECCHI", "Q_SECCHI", "STIME", "SMPNO", "COMNT_SAMP", "METOA", "REFSK", "COMNT_VAR")
  
  # find missing required fields
  
  fields <- missing_fields(data, required)
  if (length(fields) > 0) {
    errors <- bind_rows(errors, data_frame(
      level = "error",
      field = fields,
      row = NA,
      message = paste0("Required field ", fields, " is missing")
    ))
  }
  
  # find empty values for required fields
  
  for (field in required) {
    if (field %in% names(data)) {
      rows <- missing_values(data[,field])
      if (length(which(rows)) > 0) {
        errors <- bind_rows(errors, data_frame(
          level = "error",
          field = field,
          row = which(rows),
          message = paste0("Empty value for required field ", field)
        ))
      }
    }
  }
  
  # recommended fields
  
  if (level == "warning") {
    
    # find missing recommended fields
    
    fields <- missing_fields(data, recommended)
    if (length(fields) > 0) {
      errors <- bind_rows(errors, data_frame(
        field = fields,
        level = "warning",
        message = paste0("Recommended field ", fields, " is missing")
      ))
    }
    
    # find empty values for recommended fields
    
    for (field in recommended) {
      if (field %in% names(data)) {
        rows <- missing_values(data[,field])
        if (length(which(rows)) > 0) {
          errors <- bind_rows(errors, data_frame(
            level = "warning",
            field = field,
            row = which(rows),
            message = paste0("Empty value for recommended field ", field)
          ))
        }
      }
    }
    
  }
  
  return(errors)
}

#' Check if the required and recommended datatype-specific SHARK system fields (different between different datatypes) are present.
#'
#' Missing or empty required fields are reported as errors,
#' missing or empty recommended fields are reported as warnings.
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export

check_deliv_Chlorophyll <- function(data, level = "error") {
  
  errors <- tibble()
  required <- c("MYEAR",	"STATN",	"PROJ",	"ORDERER",	"SHIPC", "SDATE",	"LATIT",	"LONGI", 	"POSYS",	"WADEP",	"MSTAT", "MNDEP",	"MXDEP",	"SLABO",	"ACKR_SMP",	"SMTYP",	"SMVOL", "CHLA_CONC",	"QFLAG",	"UNCERT",	"METCU",	"DETLI",	"LMQNT",	"RANA", "ALABO",	"ACKR_ANA",	"ANADATE",	"METDC")
  recommended <- c("CRUISE_NO",	"SERNO", "COMNT_VISIT",	"WINDIR",	"WINSP",	"AIRTEMP",	"AIRPRES",	"WEATH",	"CLOUD",	"WAVES",	"ICEOB",	"SECCHI",	"Q_SECCHI", "STIME", "SMPNO",	"COMNT_SAMP", "METOA", "REFSK",	"COMNT_VAR")
  
  # find missing required fields
  
  fields <- missing_fields(data, required)
  if (length(fields) > 0) {
    errors <- bind_rows(errors, data_frame(
      level = "error",
      field = fields,
      row = NA,
      message = paste0("Required field ", fields, " is missing")
    ))
  }
  
  # find empty values for required fields
  
  for (field in required) {
    if (field %in% names(data)) {
      rows <- missing_values(data[,field])
      if (length(which(rows)) > 0) {
        errors <- bind_rows(errors, data_frame(
          level = "error",
          field = field,
          row = which(rows),
          message = paste0("Empty value for required field ", field)
        ))
      }
    }
  }
  
  # recommended fields
  
  if (level == "warning") {
    
    # find missing recommended fields
    
    fields <- missing_fields(data, recommended)
    if (length(fields) > 0) {
      errors <- bind_rows(errors, data_frame(
        field = fields,
        level = "warning",
        message = paste0("Recommended field ", fields, " is missing")
      ))
    }
    
    # find empty values for recommended fields
    
    for (field in recommended) {
      if (field %in% names(data)) {
        rows <- missing_values(data[,field])
        if (length(which(rows)) > 0) {
          errors <- bind_rows(errors, data_frame(
            level = "warning",
            field = field,
            row = which(rows),
            message = paste0("Empty value for recommended field ", field)
          ))
        }
      }
    }
    
  }
  
  return(errors)
}

#' Check if the required and recommended datatype-specific SHARK system fields (different between different datatypes) are present.
#'
#' Missing or empty required fields are reported as errors,
#' missing or empty recommended fields are reported as warnings.
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export

check_deliv_Epibenthos <- function(data, level = "error") {
  
  errors <- tibble()
  required <- c("MYEAR",	"STATN", "PROJ",	"ORDERER",	"SDATE", "SLABO", "COMNT_VISIT", "LATIT",	"LONGI",	"POSYS",	"TRANS_ID",	"TRANS_MIN",	"TRANS_MAX",	"TRANS_LAT_START",	"TRANS_LONG_START",	"TRANS_LAT_END",	"TRANS_LONG_END", "TRANS_START_DEP",	"TRANS_STOP_DEP", "SAMPLEID", "SECTIONMIN",	"SECTIONMAX",	"SECTIONSTA",	"SECTIONSTO",	"SMTYP",	"SAREA", "SUBST",	"SUBST_COVER", "LATNM",	"SFLAG", "TAXNM", "COVER%", "COUNTNR",	"ABUND",	"ALABO",	"SMTYP",	"REFSK", "METDC")
  recommended <- c("STTYP",	"MPROG","PURPM", "MSTAT", "RLABO", "CRUIS",	"SHIPC",	"NTYPE", "WATLD",	"WADEP_COR",	"WAVXP", "TRANSL",	"TRANSW",	"TRANSDIR", "COMNT_TRANS",	"SMDEP", "RPSNO", "COMNT_SAMP", "DEPOS",	"DEPOS%",	"FNFLA", "STRID",	"COMNT_VAR", "COVER_1-4",	"COVER_1-5",	"COVER_1-7", "TOTCOVERAGE%", "METOA")
  
  # find missing required fields
  
  fields <- missing_fields(data, required)
  if (length(fields) > 0) {
    errors <- bind_rows(errors, data_frame(
      level = "error",
      field = fields,
      row = NA,
      message = paste0("Required field ", fields, " is missing")
    ))
  }
  
  # find empty values for required fields
  
  for (field in required) {
    if (field %in% names(data)) {
      rows <- missing_values(data[,field])
      if (length(which(rows)) > 0) {
        errors <- bind_rows(errors, data_frame(
          level = "error",
          field = field,
          row = which(rows),
          message = paste0("Empty value for required field ", field)
        ))
      }
    }
  }
  
  # recommended fields
  
  if (level == "warning") {
    
    # find missing recommended fields
    
    fields <- missing_fields(data, recommended)
    if (length(fields) > 0) {
      errors <- bind_rows(errors, data_frame(
        field = fields,
        level = "warning",
        message = paste0("Recommended field ", fields, " is missing")
      ))
    }
    
    # find empty values for recommended fields
    
    for (field in recommended) {
      if (field %in% names(data)) {
        rows <- missing_values(data[,field])
        if (length(which(rows)) > 0) {
          errors <- bind_rows(errors, data_frame(
            level = "warning",
            field = field,
            row = which(rows),
            message = paste0("Empty value for recommended field ", field)
          ))
        }
      }
    }
    
  }
  
  return(errors)
}

#' Check if the required and recommended datatype-specific SHARK system fields (different between different datatypes) are present.
#'
#' Missing or empty required fields are reported as errors,
#' missing or empty recommended fields are reported as warnings.
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export

check_deliv_EpibenthosDropvideo <- function(data, level = "error") {
  
  errors <- tibble()
  required <- c("MYEAR",	"STATN",	"PROJ",	"ORDERER",	"SDATE",	"SLABO",	"ACKR_SMP",	"NTYPE",	"SAL",	"SECCHI",	"Q_SECCHI",	"COMNT_VISIT",	"TRANS_ID",	"TRANS_LAT_START",	"TRANS_LONG_START",	"TRANS_LAT_STOP",	"TRANS_LONG_STOP", "TRANS_START_DEP",	"TRANS_STOP_DEP",	"VID_NAME", "VIDEO_INTER",	"PROTO_WRIT",	"DEPOS", "IMAGE_NAME",	"IMAGE_STOP_TIME", "ALABO",	"ACKR_ANA",	"ANADATE",	"METDC")
  recommended <- c("POSYS", "TRANS_INTERPRET_TIME", "COMNT_SAMP", "COVER_HARD_CLAY",	"COVER_SOFT_CLAY_<0.002",	"COVER_SILT_0.002_0.06",	"COVER_SILT_CLAY",	"COVER_SAND_0.06_2",	"COVER_GRAVEL_FINE+MEDIUM_2_20",	"COVER_GRAVEL_COARSE_20_60",	"COVER_COBBLE_MEDIUM_60_200",	"COVER_COBBLE_COARSE_200_600",	"COVER_LARGE_BOULDER_>600",	"COVER_ROCK",	"COVER_SHELL_GRAVEL",	"COVER_SHELL",	"COVER_BARE_SUBST",	"COVER_UNIDENT_SUBST",	"COVER_DETRITUS",	"COVER_EPI_ZOSTERA",	"COVER_UNIDENT_PLANTAE",	"COVER_NASSARIUS_TRACKS",	"COVER_PAGURIDAE_TRACKS",	"COVER_ANIMALIA_BURROWS",	"COVER_ANIMALIA_TRACKS",	"COVER_UNIDENTIFIED_ALGAE", "REFSK",	"COMNT_VAR")
  
  # find missing required fields
  
  fields <- missing_fields(data, required)
  if (length(fields) > 0) {
    errors <- bind_rows(errors, data_frame(
      level = "error",
      field = fields,
      row = NA,
      message = paste0("Required field ", fields, " is missing")
    ))
  }
  
  # find empty values for required fields
  
  for (field in required) {
    if (field %in% names(data)) {
      rows <- missing_values(data[,field])
      if (length(which(rows)) > 0) {
        errors <- bind_rows(errors, data_frame(
          level = "error",
          field = field,
          row = which(rows),
          message = paste0("Empty value for required field ", field)
        ))
      }
    }
  }
  
  # recommended fields
  
  if (level == "warning") {
    
    # find missing recommended fields
    
    fields <- missing_fields(data, recommended)
    if (length(fields) > 0) {
      errors <- bind_rows(errors, data_frame(
        field = fields,
        level = "warning",
        message = paste0("Recommended field ", fields, " is missing")
      ))
    }
    
    # find empty values for recommended fields
    
    for (field in recommended) {
      if (field %in% names(data)) {
        rows <- missing_values(data[,field])
        if (length(which(rows)) > 0) {
          errors <- bind_rows(errors, data_frame(
            level = "warning",
            field = field,
            row = which(rows),
            message = paste0("Empty value for recommended field ", field)
          ))
        }
      }
    }
    
  }
  
  return(errors)
}

#' Check if the required and recommended datatype-specific SHARK system fields (different between different datatypes) are present.
#'
#' Missing or empty required fields are reported as errors,
#' missing or empty recommended fields are reported as warnings.
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export

check_deliv_GreySeal <- function(data, level = "error") {
  
  errors <- tibble()
  required <- c("MYEAR",	"STATN", "PROJ",	"ORDERER", "SDATE",	"LATIT",	"LONGI", 	"POSYS",	"PURPM",	"MPROG", "SLABO",	"ACKR_SMP",	"SMTYP", "LATNM",	"COUNT",	"ALABO",	"ACKR_ANA",	"ANADATE",	"METDC")
  recommended <- c("REGION", "SHIPC",	"CRUISE_NO", "COMNT_VISIT",	"WINDIR",	"WINSP",	"AIRTEMP",	"AIRPRES",	"WEATH",	"CLOUD",	"WAVES",	"ICEOB",	"STIME",	"FNFLA", "SMP_BY",	"OBSPOINT",	"OBSDIST",	"IMAGE_ID",	"SMPNO",	"COMNT_SAMP", "REFSK",	"COMNT_VAR")
  
  # find missing required fields
  
  fields <- missing_fields(data, required)
  if (length(fields) > 0) {
    errors <- bind_rows(errors, data_frame(
      level = "error",
      field = fields,
      row = NA,
      message = paste0("Required field ", fields, " is missing")
    ))
  }
  
  # find empty values for required fields
  
  for (field in required) {
    if (field %in% names(data)) {
      rows <- missing_values(data[,field])
      if (length(which(rows)) > 0) {
        errors <- bind_rows(errors, data_frame(
          level = "error",
          field = field,
          row = which(rows),
          message = paste0("Empty value for required field ", field)
        ))
      }
    }
  }
  
  # recommended fields
  
  if (level == "warning") {
    
    # find missing recommended fields
    
    fields <- missing_fields(data, recommended)
    if (length(fields) > 0) {
      errors <- bind_rows(errors, data_frame(
        field = fields,
        level = "warning",
        message = paste0("Recommended field ", fields, " is missing")
      ))
    }
    
    # find empty values for recommended fields
    
    for (field in recommended) {
      if (field %in% names(data)) {
        rows <- missing_values(data[,field])
        if (length(which(rows)) > 0) {
          errors <- bind_rows(errors, data_frame(
            level = "warning",
            field = field,
            row = which(rows),
            message = paste0("Empty value for recommended field ", field)
          ))
        }
      }
    }
    
  }
  
  return(errors)
}

#' Check if the required and recommended datatype-specific SHARK system fields (different between different datatypes) are present.
#'
#' Missing or empty required fields are reported as errors,
#' missing or empty recommended fields are reported as warnings.
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export

check_deliv_HarbourPorpoise <- function(data, level = "error") {
  
  errors <- tibble()
  required <- c("MYEAR",	"STATN", "PROJ",	"ORDERER",	"SDATE", "EDATE","POSYS", "SLABO",	"ACKR_SMP",	"SMTYP",	"LATIT",	"LONGI", 	"METDC", "LATNM",	"DPM",	"ODATE",	"OTIME",	"ALABO",	"ACKR_SMP",	"RAW")
  recommended <- c("STIME", "ETIME", "COMNT_VISIT", "COMNT_SAMP", "COMNT_VAR")
  
  # find missing required fields
  
  fields <- missing_fields(data, required)
  if (length(fields) > 0) {
    errors <- bind_rows(errors, data_frame(
      level = "error",
      field = fields,
      row = NA,
      message = paste0("Required field ", fields, " is missing")
    ))
  }
  
  # find empty values for required fields
  
  for (field in required) {
    if (field %in% names(data)) {
      rows <- missing_values(data[,field])
      if (length(which(rows)) > 0) {
        errors <- bind_rows(errors, data_frame(
          level = "error",
          field = field,
          row = which(rows),
          message = paste0("Empty value for required field ", field)
        ))
      }
    }
  }
  
  # recommended fields
  
  if (level == "warning") {
    
    # find missing recommended fields
    
    fields <- missing_fields(data, recommended)
    if (length(fields) > 0) {
      errors <- bind_rows(errors, data_frame(
        field = fields,
        level = "warning",
        message = paste0("Recommended field ", fields, " is missing")
      ))
    }
    
    # find empty values for recommended fields
    
    for (field in recommended) {
      if (field %in% names(data)) {
        rows <- missing_values(data[,field])
        if (length(which(rows)) > 0) {
          errors <- bind_rows(errors, data_frame(
            level = "warning",
            field = field,
            row = which(rows),
            message = paste0("Empty value for recommended field ", field)
          ))
        }
      }
    }
    
  }
  
  return(errors)
}

#' Check if the required and recommended datatype-specific SHARK system fields (different between different datatypes) are present.
#'
#' Missing or empty required fields are reported as errors,
#' missing or empty recommended fields are reported as warnings.
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export

check_deliv_HarbourSeal <- function(data, level = "error") {
  
  errors <- tibble()
  required <- c("MYEAR",	"STATN", "PROJ",	"ORDERER", "SDATE",	"LATIT",	"LONGI", 	"POSYS",	"PURPM",	"MPROG", "SLABO",	"ACKR_SMP",	"SMTYP", "LATNM",	"COUNT",	"ALABO",	"ACKR_ANA",	"ANADATE",	"METDC")
  recommended <- c("REGION", "SHIPC",	"CRUISE_NO", "COMNT_VISIT",	"WINDIR",	"WINSP",	"AIRTEMP",	"AIRPRES",	"WEATH",	"CLOUD",	"WAVES",	"ICEOB",	"STIME",	"FNFLA", "SMP_BY",	"OBSPOINT",	"OBSDIST",	"IMAGE_ID",	"SMPNO",	"COMNT_SAMP", "REFSK",	"COMNT_VAR")
  
  # find missing required fields
  
  fields <- missing_fields(data, required)
  if (length(fields) > 0) {
    errors <- bind_rows(errors, data_frame(
      level = "error",
      field = fields,
      row = NA,
      message = paste0("Required field ", fields, " is missing")
    ))
  }
  
  # find empty values for required fields
  
  for (field in required) {
    if (field %in% names(data)) {
      rows <- missing_values(data[,field])
      if (length(which(rows)) > 0) {
        errors <- bind_rows(errors, data_frame(
          level = "error",
          field = field,
          row = which(rows),
          message = paste0("Empty value for required field ", field)
        ))
      }
    }
  }
  
  # recommended fields
  
  if (level == "warning") {
    
    # find missing recommended fields
    
    fields <- missing_fields(data, recommended)
    if (length(fields) > 0) {
      errors <- bind_rows(errors, data_frame(
        field = fields,
        level = "warning",
        message = paste0("Recommended field ", fields, " is missing")
      ))
    }
    
    # find empty values for recommended fields
    
    for (field in recommended) {
      if (field %in% names(data)) {
        rows <- missing_values(data[,field])
        if (length(which(rows)) > 0) {
          errors <- bind_rows(errors, data_frame(
            level = "warning",
            field = field,
            row = which(rows),
            message = paste0("Empty value for recommended field ", field)
          ))
        }
      }
    }
    
  }
  
  return(errors)
}

#' Check if the required and recommended datatype-specific SHARK system fields (different between different datatypes) are present.
#'
#' Missing or empty required fields are reported as errors,
#' missing or empty recommended fields are reported as warnings.
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export

check_deliv_PhysicalChemical <- function(data, level = "error") {
  
  errors <- tibble()
  required <- c("MYEAR",	"PROJ",	"ORDERER",	"SDATE",	"STIME",	"SHIPC",	"SERNO",	"STATN",	"LATIT",	"LONGI",	"POSYS",	"DEPH")
  recommended <- c("EDATE",	"ETIME", "CRUISE_NO", "WADEP",	"NODEPTH",	"ADD_SMP",	"COMNT_VISIT",	"WINDIR",	"WINSP",	"AIRTEMP",	"AIRPRES",	"WEATH",	"CLOUD",	"WAVES",	"ICEOB",	"SECCHI",	"Q_SECCHI",	"SMPNO",	"PRES_CTD",	"Q_PRES_CTD",	"TEMP_BTL",	"Q_TEMP_BTL",	"TEMP_CTD",	"Q_TEMP_CTD",	"SALT_BTL",	"Q_SALT_BTL",	"SALT_CTD",	"Q_SALT_CTD",	"CNDC_25",	"Q_CNDC_25",	"CNDC_CTD",	"Q_CNDC_CTD",	"DOXY_BTL",	"Q_DOXY_BTL",	"DOXY_CTD",	"Q_DOXY_CTD",	"H2S",	"Q_H2S",	"PH",	"Q_PH",	"PH_LAB",	"Q_PH_LAB",	"PH_LAB_TEMP",	"Q_PH_LAB_TEMP",	"ALKY",	"Q_ALKY",	"PHOS",	"Q_PHOS",	"PTOT",	"Q_PTOT",	"NTRI",	"Q_NTRI",	"NTRA",	"Q_NTRA",	"NTRZ",	"Q_NTRZ",	"AMON",	"Q_AMON",	"NTOT",	"Q_NTOT",	"SIO3-SI",	"Q_SIO3-SI",	"HUMUS",	"Q_HUMUS",	"CPHL",	"Q_CPHL",	"DOC",	"Q_DOC",	"POC",	"Q_POC",	"TOC",	"Q_TOC",	"PON",	"Q_PON",	"CURDIR",	"Q_CURDIR",	"CURVEL",	"Q_CURVEL",	"AL",	"Q_AL",	"CDOM",	"Q_CDOM",	"COMNT_SAMP")
  
  # find missing required fields
  
  fields <- missing_fields(data, required)
  if (length(fields) > 0) {
    errors <- bind_rows(errors, data_frame(
      level = "error",
      field = fields,
      row = NA,
      message = paste0("Required field ", fields, " is missing")
    ))
  }
  
  # find empty values for required fields
  
  for (field in required) {
    if (field %in% names(data)) {
      rows <- missing_values(data[,field])
      if (length(which(rows)) > 0) {
        errors <- bind_rows(errors, data_frame(
          level = "error",
          field = field,
          row = which(rows),
          message = paste0("Empty value for required field ", field)
        ))
      }
    }
  }
  
  # recommended fields
  
  if (level == "warning") {
    
    # find missing recommended fields
    
    fields <- missing_fields(data, recommended)
    if (length(fields) > 0) {
      errors <- bind_rows(errors, data_frame(
        field = fields,
        level = "warning",
        message = paste0("Recommended field ", fields, " is missing")
      ))
    }
    
    # find empty values for recommended fields
    
    for (field in recommended) {
      if (field %in% names(data)) {
        rows <- missing_values(data[,field])
        if (length(which(rows)) > 0) {
          errors <- bind_rows(errors, data_frame(
            level = "warning",
            field = field,
            row = which(rows),
            message = paste0("Empty value for recommended field ", field)
          ))
        }
      }
    }
    
  }
  
  return(errors)
}

#' Check if the required and recommended datatype-specific SHARK system fields (different between different datatypes) are present.
#'
#' Missing or empty required fields are reported as errors,
#' missing or empty recommended fields are reported as warnings.
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export

check_deliv_Phytoplankton <- function(data, level = "error") {
  
  errors <- tibble()
  required <- c("MYEAR",	"STATN",	"PROJ",	"ORDERER",	"SHIPC", "SDATE",	"LATIT",	"LONGI", 	"POSYS",	"WADEP",	"MSTAT", "MNDEP",	"MXDEP",	"SLABO",	"ACKR_SMP",	"SMTYP", "SMVOL", "LATNM",	"SFLAG",	"TRPHY",	"COUNT", "COEFF", "SIZCL",	"SIZRF", "SDVOL", "QFLAG",	"TAXNM", "METOA", "ALABO",	"ACKR_ANA",	"ANADATE",	"METDC")
  recommended <- c("CRUISE_NO","COMNT_VISIT",	"WINDIR",	"WINSP",	"AIRTEMP",	"AIRPRES",	"WEATH",	"CLOUD",	"WAVES",	"ICEOB",	"SECCHI",	"Q_SECCHI",	"STIME", "PDMET", "METFP",	"SMPNO",	"COMNT_SAMP", "ABUND", 	"ABUND_CLASS", "C_CONC", "BIOVOL", "SDTIM", "MAGNI", "COUNTPROG","REFSK",	"COMNT_VAR")
  
  # find missing required fields
  
  fields <- missing_fields(data, required)
  if (length(fields) > 0) {
    errors <- bind_rows(errors, data_frame(
      level = "error",
      field = fields,
      row = NA,
      message = paste0("Required field ", fields, " is missing")
    ))
  }
  
  # find empty values for required fields
  
  for (field in required) {
    if (field %in% names(data)) {
      rows <- missing_values(data[,field])
      if (length(which(rows)) > 0) {
        errors <- bind_rows(errors, data_frame(
          level = "error",
          field = field,
          row = which(rows),
          message = paste0("Empty value for required field ", field)
        ))
      }
    }
  }
  
  # recommended fields
  
  if (level == "warning") {
    
    # find missing recommended fields
    
    fields <- missing_fields(data, recommended)
    if (length(fields) > 0) {
      errors <- bind_rows(errors, data_frame(
        field = fields,
        level = "warning",
        message = paste0("Recommended field ", fields, " is missing")
      ))
    }
    
    # find empty values for recommended fields
    
    for (field in recommended) {
      if (field %in% names(data)) {
        rows <- missing_values(data[,field])
        if (length(which(rows)) > 0) {
          errors <- bind_rows(errors, data_frame(
            level = "warning",
            field = field,
            row = which(rows),
            message = paste0("Empty value for recommended field ", field)
          ))
        }
      }
    }
    
  }
  
  return(errors)
}

#' Check if the required and recommended datatype-specific SHARK system fields (different between different datatypes) are present.
#'
#' Missing or empty required fields are reported as errors,
#' missing or empty recommended fields are reported as warnings.
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export

check_deliv_Picoplankton <- function(data, level = "error") {
  
  errors <- tibble()
  required <- c("MYEAR",	"STATN",	"PROJ",	"ORDERER",	"SHIPC", "SDATE",	"LATIT",	"LONGI", 	"POSYS",	"WADEP",	"MSTAT", "MNDEP",	"MXDEP",	"SLABO",	"ACKR_SMP",	"SMTYP", "SMVOL", "LATNM",	"SFLAG",	"TRPHY",	"COUNT", "COEFF", "SIZCL",	"SIZRF", "SDVOL", "QFLAG",	"TAXNM", "METOA", "ALABO",	"ACKR_ANA",	"ANADATE",	"METDC")
  recommended <- c("CRUISE_NO","COMNT_VISIT",	"WINDIR",	"WINSP",	"AIRTEMP",	"AIRPRES",	"WEATH",	"CLOUD",	"WAVES",	"ICEOB",	"SECCHI",	"Q_SECCHI",	"STIME", "PDMET", "METFP",	"SMPNO",	"COMNT_SAMP", "ABUND", 	"ABUND_CLASS", "C_CONC", "BIOVOL", "SDTIM", "MAGNI", "COUNTPROG","REFSK",	"COMNT_VAR")
  
  # find missing required fields
  
  fields <- missing_fields(data, required)
  if (length(fields) > 0) {
    errors <- bind_rows(errors, data_frame(
      level = "error",
      field = fields,
      row = NA,
      message = paste0("Required field ", fields, " is missing")
    ))
  }
  
  # find empty values for required fields
  
  for (field in required) {
    if (field %in% names(data)) {
      rows <- missing_values(data[,field])
      if (length(which(rows)) > 0) {
        errors <- bind_rows(errors, data_frame(
          level = "error",
          field = field,
          row = which(rows),
          message = paste0("Empty value for required field ", field)
        ))
      }
    }
  }
  
  # recommended fields
  
  if (level == "warning") {
    
    # find missing recommended fields
    
    fields <- missing_fields(data, recommended)
    if (length(fields) > 0) {
      errors <- bind_rows(errors, data_frame(
        field = fields,
        level = "warning",
        message = paste0("Recommended field ", fields, " is missing")
      ))
    }
    
    # find empty values for recommended fields
    
    for (field in recommended) {
      if (field %in% names(data)) {
        rows <- missing_values(data[,field])
        if (length(which(rows)) > 0) {
          errors <- bind_rows(errors, data_frame(
            level = "warning",
            field = field,
            row = which(rows),
            message = paste0("Empty value for recommended field ", field)
          ))
        }
      }
    }
    
  }
  
  return(errors)
}

#' Check if the required and recommended datatype-specific SHARK system fields (different between different datatypes) are present.
#'
#' Missing or empty required fields are reported as errors,
#' missing or empty recommended fields are reported as warnings.
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export

check_deliv_PrimaryProduction <- function(data, level = "error") {
  
  errors <- tibble()
  required <- c("MYEAR",	"STATN",	"PROJ",	"ORDERER",	"SHIPC", "SDATE",	"LATIT",	"LONGI", 	"POSYS",	"WADEP", "MNDEP",	"MXDEP",	"SLABO",	"ACKR_SMP",	"SMTYP", "SUBNO",	"INCUR",	"DPMA",	"DPMS", "INTIM",	"PROD", "ALABO",	"ACKR_ANA",	"METDC")
  recommended <- c("CRUISE_NO",	"SERNO", "MSTAT",	"COMNT_VISIT",	"WINDIR",	"WINSP",	"AIRTEMP",	"AIRPRES",	"WEATH",	"CLOUD",	"WAVES",	"ICEOB",	"SECCHI",	"Q_SECCHI",	"STIME", "SMPNO",	"COMNT_SAMP", "DARK_PROD", "TOTALPROD",	"TEMP_BTL",	"SALT_BTL",	"PH",	"ALKY",	"CO2TOT",	"QFLAG", "REFSK",	"COMNT_VAR")
  
  # find missing required fields
  
  fields <- missing_fields(data, required)
  if (length(fields) > 0) {
    errors <- bind_rows(errors, data_frame(
      level = "error",
      field = fields,
      row = NA,
      message = paste0("Required field ", fields, " is missing")
    ))
  }
  
  # find empty values for required fields
  
  for (field in required) {
    if (field %in% names(data)) {
      rows <- missing_values(data[,field])
      if (length(which(rows)) > 0) {
        errors <- bind_rows(errors, data_frame(
          level = "error",
          field = field,
          row = which(rows),
          message = paste0("Empty value for required field ", field)
        ))
      }
    }
  }
  
  # recommended fields
  
  if (level == "warning") {
    
    # find missing recommended fields
    
    fields <- missing_fields(data, recommended)
    if (length(fields) > 0) {
      errors <- bind_rows(errors, data_frame(
        field = fields,
        level = "warning",
        message = paste0("Recommended field ", fields, " is missing")
      ))
    }
    
    # find empty values for recommended fields
    
    for (field in recommended) {
      if (field %in% names(data)) {
        rows <- missing_values(data[,field])
        if (length(which(rows)) > 0) {
          errors <- bind_rows(errors, data_frame(
            level = "warning",
            field = field,
            row = which(rows),
            message = paste0("Empty value for recommended field ", field)
          ))
        }
      }
    }
    
  }
  
  return(errors)
}

#' Check if the required and recommended datatype-specific SHARK system fields (different between different datatypes) are present.
#'
#' Missing or empty required fields are reported as errors,
#' missing or empty recommended fields are reported as warnings.
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export

check_deliv_RingedSeal <- function(data, level = "error") {
  
  errors <- tibble()
  required <- c("MYEAR",	"PROJ",	"ORDERER",	"SDATE",	"POSYS","SLABO",	"ACKR_SMP",	"SMTYP", "TRANS_ID",	"FNFLA",	"TRANS_LAT_START",	"TRANS_LONG_START",	"TRANS_LAT_STOP",	"TRANS_LONG_STOP",	"LATIT",	"LONGI", 	"TRANSL",	"TRANSW",	"COMNT_SAMP",	"LATNM",	"COUNT",	"TAXNM",	"COEFF",	"ALABO",	"ACKR_ANA",	"METDC")
  recommended <- c("COMNT_VISIT",	"WINDIR",	"WINSP",	"AIRTEMP",	"AIRPRES",	"WEATH",	"CLOUD",	"WAVES",	"ICEOB",	"STIME", "IMAGE_ID", "COMNT_VAR")
  
  # find missing required fields
  
  fields <- missing_fields(data, required)
  if (length(fields) > 0) {
    errors <- bind_rows(errors, data_frame(
      level = "error",
      field = fields,
      row = NA,
      message = paste0("Required field ", fields, " is missing")
    ))
  }
  
  # find empty values for required fields
  
  for (field in required) {
    if (field %in% names(data)) {
      rows <- missing_values(data[,field])
      if (length(which(rows)) > 0) {
        errors <- bind_rows(errors, data_frame(
          level = "error",
          field = field,
          row = which(rows),
          message = paste0("Empty value for required field ", field)
        ))
      }
    }
  }
  
  # recommended fields
  
  if (level == "warning") {
    
    # find missing recommended fields
    
    fields <- missing_fields(data, recommended)
    if (length(fields) > 0) {
      errors <- bind_rows(errors, data_frame(
        field = fields,
        level = "warning",
        message = paste0("Recommended field ", fields, " is missing")
      ))
    }
    
    # find empty values for recommended fields
    
    for (field in recommended) {
      if (field %in% names(data)) {
        rows <- missing_values(data[,field])
        if (length(which(rows)) > 0) {
          errors <- bind_rows(errors, data_frame(
            level = "warning",
            field = field,
            row = which(rows),
            message = paste0("Empty value for recommended field ", field)
          ))
        }
      }
    }
    
  }
  
  return(errors)
}

#' Check if the required and recommended datatype-specific SHARK system fields (different between different datatypes) are present.
#'
#' Missing or empty required fields are reported as errors,
#' missing or empty recommended fields are reported as warnings.
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export

check_deliv_SealPathology <- function(data, level = "error") {
  
  errors <- tibble()
  required <- c("MYEAR",	"STATN",	"FCST", "PROJ",	"ORDERER",	"SDATE", "SLABO",	"ACKR_SMP", "LATNM",	"MATRX",	"SEXCODE",	"AGE",	"BODY_LGTH",	"BODY_WGTH",	"BLUBB_THICK",	"AUTOLYSIS_CL",	"REG_SKIN_CHANGE_CL",	"CLAW_LESION_CL",	"CRONIC_CHOLANGIOHEPATITIS",	"INTEST_ULCER_CL",	"PNEUMONIA",	"ARTERIOSCLEROSIS_CL",	"ADRENO_CORTHYPERPLASIA_CL", "UTERUS_PREG",	"UTERUS_STENOSIS_OCCLUSION",	"UTERUS_TUMOUR",	"CAUSE_OF_DEATH",	"ALABO",	"ACKR_ANA",	"ANADATE",	"METDC")
  recommended <- c("LATIT",	"LONGI", "COMNT_VISIT","SMPNO",	"COMNT_SAMP", "PARODONTITIS_CL", "KIDNEY_GLOMERULAR_CHANGE",	"KIDNEY_TUBULAR_HYPERPLASIA", "REFSK",	"COMNT_VAR")
  
  # find missing required fields
  
  fields <- missing_fields(data, required)
  if (length(fields) > 0) {
    errors <- bind_rows(errors, data_frame(
      level = "error",
      field = fields,
      row = NA,
      message = paste0("Required field ", fields, " is missing")
    ))
  }
  
  # find empty values for required fields
  
  for (field in required) {
    if (field %in% names(data)) {
      rows <- missing_values(data[,field])
      if (length(which(rows)) > 0) {
        errors <- bind_rows(errors, data_frame(
          level = "error",
          field = field,
          row = which(rows),
          message = paste0("Empty value for required field ", field)
        ))
      }
    }
  }
  
  # recommended fields
  
  if (level == "warning") {
    
    # find missing recommended fields
    
    fields <- missing_fields(data, recommended)
    if (length(fields) > 0) {
      errors <- bind_rows(errors, data_frame(
        field = fields,
        level = "warning",
        message = paste0("Recommended field ", fields, " is missing")
      ))
    }
    
    # find empty values for recommended fields
    
    for (field in recommended) {
      if (field %in% names(data)) {
        rows <- missing_values(data[,field])
        if (length(which(rows)) > 0) {
          errors <- bind_rows(errors, data_frame(
            level = "warning",
            field = field,
            row = which(rows),
            message = paste0("Empty value for recommended field ", field)
          ))
        }
      }
    }
    
  }
  
  return(errors)
}

#' Check if the required and recommended datatype-specific SHARK system fields (different between different datatypes) are present.
#'
#' Missing or empty required fields are reported as errors,
#' missing or empty recommended fields are reported as warnings.
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export

check_deliv_Sedimentation <- function(data, level = "error") {
  
  errors <- tibble()
  required <- c("MYEAR",	"STATN",	"PROJ",	"ORDERER",	"SHIPC",	"SDATE",	"LATIT",	"LONGI", 	"POSYS",	"WADEP",	"MSTAT", "SMDEP", "SLABO",	"ACKR_SMP",	"SMTYP",	"SMVOL",	"MESH","ALABO",	"ACKR_ANA",	"ANADATE", "METDC", "S_COR_FAC",	"SED_DW",	"QDW",	"SED_C%",	"QC",	"SED_N%",	"QN",	"SED_P%",	"QP")
  recommended <- c("COMNT_EXP",	"WINDIR",	"WINSP",	"WEATH",	"CLOUD",	"WAVES",	"ICEOB",	"SMPNO",	"STIME", "QDEP", "SMPNO",	"COMNT_SAMP", "METFP","REFSK",	"COMNT_VAR")
  
  # find missing required fields
  
  fields <- missing_fields(data, required)
  if (length(fields) > 0) {
    errors <- bind_rows(errors, data_frame(
      level = "error",
      field = fields,
      row = NA,
      message = paste0("Required field ", fields, " is missing")
    ))
  }
  
  # find empty values for required fields
  
  for (field in required) {
    if (field %in% names(data)) {
      rows <- missing_values(data[,field])
      if (length(which(rows)) > 0) {
        errors <- bind_rows(errors, data_frame(
          level = "error",
          field = field,
          row = which(rows),
          message = paste0("Empty value for required field ", field)
        ))
      }
    }
  }
  
  # recommended fields
  
  if (level == "warning") {
    
    # find missing recommended fields
    
    fields <- missing_fields(data, recommended)
    if (length(fields) > 0) {
      errors <- bind_rows(errors, data_frame(
        field = fields,
        level = "warning",
        message = paste0("Recommended field ", fields, " is missing")
      ))
    }
    
    # find empty values for recommended fields
    
    for (field in recommended) {
      if (field %in% names(data)) {
        rows <- missing_values(data[,field])
        if (length(which(rows)) > 0) {
          errors <- bind_rows(errors, data_frame(
            level = "warning",
            field = field,
            row = which(rows),
            message = paste0("Empty value for recommended field ", field)
          ))
        }
      }
    }
    
  }
  
  return(errors)
}

#' Check if the required and recommended datatype-specific SHARK system fields (different between different datatypes) are present.
#'
#' Missing or empty required fields are reported as errors,
#' missing or empty recommended fields are reported as warnings.
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export

check_deliv_Zoobenthos <- function(data, level = "error") {
  
  errors <- tibble()
  required <- c("MYEAR",	"STATN",	"PROJ",	"ORDERER",	"SHIPC","SDATE",	"LATIT",	"LONGI", 	"POSYS",	"WADEP",	"MSTAT", "SLABO",	"ACKR_SMP",	"SMTYP",	"SMVOL", "MESH_LOWER", "LATNM",	"SFLAG",	"COUNT",	"WETWT",	"QFLAG",	"STAGE",	"TAXNM",	"ALABO",	"ACKR_ANA",	"ANADATE",	"METDC")
  recommended <- c("CRUISE_NO", "COMNT_VISIT", "WINDIR",	"WINSP",	"AIRTEMP",	"AIRPRES",	"WEATH",	"CLOUD",	"WAVES",	"ICEOB",	"STIME",	"DEPH",	"SDTYP",	"H2S_SMELL",	"SECCHI",	"Q_SECCHI",	"FNFLA", "SAREA",	"METFP",	"MESH_UPPER", "SMPNO",	"COMNT_SAMP", "REFSK",	"COMNT_VAR",	"SPMAX",	"SPMIN",	"COLOUR_SED_C",	"LOIGN",	"WATER_CONT_SED",	"REDOX_POT_SED")
  
  # find missing required fields
  
  fields <- missing_fields(data, required)
  if (length(fields) > 0) {
    errors <- bind_rows(errors, data_frame(
      level = "error",
      field = fields,
      row = NA,
      message = paste0("Required field ", fields, " is missing")
    ))
  }
  
  # find empty values for required fields
  
  for (field in required) {
    if (field %in% names(data)) {
      rows <- missing_values(data[,field])
      if (length(which(rows)) > 0) {
        errors <- bind_rows(errors, data_frame(
          level = "error",
          field = field,
          row = which(rows),
          message = paste0("Empty value for required field ", field)
        ))
      }
    }
  }
  
  # recommended fields
  
  if (level == "warning") {
    
    # find missing recommended fields
    
    fields <- missing_fields(data, recommended)
    if (length(fields) > 0) {
      errors <- bind_rows(errors, data_frame(
        field = fields,
        level = "warning",
        message = paste0("Recommended field ", fields, " is missing")
      ))
    }
    
    # find empty values for recommended fields
    
    for (field in recommended) {
      if (field %in% names(data)) {
        rows <- missing_values(data[,field])
        if (length(which(rows)) > 0) {
          errors <- bind_rows(errors, data_frame(
            level = "warning",
            field = field,
            row = which(rows),
            message = paste0("Empty value for recommended field ", field)
          ))
        }
      }
    }
    
  }
  
  return(errors)
}

#' Check if the required and recommended datatype-specific SHARK system fields (different between different datatypes) are present.
#'
#' Missing or empty required fields are reported as errors,
#' missing or empty recommended fields are reported as warnings.
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export

check_deliv_Zooplankton <- function(data, level = "error") {
  
  errors <- tibble()
  required <- c("MYEAR",	"STATN",	"PROJ",	"ORDERER",	"SHIPC", "SDATE",	"LATIT",	"LONGI", 	"POSYS",	"WADEP",	"MSTAT", "MNDEP",	"MXDEP",	"SLABO",	"ACKR_SMP",	"SMTYP",	"SMVOL",	"SAREA", "CLMET", "MESHS", "NPORT",	"CPORT", "LATNM",	"SFLAG",	"COUNT",	"WETWT",	"BODY_LGTH", "QFLAG",	"SEXCODE",	"STAGE",	"TAXNM",	"ALABO",	"ACKR_ANA",	"ANADATE",	"METDC")
  recommended <- c("CRUISE_NO",	"SERNO", "COMNT_VISIT",	"WINDIR",	"WINSP",	"AIRTEMP",	"AIRPRES",	"WEATH",	"CLOUD",	"WAVES",	"ICEOB",	"SECCHI",	"Q_SECCHI",	"STIME", "WIRAN",	"PDMET",	"FLOW_READ", "METFP", "SMPNO",	"COMNT_SAMP",	"SPLIT", "MAGNI", "LENGTH_MEAN",	"LENGTH_MEDIAN",	"CCONT",	"BMCCONT", "REFSK",	"COMNT_VAR")
  
  # find missing required fields
  
  fields <- missing_fields(data, required)
  if (length(fields) > 0) {
    errors <- bind_rows(errors, data_frame(
      level = "error",
      field = fields,
      row = NA,
      message = paste0("Required field ", fields, " is missing")
    ))
  }
  
  # find empty values for required fields
  
  for (field in required) {
    if (field %in% names(data)) {
      rows <- missing_values(data[,field])
      if (length(which(rows)) > 0) {
        errors <- bind_rows(errors, data_frame(
          level = "error",
          field = field,
          row = which(rows),
          message = paste0("Empty value for required field ", field)
        ))
      }
    }
  }
  
  # recommended fields
  
  if (level == "warning") {
    
    # find missing recommended fields
    
    fields <- missing_fields(data, recommended)
    if (length(fields) > 0) {
      errors <- bind_rows(errors, data_frame(
        field = fields,
        level = "warning",
        message = paste0("Recommended field ", fields, " is missing")
      ))
    }
    
    # find empty values for recommended fields
    
    for (field in recommended) {
      if (field %in% names(data)) {
        rows <- missing_values(data[,field])
        if (length(which(rows)) > 0) {
          errors <- bind_rows(errors, data_frame(
            level = "warning",
            field = field,
            row = which(rows),
            message = paste0("Empty value for recommended field ", field)
          ))
        }
      }
    }
    
  }
  
  return(errors)
}
