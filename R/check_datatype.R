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

  errors <- data_frame()
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

check_Bacterioplankton <- function(data, level = "error") {

  errors <- data_frame()
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

check_Chlorophyll <- function(data, level = "error") {

  errors <- data_frame()
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

check_Epibenthos <- function(data, level = "error") {

  errors <- data_frame()
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

check_EpibenthosDropvideo <- function(data, level = "error") {

  errors <- data_frame()
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

check_GreySeal <- function(data, level = "error") {

  errors <- data_frame()
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

check_HarbourPorpoise <- function(data, level = "error") {

  errors <- data_frame()
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

check_HarbourSeal <- function(data, level = "error") {

  errors <- data_frame()
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

check_PhysicalChemical <- function(data, level = "error") {

  errors <- data_frame()
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

check_Phytoplankton <- function(data, level = "error") {

  errors <- data_frame()
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

check_Picoplankton <- function(data, level = "error") {

  errors <- data_frame()
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

check_PrimaryProduction <- function(data, level = "error") {

  errors <- data_frame()
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

check_RingedSeal <- function(data, level = "error") {

  errors <- data_frame()
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

check_SealPathology <- function(data, level = "error") {

  errors <- data_frame()
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

check_Sedimentation <- function(data, level = "error") {

  errors <- data_frame()
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

check_Zoobenthos <- function(data, level = "error") {

  errors <- data_frame()
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

check_Zooplankton <- function(data, level = "error") {

  errors <- data_frame()
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
