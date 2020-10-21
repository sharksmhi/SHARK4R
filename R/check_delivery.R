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

  errors <- data_frame()
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

check_deliv_Chlorophyll <- function(data, level = "error") {

  errors <- data_frame()
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

check_deliv_Epibenthos <- function(data, level = "error") {

  errors <- data_frame()
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

check_deliv_EpibenthosDropvideo <- function(data, level = "error") {

  errors <- data_frame()
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

check_deliv_GreySeal <- function(data, level = "error") {

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

check_deliv_HarbourPorpoise <- function(data, level = "error") {

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

check_deliv_HarbourSeal <- function(data, level = "error") {

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

check_deliv_PhysicalChemical <- function(data, level = "error") {

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

check_deliv_Phytoplankton <- function(data, level = "error") {

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

check_deliv_Picoplankton <- function(data, level = "error") {

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

check_deliv_PrimaryProduction <- function(data, level = "error") {

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

check_deliv_RingedSeal <- function(data, level = "error") {

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

check_deliv_SealPathology <- function(data, level = "error") {

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

check_deliv_Sedimentation <- function(data, level = "error") {

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

check_deliv_Zoobenthos <- function(data, level = "error") {

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

check_deliv_Zooplankton <- function(data, level = "error") {

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
