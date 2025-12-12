# suppress warning for "no visible binding for global variable" in R CMD check
utils::globalVariables(c("visit_year", "station_name", "sample_project_name_sv", "threshold",
                         "sample_orderer_name_sv", "platform_code", "sample_date", "parent_name",
                         "sample_latitude_dd", "sample_longitude_dd", "positioning_system_code",
                         "water_depth_m", "Statistic", "x", "y", "ymin", "ymax", "lower", "parentName",
                         "middle", "upper", "Value", "Ok", "sample_id", "shark_sample_id_md5",
                         "sample_min_depth_m", "sample_max_depth_m", "parameter", "value",
                         "taxon_species", "taxon_genus", "worms_species", "worms_genus", "worms_scientific_name",
                         "scientific_name", "nameShort", "isRecommended", "delivery_datatype",
                         "usage.name", "usage.value", "taxonId", ".", "Species", "taxonId_recommended",
                         "acceptedNameUsageID", "parentNameUsageID", "scientificName", "taxonRank",
                         "scientificNameAuthorship", "taxonomicStatus", "nomenclaturalStatus",
                         "taxonRemarks", "dataset_name", "taxon_name", "aphia_id", "TYPOMRNAMN",
                         "NAMN", "BASIN_NR", "lon", "lat", "geometry", "datatype", "dyntaxa_id",
                         "family", "genus", "taxon_id", "scientificname", "authority", "status",
                         "match_type", "STATN", "LONGI", "LATIT", "species", "SDATE", "Description/English translate",
                         "author", "guid", "name", "Kingdom", "Phylum", "Class", "Order", "Family",
                         "Genus", "Species", "hierarchy", "kingdom", "AphiaID", "worms_hierarchy", "taxon_hierarchy",
                         "up_to_date", "Data_field", "DT", "phylum", "plankton_group", "longitude", "latitude",
                         "value_num", "n_non_numeric", "frac_non_numeric", "n_total", "stats",
                         "distance_m", "within_limit", "SYNONYM_NAMES", "LATITUDE_WGS84_SWEREF99_DD",
                         "LAT_REF", "LONGITUDE_WGS84_SWEREF99_DD", "LON_REF", "OUT_OF_BOUNDS_RADIUS",
                         "RADIUS", "STATION", "STATION_NAME", "bad_point", ".data", ":=", "valid_name"))

.onLoad <- function(libname, pkgname){
  clean_shark4r_cache(days = 1, verbose = FALSE)
}

.onUnload <- function(libpath) {
  if (is_check()) {
    try(clean_shark4r_cache(0, clear_perm_cache = TRUE, verbose = FALSE), silent = TRUE)
  }
}

.field_definitions <- list(
  Bacterioplankton = list(
    required = c(
      "visit_year","station_name","sample_project_name_sv","sample_orderer_name_sv","platform_code",
      "sample_date","sample_time","sample_latitude_dd","sample_longitude_dd","positioning_system_code",
      "water_depth_m","sample_min_depth_m","sample_max_depth_m","sampling_laboratory_name_sv",
      "sampling_laboratory_accreditated","sampler_type_code","sampled_volume_l","scientific_name",
      "value","quality_flag","analysis_method_code","method_reference_code","analytical_laboratory_name_sv",
      "analytical_laboratory_accreditated","analysed_volume_cm3","preservation_method_code","counted_portions",
      "reporting_institute_name_sv"
    ),
    recommended = c("monitoring_program_code")
  ),
  Chlorophyll = list(
    required = c(
      "visit_year","station_name","reported_station_name","sample_project_name_sv","sample_orderer_name_sv",
      "platform_code","sample_date","sample_time","sample_enddate","sample_endtime","sample_latitude_dd",
      "sample_longitude_dd","positioning_system_code","water_depth_m","secchi_depth_m","secchi_depth_quality_flag",
      "sample_min_depth_m","sample_max_depth_m","sampling_laboratory_name_sv","sampling_laboratory_accreditated",
      "sampler_type_code","sampled_volume_l","value","quality_flag","analysis_method_code","method_documentation",
      "method_reference_code","estimation_uncertainty","method_calculation_uncertainty","quantification_limit",
      "detection_limit","analysis_range","analytical_laboratory_name_sv","analytical_laboratory_accreditated",
      "analysis_date","reporting_institute_name_sv"
    ),
    recommended = c("monitoring_purpose_code","monitoring_program_code")
  ),
  Epibenthos = list(
    required = c(
      "visit_year","station_name","reported_station_name","sample_project_name_sv","sample_orderer_name_sv",
      "platform_code","sample_date","sample_latitude_dd","sample_longitude_dd","positioning_system_code",
      "sample_min_depth_m","sample_max_depth_m","sampling_laboratory_name_sv","sampler_type_code","sampler_area_m2",
      "sampler_area_cm2","scientific_name","dyntaxa_id","value","size_class","analysis_method_code",
      "method_documentation","image_id","sediment_deposition_code","video_interpreted","sample_photo_code",
      "variable_comment","analytical_laboratory_name_sv","analysed_by","transect_start_latitude_dd",
      "transect_start_longitude_dd","transect_end_latitude_dd","transect_end_longitude_dd","transect_max_depth_m",
      "transect_min_depth_m","transect_max_distance_m","transect_min_distance_m","transect_video","transect_width_m",
      "sample_substrate_cover_boulder","sample_substrate_comnt_boulder","sample_substrate_cover_rock",
      "sample_substrate_comnt_rock","sample_substrate_cover_softbottom","sample_substrate_comnt_softbottom",
      "sample_substrate_cover_stone","sample_substrate_comnt_stone","sample_substrate_cover_gravel",
      "sample_substrate_comnt_gravel","sample_substrate_cover_sand","sample_substrate_comnt_sand",
      "section_bare_substrate","section_comment","section_substrate_cover_boulder","section_substrate_comnt_boulder",
      "section_substrate_cover_gravel","section_substrate_comnt_gravel","section_substrate_cover_rock",
      "section_substrate_comnt_rock","section_substrate_cover_sand","section_substrate_comnt_sand",
      "section_substrate_cover_softbottom","section_substrate_comnt_softbottom","section_substrate_cover_stone",
      "section_substrate_comnt_stone","section_debris_cover","section_start_latitude_dd","section_start_longitude_dd",
      "section_end_latitude_dd","section_end_longitude_dd","section_distance_start_m","section_distance_end_m",
      "section_fauna_flora_found","section_start_depth_m","section_end_depth_m","reported_scientific_name",
      "reported_value","reporting_institute_name_sv"
    ),
    recommended = c("monitoring_station_type_code","monitoring_purpose_code","monitoring_program_code")
  ),
  EpibenthosDropvideo = list(
    required = c(
      "visit_year","station_name","reported_station_name","sample_project_name_sv","sample_orderer_name_sv",
      "platform_code","sample_date","sample_latitude_dd","sample_longitude_dd","positioning_system_code",
      "secchi_depth_m","secchi_depth_quality_flag","sample_min_depth_m","sample_max_depth_m","sampling_laboratory_name_sv",
      "sampler_type_code","sampler_area_m2","sampler_area_cm2","scientific_name","dyntaxa_id","value","taxonomist",
      "analysis_method_code","method_documentation","method_comment","image_id","video_interpreted",
      "analytical_laboratory_name_sv","analysed_by","transect_start_latitude_dd","transect_start_longitude_dd",
      "transect_end_latitude_dd","transect_end_longitude_dd","sample_substrate_cover_boulder","sample_substrate_comnt_boulder",
      "sample_substrate_cover_rock","sample_substrate_comnt_rock","sample_substrate_cover_softbottom","sample_substrate_comnt_softbottom",
      "sample_substrate_cover_stone","sample_substrate_comnt_stone","sample_substrate_cover_gravel","sample_substrate_comnt_gravel",
      "sample_substrate_cover_sand","sample_substrate_comnt_sand","section_bare_substrate","section_comment",
      "section_substrate_cover_boulder","section_substrate_comnt_boulder","section_substrate_cover_gravel",
      "section_substrate_comnt_gravel","section_substrate_cover_rock","section_substrate_comnt_rock",
      "section_substrate_cover_sand","section_substrate_comnt_sand","section_substrate_cover_softbottom",
      "section_substrate_comnt_softbottom","section_substrate_cover_stone","section_substrate_comnt_stone",
      "section_debris_cover","section_start_latitude_dd","section_start_longitude_dd","section_end_latitude_dd",
      "section_end_longitude_dd","section_distance_start_m","section_distance_end_m","section_start_depth_m",
      "section_end_depth_m","reported_scientific_name","reported_value","reporting_institute_name_sv"
    ),
    recommended = c("monitoring_program_code")
  ),
  GreySeal = list(
    required = c(
      "visit_year","station_name","reported_station_name","sample_project_name_sv","sample_orderer_name_sv",
      "visit_date","sample_date","sample_time","sample_enddate","sample_endtime","sample_latitude_dd",
      "sample_longitude_dd","water_depth_m","sample_min_depth_m","sample_max_depth_m","sampling_laboratory_name_sv",
      "scientific_name","dyntaxa_id","value","unit","quality_flag","sex_code","dev_stage_code","trophic_type_code",
      "size_class","method_documentation","method_reference_code","analytical_laboratory_name_sv",
      "reported_scientific_name","reported_value"
    ),
    recommended = c()
  ),
  HarbourPorpoise = list(
    required = c(
      "visit_year","station_name","reported_station_name","sample_project_name_sv","sample_orderer_name_sv",
      "visit_date","sample_date","sample_time","sample_enddate","sample_endtime","sample_latitude_dd",
      "sample_longitude_dd","water_depth_m","sample_min_depth_m","sample_max_depth_m","sampling_laboratory_name_sv",
      "scientific_name","dyntaxa_id","value","quality_flag","sex_code","dev_stage_code","trophic_type_code",
      "size_class","method_documentation","method_reference_code","analytical_laboratory_name_sv",
      "reported_scientific_name","reported_value"
    ),
    recommended = c()
  ),
  HarbourSeal = list(
    required = c(
      "visit_year","station_name","reported_station_name","sample_project_name_sv","sample_orderer_name_sv",
      "visit_date","sample_date","sample_time","sample_enddate","sample_endtime","sample_latitude_dd",
      "sample_longitude_dd","water_depth_m","sample_min_depth_m","sample_max_depth_m","sampling_laboratory_name_sv",
      "scientific_name","dyntaxa_id","value","quality_flag","sex_code","dev_stage_code","trophic_type_code",
      "size_class","method_documentation","method_reference_code","analytical_laboratory_name_sv",
      "reported_scientific_name","reported_value"
    ),
    recommended = c()
  ),
  PhysicalChemical = list(
    required = c(
      "visit_year","station_name","reported_station_name","sample_project_name_sv","sample_orderer_name_sv",
      "platform_code","visit_date","sample_time","sample_enddate","sample_endtime","sample_latitude_dd",
      "sample_longitude_dd","positioning_system_code","water_depth_m","secchi_depth_m","secchi_depth_quality_flag",
      "sample_depth_m","value","quality_flag","analysis_method_code","method_reference_code","estimation_uncertainty",
      "method_calculation_uncertainty","quantification_limit","detection_limit","analysis_range",
      "analytical_laboratory_name_sv","analytical_laboratory_accreditated","sampler_type_code_phyche",
      "sampling_method_reference_code_phyche","sampling_method_comment_phyche","sampling_laboratory_code_phyche",
      "sampling_laboratory_accreditated_phyche","reporting_institute_name_sv"
    ),
    recommended = c("monitoring_program_code")
  ),
  Phytoplankton = list(
    required = c(
      "visit_year","station_name","reported_station_name","sample_project_name_sv","sample_orderer_name_sv",
      "platform_code","sample_date","sample_time","sample_latitude_dd","sample_longitude_dd",
      "positioning_system_code","water_depth_m","secchi_depth_m","secchi_depth_quality_flag","sample_min_depth_m",
      "sample_max_depth_m","sampling_laboratory_name_sv","sampling_laboratory_accreditated","sampler_type_code",
      "sampled_volume_l","plankton_sampling_method_code","scientific_name","dyntaxa_id","value","quality_flag",
      "trophic_type_code","size_class","size_class_ref_list","reported_cell_volume_um3","taxonomist",
      "analysis_method_code","method_documentation","method_reference_code","method_comment",
      "analytical_laboratory_name_sv","analytical_laboratory_accreditated","analysis_date","preservation_method_code",
      "mesh_size_um","reported_scientific_name","reported_value","reporting_institute_name_sv"
    ),
    recommended = c("monitoring_purpose_code","monitoring_program_code")
  ),
  Picoplankton = list(
    required = c(
      "visit_year","station_name","reported_station_name","sample_project_name_sv","sample_orderer_name_sv",
      "platform_code","sample_date","sample_time","sample_latitude_dd","sample_longitude_dd",
      "positioning_system_code","water_depth_m","secchi_depth_m","secchi_depth_quality_flag","sample_min_depth_m",
      "sample_max_depth_m","sampling_laboratory_name_sv","sampling_laboratory_accreditated","sampler_type_code",
      "sampled_volume_l","plankton_sampling_method_code","scientific_name","dyntaxa_id","value","quality_flag",
      "trophic_type_code","size_class","size_class_ref_list","reported_cell_volume_um3","taxonomist",
      "analysis_method_code","method_documentation","method_reference_code","analytical_laboratory_name_sv",
      "analytical_laboratory_accreditated","analysis_date","preservation_method_code","reported_scientific_name",
      "reported_value","reporting_institute_name_sv"
    ),
    recommended = c("monitoring_station_type_code","monitoring_purpose_code","monitoring_program_code")
  ),
  PrimaryProduction = list(
    required = c(
      "visit_year","station_name","reported_station_name","sample_project_name_sv","sample_orderer_name_sv",
      "platform_code","sample_date","sample_time","sample_latitude_dd","sample_longitude_dd",
      "positioning_system_code","water_depth_m","sample_min_depth_m","sample_max_depth_m","sampling_laboratory_name_sv",
      "sampling_laboratory_accreditated","sampler_type_code","sample_comment","DPM_added","DPM_sample","DPM_darkness",
      "value","quality_flag","analysis_method_code","method_documentation","method_reference_code",
      "analytical_laboratory_name_sv","analytical_laboratory_accreditated","incubation_start_time","incubation_end_time",
      "incubation_time_h","reporting_institute_name_sv"
    ),
    recommended = c("monitoring_station_type_code","monitoring_purpose_code","monitoring_program_code")
  ),
  RingedSeal = list(
    required = c(
      "visit_year","station_name","reported_station_name","sample_project_name_sv","sample_orderer_name_sv",
      "sample_date","sample_time","sample_enddate","sample_endtime","sample_latitude_dd","sample_longitude_dd",
      "water_depth_m","sample_min_depth_m","sample_max_depth_m","sampling_laboratory_name_sv","scientific_name",
      "dyntaxa_id","value","quality_flag","sex_code","dev_stage_code","trophic_type_code","size_class",
      "method_documentation","method_reference_code","analytical_laboratory_name_sv","reported_scientific_name",
      "reported_value"
    ),
    recommended = c()
  ),
  SealPathology = list(
    required = c(
      "visit_year","station_name","reported_station_name","sample_project_name_sv","sample_orderer_name_sv",
      "sample_date","sample_time","sample_enddate","sample_endtime","sample_latitude_dd","sample_longitude_dd",
      "water_depth_m","sample_min_depth_m","sample_max_depth_m","sampling_laboratory_name_sv","scientific_name",
      "dyntaxa_id","value","quality_flag","sex_code","dev_stage_code","trophic_type_code","size_class",
      "method_documentation","method_reference_code","analytical_laboratory_name_sv","reported_scientific_name",
      "reported_value"
    ),
    recommended = c()
  ),
  Sedimentation = list(
    required = c(
      "visit_year","station_name","reported_station_name","sample_project_name_sv","sample_orderer_name_sv",
      "platform_code","sample_date","sample_time","sample_enddate","sample_latitude_dd","sample_longitude_dd",
      "positioning_system_code","water_depth_m","sample_min_depth_m","sample_max_depth_m","sample_depth_quality_flag",
      "sampling_laboratory_name_sv","sampling_laboratory_accreditated","sampler_type_code","value","quality_flag",
      "method_documentation","method_reference_code","aggregated_subsamples","analytical_laboratory_name_sv",
      "analytical_laboratory_accreditated","preservation_method_code","mesh_size_um","method_incubation",
      "incubation_start_time","incubation_end_time","incubation_time_h","salinity_correction","reporting_institute_name_sv"
    ),
    recommended = c("monitoring_program_code")
  ),
  Zoobenthos = list(
    required = c(
      "delivery_datatype","check_status_sv","data_checked_by_sv","visit_year","station_name","sample_project_name_sv",
      "sample_orderer_name_sv","platform_code","sample_date","sample_time","sample_latitude_dd","sample_longitude_dd",
      "positioning_system_code","water_depth_m","secchi_depth_m","secchi_depth_quality_flag","sediment_type",
      "sample_min_depth_m","sample_max_depth_m","sampling_laboratory_name_sv","sampling_laboratory_accreditated",
      "sampler_type_code","sampled_volume_l","sampler_area_cm2","scientific_name","dyntaxa_id","value","quality_flag",
      "taxonomist","method_documentation","method_reference_code","analytical_laboratory_name_sv",
      "analytical_laboratory_accreditated","analysed_by","analysis_date","preservation_method_code","upper_mesh_size_um",
      "lower_mesh_size_um","reported_scientific_name","reported_value","reporting_institute_name_sv"
    ),
    recommended = c("monitoring_purpose_code","monitoring_program_code")
  ),
  Zooplankton = list(
    required = c(
      "delivery_datatype","check_status_sv","data_checked_by_sv","visit_year","station_name","sample_project_name_sv",
      "sample_orderer_name_sv","platform_code","sample_date","sample_time","sample_latitude_dd","sample_longitude_dd",
      "positioning_system_code","water_depth_m","secchi_depth_m","secchi_depth_quality_flag","sample_min_depth_m",
      "sample_max_depth_m","sampling_laboratory_name_sv","sampling_laboratory_accreditated","sampler_type_code",
      "sampled_volume_l","sampler_area_cm2","plankton_sampling_method_code","scientific_name","dyntaxa_id","value",
      "quality_flag","sex_code","dev_stage_code","size_class","size_min_um","size_max_um","taxonomist","method_documentation",
      "method_reference_code","calculation_method","analytical_laboratory_name_sv","analytical_laboratory_accreditated",
      "analysed_by","analysis_date","preservation_method_code","mesh_size_um","reported_scientific_name","reported_value",
      "reporting_institute_name_sv"
    ),
    recommended = c("monitoring_purpose_code","monitoring_program_code")
  )
)

.threshold_values <- dplyr::tribble(
  ~parameter,                                ~mild_upper,     ~extreme_upper, ~datatype,
  "Bacterial carbon production",             765842329,       1200706084,     "Bacterioplankton",
  "Bacterial abundance",                     4686343500,      6779382000,     "Bacterioplankton",
  "Bacterial cell carbon content",           18.96,           20.76,          "Bacterioplankton",
  "Chlorophyll-a",                           6.25,            9.4,            "Chlorophyll",
  "Abundance",                85155831,        133564616,      "Picoplankton",
  "Biovolume concentration",  0.05928825,      0.09323008,     "Picoplankton",
  "Carbon concentration",     13.27727,        20.85692,       "Picoplankton",
  "# counted",                461.5,           733,            "Picoplankton",
  "Abundance",                 1092.02,         1731.232,       "Zooplankton",
  "# counted",                 54.5,            86,             "Zooplankton",
  "Length (mean)",                           1286.662,        1898.325,       "Zooplankton",
  "Length (median)",                         1287,            1899,           "Zooplankton",
  "Wet weight",                0.82,            1.3,            "Zooplankton",
  "Carbon content",            3.88,            6.16,           "Zooplankton",
  "Wet weight/volume",                       9.816648,        15.54263,       "Zooplankton",
  "Wet weight/area",                         372.6163,        593.9886,       "Zooplankton",
  "Abundance",               39460,           62920,          "Phytoplankton",
  "Biovolume concentration", 0.01514523,      0.02397705,     "Phytoplankton",
  "Carbon concentration",    1.679784,        2.653602,       "Phytoplankton",
  "# counted",               52,              82,             "Phytoplankton",
  "Carbon production",                       36.6904,         58.41079,       "Primary production",
  "Carbon prod in light",                    36.6904,         58.41079,       "Primary production",
  "Carbon production/hour",                  11.86375,        18.6775,        "Primary production",
  "# counted",                  87,              138,            "Epibenthos",
  "Dry weight",                 0.2303094,       0.367895,       "Epibenthos",
  "Species distribution max depth",          29.3125,         44.425,         "Epibenthos",
  "Species distribution min depth",          13.075,          20.65,          "Epibenthos",
  "# counted",                162.5,           260,            "Harbour seal",
  "# counted",                   397.25,          632,            "Grey seal",
  "BQIm",                                    18.52294,        26.96423,       "Zoobenthos",
  "Abundance",                  185,             290,            "Zoobenthos",
  "# counted",                  21,              33,             "Zoobenthos",
  "Wet weight",                 0.5395,          0.859,          "Zoobenthos",
  "Calculated # counted",      28.247,          41.6792,        "Ringed seal",
  "Porpoise positive minutes",               189.5,           299,            "Harbour Porpoise"
)

# --- Define parameter-specific conditions ---
.param_conditions <- list(
  "Total cover of all species" = list(
    condition = function(x) x > 100,
    range_msg = "0-100%"
  ),
  "Cover" = list(
    condition = function(x) x > 100,
    range_msg = "0-100%"
  ),
  "Cover class" = list(
    condition = function(x) x > 10,
    range_msg = "0-10"
  ),
  "Sediment deposition cover" = list(
    condition = function(x) x > 100,
    range_msg = "0-100%"
  ),
  "Abundance class" = list(
    condition = function(x) x > 10,
    range_msg = "0-10"
  ),
  "Wet weight" = list(
    condition = function(x) x == 0,
    range_msg = "> 0"
  )
)

# --- Row-wise dependent rules ---
.rowwise_conditions <- list(
  "BQIm" = function(df) {
    val <- suppressWarnings(as.numeric(as.character(df$value)))
    is_abund <- df$parameter == "Abundance"
    is_BQIm  <- df$parameter == "BQIm"
    (!is.na(val)) & ((is_abund & val == 0) | (is_BQIm & val > 0))
  }
)

# Translate datatype names
.type_lookup <- c(
  "Bacterioplankton"       = "Bacterioplankton",
  "Chlorophyll"            = "Chlorophyll",
  "Epibenthos"             = "Epibenthos",
  "Grey seal"              = "GreySeal",
  "Harbour Porpoise"       = "HarbourPorpoise",
  "Harbour seal"           = "HarbourSeal",
  "Jellyfish"              = "Jellyfish",
  "Physical and Chemical"  = "PhysicalChemical",
  "Phytoplankton"          = "Phytoplankton",
  "Picoplankton"           = "Picoplankton",
  "Plankton Barcoding"     = "PlanktonBarcoding",
  "Plankton Imaging"       = "PlanktonImaging",
  "Primary production"     = "PrimaryProduction",
  "Profile"                = "Profile",
  "Ringed seal"            = "RingedSeal",
  "Seal pathology"         = "SealPathology",
  "Sedimentation"          = "Sedimentation",
  "Zoobenthos"             = "Zoobenthos",
  "Zooplankton"            = "Zooplankton"
)
