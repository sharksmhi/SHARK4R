# suppress warning for "no visible binding for global variable" in R CMD check
utils::globalVariables(c("visit_year", "station_name", "sample_project_name_sv", "threshold",
                         "sample_orderer_name_sv", "platform_code", "sample_date",
                         "sample_latitude_dd", "sample_longitude_dd", "positioning_system_code",
                         "water_depth_m", "Statistic", "x", "y", "ymin", "ymax", "lower",
                         "middle", "upper", "Value", "Ok", "sample_id", "shark_sample_id_md5",
                         "sample_min_depth_m", "sample_max_depth_m", "parameter", "value",
                         "taxon_species", "taxon_genus", "worms_species", "worms_genus",
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
                         "RADIUS", "STATION", "STATION_NAME"))

.onLoad <- function(libname, pkgname){
  clean_shark4r_cache(days = 1, verbose = FALSE)
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
  ),
  deliv_Bacterioplankton = list(
    required = c(
      "MYEAR","STATN","PROJ","ORDERER","SHIPC","SERNO","SDATE","LATIT","LONGI","POSYS",
      "WADEP","MSTAT","MNDEP","MXDEP","SLABO","ACKR_SMP","SMTYP",
      "SMVOL","CHLA_CONC","QFLAG","UNCERT","METCU","DETLI","LMQNT","RANA","ALABO",
      "ACKR_ANA","ANADATE","METDC"
    ),
    recommended = c(
      "CRUISE_NO","COMNT_VISIT","WINDIR","WINSP","AIRTEMP","AIRPRES","WEATH","CLOUD",
      "WAVES","ICEOB","SECCHI","Q_SECCHI","STIME","SMPNO","COMNT_SAMP","METOA","REFSK",
      "COMNT_VAR"
    )
  ),
  deliv_Chlorophyll = list(
    required = c(
      "MYEAR","STATN","PROJ","ORDERER","SHIPC","SDATE","LATIT","LONGI","POSYS","WADEP",
      "MSTAT","MNDEP","MXDEP","SLABO","ACKR_SMP","SMTYP","SMVOL","CHLA_CONC",
      "QFLAG","UNCERT","METCU","DETLI","LMQNT","RANA","ALABO","ACKR_ANA","ANADATE","METDC"
    ),
    recommended = c(
      "CRUISE_NO","SERNO","COMNT_VISIT","WINDIR","WINSP","AIRTEMP","AIRPRES","WEATH",
      "CLOUD","WAVES","ICEOB","SECCHI","Q_SECCHI","STIME","SMPNO","COMNT_SAMP","METOA",
      "REFSK","COMNT_VAR"
    )
  ),
  deliv_Epibenthos = list(
    required = c(
      "MYEAR","STATN","PROJ","ORDERER","SDATE","SLABO","COMNT_VISIT","LATIT","LONGI",
      "POSYS","TRANS_ID","TRANS_MIN","TRANS_MAX","TRANS_LAT_START","TRANS_LONG_START",
      "TRANS_LAT_END","TRANS_LONG_END","TRANS_START_DEP","TRANS_STOP_DEP","SAMPLEID",
      "SECTIONMIN","SECTIONMAX","SECTIONSTA","SECTIONSTO","SAREA","SUBST","SUBST_COVER",
      "LATNM","SFLAG","TAXNM","COVER%","COUNTNR","ABUND","ALABO","SMTYP","REFSK","METDC"
    ),
    recommended = c(
      "STTYP","MPROG","PURPM","MSTAT","RLABO","CRUIS","SHIPC","NTYPE","WATLD","WADEP_COR",
      "WAVXP","TRANSL","TRANSW","TRANSDIR","COMNT_TRANS","SMDEP","RPSNO","COMNT_SAMP",
      "DEPOS","DEPOS%","FNFLA","STRID","COMNT_VAR","COVER_1-4","COVER_1-5","COVER_1-7",
      "TOTCOVERAGE%","METOA"
    )
  ),
  deliv_EpibenthosDropvideo = list(
    required = c(
      "MYEAR","STATN","PROJ","ORDERER","SDATE","SLABO","ACKR_SMP","NTYPE","SAL",
      "SECCHI","Q_SECCHI","COMNT_VISIT","TRANS_ID","TRANS_LAT_START","TRANS_LONG_START",
      "TRANS_LAT_STOP","TRANS_LONG_STOP","TRANS_START_DEP","TRANS_STOP_DEP","VID_NAME",
      "VIDEO_INTER","PROTO_WRIT","DEPOS","IMAGE_NAME","IMAGE_STOP_TIME","ALABO","ACKR_ANA",
      "ANADATE","METDC"
    ),
    recommended = c(
      "POSYS","TRANS_INTERPRET_TIME","COMNT_SAMP","COVER_HARD_CLAY","COVER_SOFT_CLAY_<0.002",
      "COVER_SILT_0.002_0.06","COVER_SILT_CLAY","COVER_SAND_0.06_2","COVER_GRAVEL_FINE+MEDIUM_2_20",
      "COVER_GRAVEL_COARSE_20_60","COVER_COBBLE_MEDIUM_60_200","COVER_COBBLE_COARSE_200_600",
      "COVER_LARGE_BOULDER_>600","COVER_ROCK","COVER_SHELL_GRAVEL","COVER_SHELL","COVER_BARE_SUBST",
      "COVER_UNIDENT_SUBST","COVER_DETRITUS","COVER_EPI_ZOSTERA","COVER_UNIDENT_PLANTAE",
      "COVER_NASSARIUS_TRACKS","COVER_PAGURIDAE_TRACKS","COVER_ANIMALIA_BURROWS",
      "COVER_ANIMALIA_TRACKS","COVER_UNIDENTIFIED_ALGAE","REFSK","COMNT_VAR"
    )
  ),
  deliv_GreySeal = list(
    required = c(
      "MYEAR","STATN","PROJ","ORDERER","SDATE","LATIT","LONGI","POSYS","PURPM","MPROG",
      "SLABO","ACKR_SMP","SMTYP","LATNM","COUNT","ALABO","ACKR_ANA","ANADATE","METDC"
    ),
    recommended = c(
      "REGION","SHIPC","CRUISE_NO","COMNT_VISIT","WINDIR","WINSP","AIRTEMP","AIRPRES",
      "WEATH","CLOUD","WAVES","ICEOB","STIME","FNFLA","SMP_BY","OBSPOINT","OBSDIST",
      "IMAGE_ID","SMPNO","COMNT_SAMP","REFSK","COMNT_VAR"
    )
  ),
  deliv_HarbourPorpoise = list(
    required = c(
      "MYEAR","STATN","PROJ","ORDERER","SDATE","EDATE","POSYS","SLABO","SMTYP","LATIT",
      "LONGI","METDC","LATNM","DPM","ODATE","OTIME","ALABO","ACKR_SMP","RAW"
    ),
    recommended = c("STIME","ETIME","COMNT_VISIT","COMNT_SAMP","COMNT_VAR")
  ),
  deliv_HarbourSeal = list(
    required = c(
      "MYEAR","STATN","PROJ","ORDERER","SDATE","LATIT","LONGI","POSYS","PURPM","MPROG",
      "SLABO","ACKR_SMP","SMTYP","LATNM","COUNT","ALABO","ACKR_ANA","ANADATE","METDC"
    ),
    recommended = c(
      "REGION","SHIPC","CRUISE_NO","COMNT_VISIT","WINDIR","WINSP","AIRTEMP","AIRPRES",
      "WEATH","CLOUD","WAVES","ICEOB","STIME","FNFLA","SMP_BY","OBSPOINT","OBSDIST",
      "IMAGE_ID","SMPNO","COMNT_SAMP","REFSK","COMNT_VAR"
    )
  ),
  deliv_PhysicalChemical = list(
    required = c(
      "MYEAR","PROJ","ORDERER","SDATE","STIME","SHIPC","SERNO","STATN","LATIT","LONGI",
      "POSYS","DEPH"
    ),
    recommended = c(
      "EDATE","ETIME","CRUISE_NO","WADEP","NODEPTH","ADD_SMP","COMNT_VISIT","WINDIR",
      "WINSP","AIRTEMP","AIRPRES","WEATH","CLOUD","WAVES","ICEOB","SECCHI","Q_SECCHI",
      "SMPNO","PRES_CTD","Q_PRES_CTD","TEMP_BTL","Q_TEMP_BTL","TEMP_CTD","Q_TEMP_CTD",
      "SALT_BTL","Q_SALT_BTL","SALT_CTD","Q_SALT_CTD","CNDC_25","Q_CNDC_25","CNDC_CTD",
      "Q_CNDC_CTD","DOXY_BTL","Q_DOXY_BTL","DOXY_CTD","Q_DOXY_CTD","H2S","Q_H2S","PH",
      "Q_PH","PH_LAB","Q_PH_LAB","PH_LAB_TEMP","Q_PH_LAB_TEMP","ALKY","Q_ALKY","PHOS",
      "Q_PHOS","PTOT","Q_PTOT","NTRI","Q_NTRI","NTRA","Q_NTRA","NTRZ","Q_NTRZ","AMON",
      "Q_AMON","NTOT","Q_NTOT","SIO3-SI","Q_SIO3-SI","HUMUS","Q_HUMUS","CPHL","Q_CPHL",
      "DOC","Q_DOC","POC","Q_POC","TOC","Q_TOC","PON","Q_PON","CURDIR","Q_CURDIR",
      "CURVEL","Q_CURVEL","AL","Q_AL","CDOM","Q_CDOM","COMNT_SAMP"
    )
  ),
  deliv_Phytoplankton = list(
    required = c(
      "MYEAR","STATN","PROJ","ORDERER","SHIPC","SDATE","LATIT","LONGI","POSYS",
      "WADEP","MSTAT","MNDEP","MXDEP","SLABO","ACKR_SMP","SMTYP","SMVOL","LATNM",
      "SFLAG","TRPHY","COUNT","COEFF","SIZCL","SIZRF","SDVOL","QFLAG","TAXNM",
      "METOA","ALABO","ACKR_ANA","ANADATE","METDC"
    ),
    recommended = c(
      "CRUISE_NO","COMNT_VISIT","WINDIR","WINSP","AIRTEMP","AIRPRES","WEATH","CLOUD",
      "WAVES","ICEOB","SECCHI","Q_SECCHI","STIME","PDMET","METFP","SMPNO","COMNT_SAMP",
      "ABUND","ABUND_CLASS","C_CONC","BIOVOL","SDTIM","MAGNI","COUNTPROG","REFSK","COMNT_VAR"
    )
  ),

  deliv_Picoplankton = list(
    required = c(
      "MYEAR","STATN","PROJ","ORDERER","SHIPC","SDATE","LATIT","LONGI","POSYS",
      "WADEP","MSTAT","MNDEP","MXDEP","SLABO","ACKR_SMP","SMTYP","SMVOL","LATNM",
      "SFLAG","TRPHY","COUNT","COEFF","SIZCL","SIZRF","SDVOL","QFLAG","TAXNM",
      "METOA","ALABO","ACKR_ANA","ANADATE","METDC"
    ),
    recommended = c(
      "CRUISE_NO","COMNT_VISIT","WINDIR","WINSP","AIRTEMP","AIRPRES","WEATH","CLOUD",
      "WAVES","ICEOB","SECCHI","Q_SECCHI","STIME","PDMET","METFP","SMPNO","COMNT_SAMP",
      "ABUND","ABUND_CLASS","C_CONC","BIOVOL","SDTIM","MAGNI","COUNTPROG","REFSK","COMNT_VAR"
    )
  ),

  deliv_PrimaryProduction = list(
    required = c(
      "MYEAR","STATN","PROJ","ORDERER","SHIPC","SDATE","LATIT","LONGI","POSYS",
      "WADEP","MNDEP","MXDEP","SLABO","ACKR_SMP","SMTYP","SUBNO","INCUR","DPMA",
      "DPMS","INTIM","PROD","ALABO","ACKR_ANA","METDC"
    ),
    recommended = c(
      "CRUISE_NO","SERNO","MSTAT","COMNT_VISIT","WINDIR","WINSP","AIRTEMP","AIRPRES",
      "WEATH","CLOUD","WAVES","ICEOB","SECCHI","Q_SECCHI","STIME","SMPNO","COMNT_SAMP",
      "DARK_PROD","TOTALPROD","TEMP_BTL","SALT_BTL","PH","ALKY","CO2TOT","QFLAG",
      "REFSK","COMNT_VAR"
    )
  ),

  deliv_RingedSeal = list(
    required = c(
      "MYEAR","PROJ","ORDERER","SDATE","POSYS","SLABO","ACKR_SMP","SMTYP","TRANS_ID",
      "FNFLA","TRANS_LAT_START","TRANS_LONG_START","TRANS_LAT_STOP","TRANS_LONG_STOP",
      "LATIT","LONGI","TRANSL","TRANSW","COMNT_SAMP","LATNM","COUNT","TAXNM",
      "COEFF","ALABO","ACKR_ANA","METDC"
    ),
    recommended = c(
      "COMNT_VISIT","WINDIR","WINSP","AIRTEMP","AIRPRES","WEATH","CLOUD","WAVES",
      "ICEOB","STIME","IMAGE_ID","COMNT_VAR"
    )
  ),

  deliv_SealPathology = list(
    required = c(
      "MYEAR","STATN","FCST","PROJ","ORDERER","SDATE","SLABO","ACKR_SMP","LATNM",
      "MATRX","SEXCODE","AGE","BODY_LGTH","BODY_WGTH","BLUBB_THICK","AUTOLYSIS_CL",
      "REG_SKIN_CHANGE_CL","CLAW_LESION_CL","CRONIC_CHOLANGIOHEPATITIS",
      "INTEST_ULCER_CL","PNEUMONIA","ARTERIOSCLEROSIS_CL","ADRENO_CORTHYPERPLASIA_CL",
      "UTERUS_PREG","UTERUS_STENOSIS_OCCLUSION","UTERUS_TUMOUR","CAUSE_OF_DEATH",
      "ALABO","ACKR_ANA","ANADATE","METDC"
    ),
    recommended = c(
      "LATIT","LONGI","COMNT_VISIT","SMPNO","COMNT_SAMP","PARODONTITIS_CL",
      "KIDNEY_GLOMERULAR_CHANGE","KIDNEY_TUBULAR_HYPERPLASIA","REFSK","COMNT_VAR"
    )
  ),

  deliv_Sedimentation = list(
    required = c(
      "MYEAR","STATN","PROJ","ORDERER","SHIPC","SDATE","LATIT","LONGI","POSYS","WADEP",
      "MSTAT","SMDEP","SLABO","ACKR_SMP","SMTYP","SMVOL","MESH","ALABO","ACKR_ANA",
      "ANADATE","METDC","S_COR_FAC","SED_DW","QDW","SED_C%","QC","SED_N%","QN",
      "SED_P%","QP"
    ),
    recommended = c(
      "COMNT_EXP","WINDIR","WINSP","WEATH","CLOUD","WAVES","ICEOB","STIME","QDEP",
      "SMPNO","COMNT_SAMP","METFP","REFSK","COMNT_VAR"
    )
  ),

  deliv_Zoobenthos = list(
    required = c(
      "MYEAR","STATN","PROJ","ORDERER","SHIPC","SDATE","LATIT","LONGI","POSYS","WADEP",
      "MSTAT","SLABO","ACKR_SMP","SMTYP","SMVOL","MESH_LOWER","LATNM","SFLAG","COUNT",
      "WETWT","QFLAG","STAGE","TAXNM","ALABO","ACKR_ANA","ANADATE","METDC"
    ),
    recommended = c(
      "CRUISE_NO","COMNT_VISIT","WINDIR","WINSP","AIRTEMP","AIRPRES","WEATH","CLOUD",
      "WAVES","ICEOB","STIME","DEPH","SDTYP","H2S_SMELL","SECCHI","Q_SECCHI","FNFLA",
      "SAREA","METFP","MESH_UPPER","SMPNO","COMNT_SAMP","REFSK","COMNT_VAR","SPMAX",
      "SPMIN","COLOUR_SED_C","LOIGN","WATER_CONT_SED","REDOX_POT_SED"
    )
  ),

  deliv_Zooplankton = list(
    required = c(
      "MYEAR","STATN","PROJ","ORDERER","SHIPC","SDATE","LATIT","LONGI","POSYS","WADEP",
      "MSTAT","MNDEP","MXDEP","SLABO","ACKR_SMP","SMTYP","SMVOL","SAREA","CLMET",
      "MESHS","NPORT","CPORT","LATNM","SFLAG","COUNT","WETWT","BODY_LGTH","QFLAG",
      "SEXCODE","STAGE","TAXNM","ALABO","ACKR_ANA","ANADATE","METDC"
    ),
    recommended = c(
      "CRUISE_NO","SERNO","COMNT_VISIT","WINDIR","WINSP","AIRTEMP","AIRPRES","WEATH",
      "CLOUD","WAVES","ICEOB","SECCHI","Q_SECCHI","STIME","WIRAN","PDMET","FLOW_READ",
      "METFP","SMPNO","COMNT_SAMP","SPLIT","MAGNI","LENGTH_MEAN","LENGTH_MEDIAN",
      "CCONT","BMCCONT","REFSK","COMNT_VAR"
    )
  )
)

.threshold_values <- tibble::tribble(
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
