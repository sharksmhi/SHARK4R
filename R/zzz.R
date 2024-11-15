# suppress warning for "no visible binding for global variable" in R CMD check
utils::globalVariables(c("visit_year", "station_name", "sample_project_name_sv",
                         "sample_orderer_name_sv", "platform_code", "sample_date",
                         "sample_latitude_dd", "sample_longitude_dd", "positioning_system_code",
                         "water_depth_m", "Statistic", "x", "y", "ymin", "ymax", "lower",
                         "middle", "upper", "Value", "Ok", "sample_id", "shark_sample_id_md5", 
                         "sample_min_depth_m", "sample_max_depth_m", "parameter", "value",
                         "taxon_species", "taxon_genus", "worms_species", "worms_genus", 
                         "scientific_name", "nameShort", "isRecommended",
                         "usage.name", "usage.value", "taxonId", ".", "Species", "taxonId_recommended",
                         "acceptedNameUsageID", "parentNameUsageID", "scientificName", "taxonRank", 
                         "scientificNameAuthorship", "taxonomicStatus", "nomenclaturalStatus", 
                         "taxonRemarks", "dataset_name", "taxon_name", "aphia_id", "TYPOMRNAMN", 
                         "NAMN", "BASIN_NR", "lon", "lat", "geometry", "datatype", "dyntaxa_id", 
                         "family", "genus", "taxon_id", "scientificname", "authority", "status", 
                         "match_type", "STATN", "LONGI", "LATIT",
                         "author", "guid", "name", "Kingdom", "Phylum", "Class", "Order", "Family", 
                         "Genus", "Species", "hierarchy", "AphiaID", "worms_hierarchy", "taxon_hierarchy",
                         "up_to_date", "Data_field", "DT"))

.onLoad <- function(libname, pkgname){
  clear_cache(age=36)
}
