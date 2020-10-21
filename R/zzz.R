# suppress warning for "no visible binding for global variable" in R CMD check
utils::globalVariables(c("visit_year", "station_name", "sample_project_name_sv",
                         "sample_orderer_name_sv", "platform_code", "sample_date",
                         "sample_latitude_dd", "sample_longitude_dd", "positioning_system_code",
                         "water_depth_m", "Statistic", "x", "y", "ymin", "ymax", "lower",
                         "middle", "upper", "Value", "Ok"))

.onLoad <- function(libname, pkgname){
  clear_cache(age=36)
}
