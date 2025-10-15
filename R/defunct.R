#' Defunct functions
#'
#' @description
#' `r lifecycle::badge("defunct")`
#'
#' These functions were deprecated before being made defunct.
#' If there's a known replacement, calling the function
#' will tell you about it.
#'
#' @keywords internal
#' @name defunct
NULL

#' @usage # Deprecated in 1.0.0 -------------------------------------
#' @name defunct
NULL
#' @noRd
#' @rdname defunct
get_shark_table <- function(...) {
  lifecycle::deprecate_stop("1.0.0", "get_shark_table()", "get_shark_data()", "get_shark_table() is defunct due to inefficiency in handling large datasets.")
}
#' @usage # Deprecated in 0.1.4 -------------------------------------
#' @name defunct
NULL
#' @noRd
#' @rdname defunct
check_data_version <- function(...) {
  lifecycle::deprecate_stop("0.1.4", "check_data_version()", "get_shark_data()", "The SHARKdata API has been replaced by the SHARK API.")
}
#' @noRd
#' @rdname defunct
update_data <- function(...) {
  lifecycle::deprecate_stop("0.1.4", "update_data()", "get_shark_data()", "The SHARKdata API has been replaced by the SHARK API.")
}
#' @noRd
#' @rdname defunct
download_sharkdata <- function(...) {
  lifecycle::deprecate_stop("0.1.4", "download_sharkdata()", "get_shark_data()", "The SHARKdata API has been replaced by the SHARK API.")
}
#' @noRd
#' @rdname defunct
load_dataset_names <- function(...) {
  lifecycle::deprecate_stop("0.1.4", "load_dataset_names()", "get_shark_options()", "The SHARKdata API has been replaced by the SHARK API.")
}
#' @noRd
#' @rdname defunct
load_sharkdata <- function(...) {
  lifecycle::deprecate_stop("0.1.4", "load_sharkdata()", "get_shark_options()", "The SHARKdata API has been replaced by the SHARK API.")
}
#' @noRd
#' @rdname defunct
load_dataset_types <- function(...) {
  lifecycle::deprecate_stop("0.1.4", "load_dataset_types()", "get_shark_options()", "The SHARKdata API has been replaced by the SHARK API.")
}
#' @noRd
#' @rdname defunct
download_file <- function(...) {
  lifecycle::deprecate_stop("0.1.4", "download_file()", "get_shark_data()", "The SHARKdata API has been replaced by the SHARK API.")
}
#' @noRd
#' @rdname defunct
year_filter <- function(datasets, year) {
  lifecycle::deprecate_stop("0.1.4", "year_filter()", "get_shark_data()", "The SHARKdata API has been replaced by the SHARK API and this function is no longer needed.")
}
#' @noRd
#' @rdname defunct
data_deliverer_filter <- function(datasets, data_deliverer) {
  lifecycle::deprecate_stop("0.1.4", "data_deliverer_filter()", "get_shark_data()", "The SHARKdata API has been replaced by the SHARK API and this function is no longer needed.")
}
#' @noRd
#' @rdname defunct
read_data <- function(temp, filtered_datasets, dataset_name) {
  lifecycle::deprecate_stop("0.1.4", "read_data()", "get_shark_data()", "The SHARKdata API has been replaced by the SHARK API and this function is no longer needed.")
}
#' @noRd
#' @rdname defunct
validate_dataset_names <- function(filtered_datasets, dataset_names) {
  lifecycle::deprecate_stop("0.1.4", "validate_dataset_names()", "get_shark_data()", "The SHARKdata API has been replaced by the SHARK API and this function is no longer needed.")
}
#' @noRd
#' @rdname defunct
load_dyntaxa_taxonomy <- function(dyntaxa_id_input) {
  lifecycle::deprecate_stop("0.1.4", "load_dyntaxa_taxonomy()", "get_dyntaxa_records()")
}
#' @noRd
#' @rdname defunct
read_species_list <- function(filename) {
  lifecycle::deprecate_stop("0.1.4", "read_species_list()", "get_dyntaxa_records()")
}
#' @noRd
#' @rdname defunct
gather_species_info <- function(shark_species_list) {
  lifecycle::deprecate_stop("0.1.4", "gather_species_info()", "get_dyntaxa_records()")
}
#' @noRd
#' @rdname defunct
add_species_info <- function(shark_species_list, species) {
  lifecycle::deprecate_stop("0.1.4", "add_species_info()", "get_dyntaxa_records()")
}
#' @noRd
#' @rdname defunct
load_worms_taxonomy <- function(aphia_id_input) {
  lifecycle::deprecate_stop("0.1.4", "load_worms_taxonomy()", "add_worms_taxonomy()")
}
#' @noRd
#' @rdname defunct
gather_worms_species_info <- function(taxa_worms) {
  lifecycle::deprecate_stop("0.1.4", "gather_worms_species_info()", "add_worms_taxonomy()")
}
#' @noRd
#' @rdname defunct
add_worms_species_info <- function(taxa_worms, species) {
  lifecycle::deprecate_stop("0.1.4", "add_worms_species_info()", "add_worms_taxonomy()")
}
#' @noRd
#' @rdname defunct
get_geographical_info <- function(latitude_dd, longitude_dd) {
  lifecycle::deprecate_stop("0.1.4", "get_geographical_info()", "ifcb_which_basin()")
}
#' @noRd
#' @rdname defunct
filter_outdated_datasets <- function(data, datasets_to_update) {
  lifecycle::deprecate_stop("0.1.4", "filter_outdated_datasets()", "get_shark_data()", "The SHARKdata API has been replaced by the SHARK API and this function is no longer needed.")
}
