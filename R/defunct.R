#' Defunct functions
#'
#' @description
#' `r lifecycle::badge("defunct")`
#'
#' These functions were deprecated before being made defunct.
#' If there's a known replacement, calling the function
#' will tell you about it.
#'
#' @param ... Arguments previously accepted by the function. Ignored.
#' @keywords internal
#' @name defunct
NULL

# Deprecated in 1.0.0 -------------------------------------

#' @rdname defunct
get_shark_table <- function(...) {
  lifecycle::deprecate_stop("1.0.0", "get_shark_table()", "get_shark_data()", "get_shark_table() is defunct due to inefficiency in handling large datasets.")
}

# Deprecated in 0.1.4 -------------------------------------

#' @rdname defunct
check_data_version <- function(...) {
  lifecycle::deprecate_stop("0.1.4", "check_data_version()", "get_shark_data()", "The SHARKdata API has been replaced by the SHARK API.")
}
#' @rdname defunct
update_data <- function(...) {
  lifecycle::deprecate_stop("0.1.4", "update_data()", "get_shark_data()", "The SHARKdata API has been replaced by the SHARK API.")
}
#' @rdname defunct
download_sharkdata <- function(...) {
  lifecycle::deprecate_stop("0.1.4", "download_sharkdata()", "get_shark_data()", "The SHARKdata API has been replaced by the SHARK API.")
}
#' @rdname defunct
load_dataset_names <- function(...) {
  lifecycle::deprecate_stop("0.1.4", "load_dataset_names()", "get_shark_options()", "The SHARKdata API has been replaced by the SHARK API.")
}
#' @rdname defunct
load_sharkdata <- function(...) {
  lifecycle::deprecate_stop("0.1.4", "load_sharkdata()", "get_shark_options()", "The SHARKdata API has been replaced by the SHARK API.")
}
#' @rdname defunct
load_dataset_types <- function(...) {
  lifecycle::deprecate_stop("0.1.4", "load_dataset_types()", "get_shark_options()", "The SHARKdata API has been replaced by the SHARK API.")
}
#' @rdname defunct
download_file <- function(...) {
  lifecycle::deprecate_stop("0.1.4", "download_file()", "get_shark_data()", "The SHARKdata API has been replaced by the SHARK API.")
}
#' @rdname defunct
year_filter <- function(...) {
  lifecycle::deprecate_stop("0.1.4", "year_filter()", "get_shark_data()", "The SHARKdata API has been replaced by the SHARK API and this function is no longer needed.")
}
#' @rdname defunct
data_deliverer_filter <- function(...) {
  lifecycle::deprecate_stop("0.1.4", "data_deliverer_filter()", "get_shark_data()", "The SHARKdata API has been replaced by the SHARK API and this function is no longer needed.")
}
#' @rdname defunct
read_data <- function(...) {
  lifecycle::deprecate_stop("0.1.4", "read_data()", "get_shark_data()", "The SHARKdata API has been replaced by the SHARK API and this function is no longer needed.")
}
#' @rdname defunct
validate_dataset_names <- function(...) {
  lifecycle::deprecate_stop("0.1.4", "validate_dataset_names()", "get_shark_data()", "The SHARKdata API has been replaced by the SHARK API and this function is no longer needed.")
}
#' @rdname defunct
load_dyntaxa_taxonomy <- function(...) {
  lifecycle::deprecate_stop("0.1.4", "load_dyntaxa_taxonomy()", "get_dyntaxa_records()")
}
#' @rdname defunct
read_species_list <- function(...) {
  lifecycle::deprecate_stop("0.1.4", "read_species_list()", "get_dyntaxa_records()")
}
#' @rdname defunct
gather_species_info <- function(...) {
  lifecycle::deprecate_stop("0.1.4", "gather_species_info()", "get_dyntaxa_records()")
}
#' @rdname defunct
add_species_info <- function(...) {
  lifecycle::deprecate_stop("0.1.4", "add_species_info()", "get_dyntaxa_records()")
}
#' @rdname defunct
load_worms_taxonomy <- function(...) {
  lifecycle::deprecate_stop("0.1.4", "load_worms_taxonomy()", "add_worms_taxonomy()")
}
#' @rdname defunct
gather_worms_species_info <- function(...) {
  lifecycle::deprecate_stop("0.1.4", "gather_worms_species_info()", "add_worms_taxonomy()")
}
#' @rdname defunct
add_worms_species_info <- function(...) {
  lifecycle::deprecate_stop("0.1.4", "add_worms_species_info()", "add_worms_taxonomy()")
}
#' @rdname defunct
get_geographical_info <- function(...) {
  lifecycle::deprecate_stop("0.1.4", "get_geographical_info()", "ifcb_which_basin()")
}
#' @rdname defunct
filter_outdated_datasets <- function(...) {
  lifecycle::deprecate_stop("0.1.4", "filter_outdated_datasets()", "get_shark_data()", "The SHARKdata API has been replaced by the SHARK API and this function is no longer needed.")
}
