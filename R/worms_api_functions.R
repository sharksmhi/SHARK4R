#' Update taxonomy from SHARKdata datasets via WoRMS API
#'
#' This function collects WoRMS (World Register of Marine Species) taxonomy information for a given set of Aphia IDs.
#' The data is organized into a full taxonomic table that can be joined with data downloaded from [SHARKdata](https://sharkdata.smhi.se/)
#'
#' @param aphiaid A numeric vector containing Aphia IDs for which WoRMS taxonomy needs to be updated.
#'
#' @return A data frame containing updated WoRMS taxonomy information.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Update WoRMS taxonomy for a set of Aphia IDs
#' updated_taxonomy <- update_worms_taxonomy(c(149619, 149122, 11))
#' print(updated_taxonomy)
#' }
#'
#' @seealso \code{\link{download_sharkdata}}, \code{\link{update_dyntaxa_taxonomy}}, [WoRMS API Documentation](http://www.marinespecies.org/rest/)
#'
update_worms_taxonomy <- function(aphiaid) {
  worms_class <- data.frame()
  for (ids in aphiaid) {
    tryCatch({
      worms_class_i <- wm_classification(ids) %>%
        select(-AphiaID) %>%
        mutate(scientific_name = last(scientificname)) %>%
        pivot_wider(names_from = rank, values_from = scientificname) %>%
        mutate(worms_hierarchy = paste(na.omit(.), collapse = " - "),
               aphia_id = ids) %>%
        mutate(Kingdom = ifelse("Kingdom" %in% names(.), Kingdom, NA),
               Phylum = ifelse("Phylum" %in% names(.), Phylum, NA),
               Class = ifelse("Class" %in% names(.), Class, NA),
               Order = ifelse("Order" %in% names(.), Order, NA),
               Family = ifelse("Family" %in% names(.), Family, NA),
               Genus = ifelse("Genus" %in% names(.), Genus, NA),
               Species = ifelse("Species" %in% names(.), Species, NA)) %>%
        rename(worms_kingdom = Kingdom,
               worms_phylum = Phylum,
               worms_class = Class,
               worms_order = Order,
               worms_family = Family,
               worms_genus = Genus,
               worms_species = Species)
    }, error=function(e) {
      worms_class_i <<- data.frame(aphia_id = ids)
    })
    worms_class <- bind_rows(worms_class, worms_class_i)
  }

  names <- c("aphia_id", "scientific_name", names(worms_class)[grepl("worms_", names(worms_class))])
  
  return(worms_class %>%
           select(any_of(names)) %>%
           relocate(worms_hierarchy, .after = last_col()))
}
