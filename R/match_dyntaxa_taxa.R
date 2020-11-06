#' Taxon matching using Dyntaxa (https://www.dyntaxa.se/)
#' matches latin name in data with Dyntaxa taxon list
#' @param names Vector of scientific names.
#' @param ask Ask user in case of multiple matches.
#' @return Data frame with scientific name, scientific name ID and match type.
#' @export

match_dyntaxa <- function(names) {

  f <- as.factor(names)
  indices <- as.numeric(f)
  unames <- levels(f)

  dyntaxa_db <- read_delim(system.file("extdata", "dyntaxa_Biota.txt", package = "SHARK4R"), delim ="\t", guess_max = 2000, col_names = T, locale = readr::locale(encoding = "latin1"))

  match_index <- match(names, dyntaxa_db$ScientificName)

  match_type <- names %in% dyntaxa_db$ScientificName

  matches <- data.frame(reported_ScientificName = names, match_type = match_type)

  if (length(which(match_type == FALSE))>0) {
    message("Unmatched taxa found")
    print(matches[!match_type,])
  }
  else {
    message("All taxa found")
  }
}
