#' Taxon matching using WoRMS (http://www.marinespecies.org/)
#' matches latin name in data with WoRMS taxon list
#' @param names Vector of scientific names.
#' @param ask Ask user in case of multiple matches.
#' @return Data frame with scientific name, scientific name ID and match type.
#' @export

match_wormstaxa <- function(names, ask = TRUE) {

  f <- as.factor(names)
  indices <- as.numeric(f)
  unames <- levels(f)

  pages <- split(unames, as.integer((seq_along(unames) - 1) / 50))
  paged_worms_taxamatch_call <- function(page) { cache_call(page, expression(worrms::wm_records_taxamatch(page)))}
  matches <- unlist(lapply(pages, paged_worms_taxamatch_call), recursive = FALSE)

  results <- data.frame(scientificName = character(), match_type = character(), stringsAsFactors = FALSE)

  # count no matches and multiple matches

  no <- NULL
  multiple <- NULL
  for (i in 1:length(matches)) {
    if (is.data.frame(matches[[i]])) {
      if (nrow(matches[[i]]) > 1) {
        multiple <- c(multiple, unames[i])
      }
    } else {
      no <- c(no, unames[i])
    }
  }

  message(sprintf("%s names, %s without matches, %s with multiple matches", length(unames), length(no), length(multiple)))

  # ask user to resolve names, skip, or print names with multiple matches

  if (ask) {
    proceed <- NA
    while (is.na(proceed)) {
      r <- readline(prompt = "Proceed to resolve names (y/n/info)? ")
      if (r == "y") {
        proceed <- TRUE
      } else if (r == "n") {
        proceed <- TRUE
        ask <- FALSE
      } else if (substr(r, 1, 1) == "i") {
        print(multiple)
      }
    }
  }

  # populate data frame

  for (i in seq_along(matches)) {

    row <- list(scientificName = NA, match_type = NA)

    match <- matches[[i]]
    if (is.data.frame(match)) {

      if (nrow(match) == 1) {

        # single match

        row$scientificName = match$scientificname
        row$match_type = match$match_type

      } else if (ask) {

        # multiple matches

        print(match %>% select(scientificname, authority, status, match_type))
        message(unames[i])
        n <- readline(prompt = "Multiple matches, pick a number or leave empty to skip: ")
        s <- as.integer(n)
        if (!is.na(n) & n > 0 & n <= nrow(match)) {
          row$scientificName = match$scientificname[s]
          row$match_type = match$match_type[s]
        }

      }

    }

    results <- bind_rows(results, row)

  }

  return(results[indices,])
}
