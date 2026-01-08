#' Retrieve and organize WoRMS taxonomy for SHARK Aphia IDs
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function was deprecated and replaced by a function with more accurate name. Use [add_worms_taxonomy()] instead.
#'
#' This function collects WoRMS (World Register of Marine Species) taxonomy information for a given set of Aphia IDs.
#' The data is organized into a full taxonomic table that can be joined with data downloaded from [SHARK](https://shark.smhi.se/en/).
#'
#' @param aphia_id A numeric vector containing Aphia IDs for which WoRMS taxonomy needs to be updated.
#' @param aphiaid
#'     `r lifecycle::badge("deprecated")`
#'     Use \code{aphia_id} instead.
#'
#' @return A `tibble` containing updated WoRMS taxonomy information.
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#' \donttest{
#' # Update WoRMS taxonomy for a set of Aphia IDs
#' updated_taxonomy <- update_worms_taxonomy(c(149619, 149122, 11))
#' print(updated_taxonomy)
#' }
#' @seealso \url{https://marinespecies.org/} for WoRMS website.
#' @seealso \code{\link{get_shark_data}}, \code{\link{update_dyntaxa_taxonomy}}, [WoRMS API Documentation](https://www.marinespecies.org/rest/), \url{https://CRAN.R-project.org/package=worrms}
#'
update_worms_taxonomy <- function(aphia_id, aphiaid=deprecated()) {
  if (is_present(aphiaid)) {

    # Signal the deprecation to the user
    deprecate_warn("0.1.3", "SHARK4R::update_worms_taxonomy(aphiaid = )", "SHARK4R::update_worms_taxonomy(aphia_id = )")

    # Deal with the deprecated argument for compatibility
    aphia_id <- aphiaid
  }

  lifecycle::deprecate_warn("0.1.3", "SHARK4R::update_worms_taxonomy()", "SHARK4R::add_worms_taxonomy()")

  add_worms_taxonomy(aphia_id)
}

#' Add WoRMS taxonomy hierarchy to AphiaIDs or scientific names
#'
#' This function enhances a dataset of AphiaIDs (and optionally scientific names)
#' with their complete hierarchical taxonomy from the World Register of Marine Species (WoRMS).
#' Missing AphiaIDs can be resolved from scientific names automatically.
#'
#' @param aphia_ids Numeric vector of AphiaIDs.
#' @param scientific_names Optional character vector of scientific names (same length as `aphia_id`).
#' @param add_rank_to_hierarchy Logical (default FALSE). If TRUE, includes rank labels in
#'   the concatenated hierarchy string.
#' @param verbose Logical (default TRUE). If TRUE, prints progress updates.
#' @param aphia_id
#'     `r lifecycle::badge("deprecated")`
#'     Use \code{aphia_ids} instead.
#' @param scientific_name
#'     `r lifecycle::badge("deprecated")`
#'     Use \code{scientific_names} instead.
#'
#' @return A `tibble` with taxonomy columns added, including:
#' \itemize{
#'   \item `aphia_id`, `scientific_name`
#'   \item `worms_kingdom`, `worms_phylum`, `worms_class`, `worms_order`,
#'         `worms_family`, `worms_genus`, `worms_species`
#'   \item `worms_scientific_name`, `worms_hierarchy`
#' }
#'
#' @examples
#' \donttest{
#' # Using AphiaID only
#' add_worms_taxonomy(c(1080, 109604), verbose = FALSE)
#'
#' # Using a combination of AphiaID and scientific name
#' add_worms_taxonomy(
#'   aphia_ids = c(NA, 109604),
#'   scientific_names = c("Calanus finmarchicus", "Oithona similis"),
#'   verbose = FALSE
#' )
#' }
#'
#' @export
add_worms_taxonomy <- function(aphia_ids,
                               scientific_names = NULL,
                               add_rank_to_hierarchy = FALSE,
                               verbose = TRUE,
                               aphia_id = deprecated(),
                               scientific_name = deprecated()) {

  if (is_present(aphia_id)) {

    # Signal the deprecation to the user
    lifecycle::deprecate_warn("1.0.0", "SHARK4R::add_worms_taxonomy(aphia_id = )", "SHARK4R::add_worms_taxonomy(aphia_ids = )",
                              "add_worms_taxonomy() handles multiple 'aphia_id' inputs")

    # Deal with the deprecated argument for compatibility
    aphia_ids <- aphia_id
  }

  if (is_present(scientific_name)) {

    # Signal the deprecation to the user
    lifecycle::deprecate_warn("1.0.0", "SHARK4R::add_worms_taxonomy(scientific_name = )", "SHARK4R::add_worms_taxonomy(scientific_names = )",
                              "add_worms_taxonomy() handles multiple 'scientific_name' inputs")

    # Deal with the deprecated argument for compatibility
    scientific_names <- scientific_name
  }

  # --- Input validation ---
  if (!is.null(scientific_names) && length(aphia_ids) != length(scientific_names)) {
    stop("'scientific_names' and 'aphia_ids' must have the same length.")
  }

  # --- Resolve missing AphiaIDs from scientific names ---
  if (!is.null(scientific_names)) {
    aphia_id_df <- data.frame(aphia_ids, scientific_names, stringsAsFactors = FALSE)
    to_match <- scientific_names[is.na(aphia_ids)]

    if (length(to_match) > 0) {
      if (verbose) cat("Resolving", length(unique(to_match)), "missing AphiaIDs from scientific names...\n")

      worms_records <- match_worms_taxa(unique(to_match), verbose = verbose)
      name_map <- dplyr::select(worms_records, name, AphiaID)

      aphia_id_df <- aphia_id_df %>%
        dplyr::left_join(name_map, by = c("scientific_names" = "name")) %>%
        dplyr::mutate(aphia_ids = dplyr::coalesce(aphia_ids, AphiaID))
    } else if (verbose) {
      cat("All AphiaIDs provided, skipping name lookup.\n")
    }

    aphia_ids <- aphia_id_df$aphia_ids
  }

  # --- Retrieve taxonomy ---
  unique_ids <- unique(aphia_ids[!is.na(aphia_ids)])
  if (length(unique_ids) == 0) stop("No valid AphiaIDs available.")

  worms_unique <- get_worms_classification(
    aphia_ids = unique_ids,
    add_rank_to_hierarchy = add_rank_to_hierarchy,
    verbose = verbose
  )

  required_ranks <- c("Kingdom","Phylum","Class","Order","Family","Genus","Species")
  for (r in required_ranks) {
    if (!r %in% names(worms_unique)) worms_unique[[r]] <- NA_character_
  }

  worms_unique <- worms_unique %>%
    dplyr::rename(
      worms_scientific_name = scientific_name,
      worms_kingdom = Kingdom,
      worms_phylum  = Phylum,
      worms_class   = Class,
      worms_order   = Order,
      worms_family  = Family,
      worms_genus   = Genus,
      worms_species = Species
    )

  # --- Join results back to input order ---
  worms_full <- dplyr::tibble(aphia_id = aphia_ids) %>%
    dplyr::left_join(worms_unique, by = "aphia_id")

  if (!is.null(scientific_names)) {
    worms_full$scientific_name <- scientific_names
  }

  # --- Clean column order ---
  names_out <- c(
    "aphia_id",
    "scientific_name",
    names(worms_unique)[grepl("^worms_", names(worms_unique))]
  )

  worms_full %>%
    dplyr::select(dplyr::any_of(names_out)) %>%
    dplyr::relocate(worms_hierarchy, .after = dplyr::last_col())
}

#' Retrieve WoRMS records
#'
#' This function retrieves records from the WoRMS (World Register of Marine Species) database using the `worrms` R package for a given list of Aphia IDs.
#' If the retrieval fails, it retries a specified number of times before stopping.
#'
#' @param aphia_ids A vector of Aphia IDs for which records should be retrieved.
#' @param max_retries An integer specifying the maximum number of retry attempts for each Aphia ID in case of failure. Default is 3.
#' @param sleep_time A numeric value specifying the time (in seconds) to wait between retry attempts. Default is 10 seconds.
#' @param verbose A logical indicating whether to print progress messages. Default is TRUE.
#' @param aphia_id
#'     `r lifecycle::badge("deprecated")`
#'     Use \code{aphia_ids} instead.
#'
#' @return A `tibble` containing the retrieved WoRMS records for the provided Aphia IDs. Each row corresponds to one Aphia ID.
#'
#' @details The function attempts to fetch records for each Aphia ID in the provided vector. If a retrieval fails, it retries up to
#' the specified `max_retries`, with a pause of `sleep_time` seconds between attempts. If all retries fail for an Aphia ID, the function
#' stops with an error message.
#'
#' @examples
#' \donttest{
#' # Example usage with a vector of Aphia IDs
#' aphia_ids <- c(12345, 67890, 112233)
#' worms_records <- get_worms_records(aphia_ids, verbose = FALSE)
#'
#' print(worms_records)
#' }
#'
#' @seealso \url{https://marinespecies.org/} for WoRMS website.
#' @seealso \url{https://CRAN.R-project.org/package=worrms}
#'
#' @export
get_worms_records <- function(aphia_ids, max_retries = 3, sleep_time = 10, verbose = TRUE, aphia_id = deprecated()) {

  if (is_present(aphia_id)) {

    # Signal the deprecation to the user
    lifecycle::deprecate_warn("1.0.0", "SHARK4R::get_worms_records(aphia_id = )", "SHARK4R::get_worms_records(aphia_ids = )",
                              "get_worms_records() handles multiple 'aphia_id' inputs")

    # Deal with the deprecated argument for compatibility
    aphia_ids <- aphia_id
  }

  worms_records <- list()
  cache <- list()  # Cache for unique AphiaIDs
  no_content_messages <- c()

  if (verbose) {
    pb <- utils::txtProgressBar(min = 0, max = length(aphia_ids), style = 3)
  }

  for (i in seq_along(aphia_ids)) {
    id <- aphia_ids[i]
    worms_record <- NULL

    # Update progress bar
    if (verbose) utils::setTxtProgressBar(pb, i)

    # Check cache first
    if (!is.null(cache[[as.character(id)]])) {
      worms_record <- cache[[as.character(id)]]
    } else {
      attempt <- 1
      success <- FALSE

      while (attempt <= max_retries && !success) {
        tryCatch({
          worms_record <- wm_record(id)

          if (!is.null(worms_record)) {
            success <- TRUE
          }
        }, error = function(err) {
          error_message <- conditionMessage(err)

          if (grepl("204", error_message)) {
            no_content_messages <<- c(no_content_messages,
                                      paste0("No WoRMS content for AphiaID '", id, "'"))
            worms_record <<- tibble(
              AphiaID = id,
              status = "no content"
            )
            success <<- TRUE
          } else if (attempt == max_retries) {
            stop("Error retrieving WoRMS record for AphiaID ", id,
                 " after ", max_retries, " attempts: ", error_message)
          } else {
            message("Attempt ", attempt, " failed for AphiaID ", id, ": ",
                    error_message, " - Retrying...")
            Sys.sleep(sleep_time)
          }
        })

        attempt <- attempt + 1
      }

      # If still NULL after retries, insert a placeholder record
      if (is.null(worms_record)) {
        worms_record <- tibble(
          AphiaID = id,
          status = "Failed"
        )
      }

      # Store result in cache
      cache[[as.character(id)]] <- worms_record
    }

    worms_records <- bind_rows(worms_records, worms_record)
  }

  if (verbose) close(pb)

  if (verbose && length(no_content_messages) > 0) {
    cat(paste(no_content_messages, collapse = "\n"), "\n")
  }

  worms_records
}

#' Retrieve WoRMS records by taxonomic names with retry logic
#'
#' This function retrieves records from the WoRMS database using the `worrms` R package for a vector of taxonomic names.
#' It includes retry logic to handle temporary failures and ensures all names are processed. The function can query
#' all names at once using a bulk API call or iterate over names individually.
#'
#' @param taxa_names A character vector of taxonomic names for which to retrieve records.
#' @param fuzzy A logical value indicating whether to perform a fuzzy search. Default is TRUE.
#'   **Note:** Fuzzy search is only applied in iterative mode (`bulk = FALSE`) and is ignored in bulk mode.
#' @param best_match_only A logical value indicating whether to automatically select the first match and return a single match. Default is TRUE.
#' @param max_retries Integer specifying the maximum number of retries for the request in case of failure. Default is 3.
#' @param sleep_time Numeric specifying the number of seconds to wait before retrying a failed request. Default is 10.
#' @param marine_only Logical indicating whether to restrict results to marine taxa only. Default is TRUE.
#' @param bulk Logical indicating whether to perform a bulk API call for all unique names at once. Default is FALSE.
#' @param chunk_size Integer specifying the maximum number of taxa per bulk API request. Default is 500.
#'   Only used when `bulk = TRUE`. WoRMS API may reject very large requests, so chunking prevents overload.
#' @param verbose Logical indicating whether to print progress messages. Default is TRUE.
#'
#' @return A `tibble` containing the retrieved WoRMS records. Each row corresponds to a record for a taxonomic name.
#'   Repeated taxa in the input are preserved in the output.
#'
#' @details
#' - If `bulk = TRUE`, all unique names are sent to the API in a single request. Fuzzy matching is ignored.
#' - If `bulk = FALSE`, the function iterates over names individually, optionally using fuzzy matching.
#' - The function retries failed requests up to `max_retries` times, pausing for `sleep_time` seconds between attempts.
#' - Names for which no records are found will have `status = "no content"` and `AphiaID = NA`.
#' - Names are cleaned before being passed to the API call by converting them to UTF-8, replacing problematic symbols with spaces,
#' removing trailing periods, collapsing extra spaces and by trimming whitespace.
#'
#' @examples
#' \donttest{
#' # Retrieve WoRMS records iteratively for two taxonomic names
#' records <- match_worms_taxa(c("Amphidinium", "Karenia"),
#'                             max_retries = 3,
#'                             sleep_time = 5,
#'                             marine_only = TRUE,
#'                             verbose = FALSE)
#' print(records)
#'
#' # Retrieve WoRMS records in bulk mode (faster for many names)
#' records_bulk <- match_worms_taxa(c("Amphidinium", "Karenia", "Navicula"),
#'                                  bulk = TRUE,
#'                                  marine_only = TRUE,
#'                                  verbose = FALSE)
#' }
#'
#' @seealso \url{https://marinespecies.org/} for WoRMS website.
#' @seealso \url{https://CRAN.R-project.org/package=worrms}
#'
#' @export
match_worms_taxa <- function(taxa_names,
                             fuzzy = TRUE,
                             best_match_only = TRUE,
                             max_retries = 3,
                             sleep_time = 10,
                             marine_only = TRUE,
                             bulk = FALSE,
                             chunk_size = 500,
                             verbose = TRUE) {
  # Helper to rbind data.frames with different columns (like dplyr::bind_rows)
  rbind_fill <- function(dfs) {
    if (length(dfs) == 0) return(tibble())
    # keep only non-null dfs
    dfs <- dfs[!vapply(dfs, is.null, logical(1))]
    all_names <- unique(unlist(lapply(dfs, names)))
    dfs2 <- lapply(dfs, function(df) {
      missing <- setdiff(all_names, names(df))
      if (length(missing) > 0) df[missing] <- NA
      # ensure column order consistent
      df[all_names]
    })
    do.call(rbind, dfs2)
  }

  # ---------------------------
  # Build mapping table
  # ---------------------------
  unique_taxa <- unique(taxa_names)
  name_map <- data.frame(
    taxa_names = unique_taxa,
    cleaned = vapply(unique_taxa, clean_taxon, character(1)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  # unique keys to query (exclude possible _empty_ for API calls)
  unique_names_all <- unique(name_map$cleaned)
  unique_names_api <- setdiff(unique_names_all, "_empty_")

  # Prepare storage
  no_content_messages <- character(0)

  # We'll collect results as list of data.frames, then rbind_fill()
  worms_unique_list <- list()

  # ---------------------------
  # Add invalid/_empty_ entries up-front so mapping is consistent
  # ---------------------------
  if (any(name_map$cleaned == "_empty_")) {
    empties <- unique(name_map$cleaned[name_map$cleaned == "_empty_"])
    # create a single row for the _empty_ placeholder with status invalid
    worms_unique_list <- c(worms_unique_list, list(
      tibble(
        name = empties,
        status = "no content",
        AphiaID = NA_integer_,
        rank = NA_character_,
        scientificname = NA_character_
      )
    ))
  }

  # ---------------------------
  # Helper to call API with retry (single name)
  # ---------------------------
  fetch_single <- function(q, attempt_max) {
    attempt <- 1
    while (attempt <= attempt_max) {
      res <- tryCatch({
        df <- tibble(name = q,
                     worrms::wm_records_name(q, fuzzy = fuzzy, marine_only = marine_only))
        if (best_match_only && nrow(df) > 1) df <- df[1, , drop = FALSE]
        return(df)
      }, error = function(err) {
        msg <- conditionMessage(err)
        # if 204 or "no content" treat as no content
        if (grepl("204", msg) || grepl("No content", msg, ignore.case = TRUE)) {
          no_content_messages <<- c(no_content_messages, paste0("No WoRMS content for '", q, "'"))
          return(tibble(name = q,
                        status = "no content",
                        AphiaID = NA_integer_,
                        rank = NA_character_,
                        scientificname = NA_character_))
        }
        if (attempt == attempt_max) {
          stop("Error retrieving WoRMS record for '", q, "' after ", attempt_max, " attempts: ", msg)
        } else {
          Sys.sleep(sleep_time)
          attempt <<- attempt + 1
          return(NULL)
        }
      })
      # If res is NULL, loop to retry
      if (!is.null(res)) return(res)
      attempt <- attempt + 1
    }
    # fallback
    tibble(name = q, status = "no content")
  }

  # ---------------------------
  # Bulk or iterative retrieval
  # ---------------------------
  if (bulk) {
    # Split the API queries into chunks
    name_chunks <- split(unique_names_api, ceiling(seq_along(unique_names_api) / chunk_size))

    attempt <- 1
    success <- FALSE

    for (chunk_idx in seq_along(name_chunks)) {
      chunk <- name_chunks[[chunk_idx]]

      attempt <- 1
      success <- FALSE

      while (attempt <= max_retries && !success) {
        tryCatch({
          if (length(chunk) > 0) {
            raw_records <- worrms::wm_records_names(chunk, marine_only = marine_only)
          } else {
            raw_records <- list()
          }

          # Each element corresponds to one queried name
          per_name <- lapply(seq_along(chunk), function(i) {
            rec <- raw_records[[i]]
            if (is.null(rec) || length(rec) == 0) {
              tibble(name = chunk[i],
                     status = "no content",
                     AphiaID = NA_integer_,
                     rank = NA_character_,
                     scientificname = NA_character_)
            } else {
              if (is.data.frame(rec)) {
                if (best_match_only && nrow(rec) > 1) rec <- rec[1, , drop = FALSE]
                rec2 <- rec
                rec2$name <- chunk[i]
                rec2 <- rec2[c("name", setdiff(names(rec2), "name"))]
                rec2
              } else {
                tibble(name = chunk[i], rec)
              }
            }
          })

          worms_unique_list <- c(worms_unique_list, per_name)
          success <- TRUE

          if (verbose) message(sprintf("Processed chunk %d/%d (%d taxa)", chunk_idx, length(name_chunks), length(chunk)))

        }, error = function(err) {
          msg <- conditionMessage(err)
          if (grepl("204", msg) || grepl("No content", msg, ignore.case = TRUE)) {
            no_content_messages <<- c(no_content_messages, sprintf("No WoRMS content in chunk %d.", chunk_idx))
            fallback <- lapply(chunk, function(nm) {
              tibble(name = nm,
                     status = "no content",
                     AphiaID = NA_integer_,
                     rank = NA_character_,
                     scientificname = NA_character_)
            })
            worms_unique_list <<- c(worms_unique_list, fallback)
            success <<- TRUE
          } else if (attempt == max_retries) {
            stop("Error retrieving WoRMS records for chunk ", chunk_idx,
                 " after ", max_retries, " attempts: ", msg)
          } else {
            Sys.sleep(sleep_time)
          }
        })
        attempt <- attempt + 1
      }
    }
  } else {
    # iterative calls for each cleaned name (excluding _empty_)
    names_to_query <- unique_names_api
    if (verbose && length(names_to_query) > 0) pb <- utils::txtProgressBar(min = 0, max = length(names_to_query), style = 3)
    for (i in seq_along(names_to_query)) {
      q <- names_to_query[i]
      if (verbose) utils::setTxtProgressBar(pb, i)
      # fetch with retries
      worms_res <- fetch_single(q, max_retries)
      worms_unique_list <- c(worms_unique_list, list(worms_res))
    }
    if (verbose && length(names_to_query) > 0) close(pb)
  }

  # ---------------------------
  # Combine into one data.frame
  # ---------------------------
  worms_unique <- rbind_fill(worms_unique_list)

  # ensure name column exists (should)
  if (!"name" %in% names(worms_unique)) {
    worms_unique$name <- name_map$cleaned[1]  # fallback
  }

  # ---------------------------
  # Ensure consistent columns for mapping-back stage
  # ---------------------------
  # Guarantee some typical columns exist so mapping back creates consistent rows:
  expected_cols <- c("name", "status", "AphiaID", "rank", "scientificname")
  missing_cols <- setdiff(expected_cols, names(worms_unique))
  if (length(missing_cols) > 0) {
    worms_unique[missing_cols] <- NA
  }
  # Reorder columns to have name first
  worms_unique <- worms_unique[c("name", setdiff(names(worms_unique), "name"))]

  # ---------------------------
  # Map back to original taxa_names (preserve duplicates and order)
  # ---------------------------
  worms_records_list <- lapply(taxa_names, function(orig) {
    cleaned_x <- name_map$cleaned[name_map$taxa_names == orig]
    # cleaned_x should be length 1 (since name_map built from unique_taxa)
    if (length(cleaned_x) != 1) cleaned_x <- cleaned_x[1]
    rows <- worms_unique[worms_unique$name == cleaned_x, , drop = FALSE]

    if (nrow(rows) == 0) {
      # create a consistent "no result" row
      tibble(
        name = cleaned_x,
        status = "no content",
        AphiaID = NA_integer_,
        rank = NA_character_,
        scientificname = NA_character_
      )
    } else {
      # If the API returned multiple possible rows for the cleaned name,
      # prefer the first (this matches previous best_match_only handling)
      rows[1, , drop = FALSE]
    }
  })

  worms_records <- rbind_fill(worms_records_list)

  # Map the original taxa_names to 'name' column directly
  worms_records$name <- taxa_names

  # Reorder columns to have 'name' first (optional)
  worms_records <- worms_records[c("name", setdiff(names(worms_records), "name"))]

  worms_records
}

#' Retrieve WoRMS records by taxonomic names with retry logic
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function has been deprecated. Users are encouraged to use \code{\link{match_worms_taxa}} instead.
#'
#' This function retrieves records from the WoRMS database using the `worrms` R package for a vector of taxonomic names.
#' It includes retry logic to handle temporary failures and ensures all names are processed.
#'
#' @param taxa_names A vector of taxonomic names for which to retrieve records.
#' @param fuzzy A logical value indicating whether to search using a fuzzy search pattern. Default is TRUE.
#' @param best_match_only A logical value indicating whether to automatically select the first match and return a single match. Default is TRUE.
#' @param max_retries An integer specifying the maximum number of retries for the request in case of failure. Default is 3.
#' @param sleep_time A numeric value specifying the number of seconds to wait before retrying a failed request. Default is 10.
#' @param marine_only A logical value indicating whether to restrict the results to marine taxa only. Default is `FALSE`.
#' @param verbose A logical indicating whether to print progress messages. Default is TRUE.
#'
#' @return A `tibble` containing the retrieved WoRMS records. Each row corresponds to a record for a taxonomic name.
#'
#' @details
#' The function attempts to retrieve records for the input taxonomic names using the `wm_records_names` function from the WoRMS API.
#' If a request fails, it retries up to `max_retries` times, pausing for `sleep_time` seconds between attempts.
#' If all attempts fail, the function stops and throws an error.
#'
#' @examples
#' \donttest{
#' # Retrieve WoRMS records for the taxonomic names "Amphidinium" and "Karenia"
#' records <- get_worms_records_name(c("Amphidinium", "Karenia"),
#'                                   max_retries = 3, sleep_time = 5, marine_only = TRUE)
#' }
#'
#' @seealso \url{https://marinespecies.org/} for WoRMS website.
#' @seealso \url{https://CRAN.R-project.org/package=worrms}
#'
#' @keywords internal
#' @export
get_worms_records_name <- function(taxa_names, fuzzy = TRUE, best_match_only = TRUE,
                                   max_retries = 3, sleep_time = 10, marine_only = TRUE,
                                   verbose = TRUE) {

  lifecycle::deprecate_warn("1.0.0", "get_worms_records_name()", "match_worms_taxa()")

  match_worms_taxa(taxa_names = taxa_names,
                   fuzzy = fuzzy,
                   best_match_only = best_match_only,
                   max_retries = max_retries,
                   sleep_time = sleep_time,
                   marine_only = marine_only,
                   verbose = verbose)
}

#' Assign phytoplankton group to scientific names
#'
#' This function assigns default phytoplankton groups (Diatoms, Dinoflagellates, Cyanobacteria, or Other)
#' to a list of scientific names or Aphia IDs by retrieving species information from the
#' World Register of Marine Species (WoRMS). The function checks both Aphia IDs and scientific names,
#' handles missing records, and assigns the appropriate plankton group based on taxonomic classification in WoRMS.
#' Additionally, custom plankton groups can be specified using the `custom_groups` parameter,
#' allowing users to define additional classifications based on specific taxonomic criteria.
#'
#' @param scientific_names A character vector of scientific names of marine species.
#' @param aphia_ids A numeric vector of Aphia IDs corresponding to the scientific names. If provided, it improves the accuracy and speed of the matching process. The length of `aphia_ids` must match the length of `scientific_names`. Defaults to `NULL`, in which case the function will attempt to assign plankton groups based only on the scientific names.
#' @param diatom_class A character string or vector representing the diatom class. Default is "Bacillariophyceae", "Coscinodiscophyceae", "Mediophyceae" and "Diatomophyceae".
#' @param dinoflagellate_class A character string or vector representing the dinoflagellate class. Default is "Dinophyceae".
#' @param cyanobacteria_class A character string or vector representing the cyanobacteria class. Default is "Cyanophyceae".
#' @param cyanobacteria_phylum A character string or vector representing the cyanobacteria phylum. Default is "Cyanobacteria".
#' @param match_first_word A logical value indicating whether to match the first word of the scientific name if the Aphia ID is missing. Default is TRUE.
#' @param marine_only A logical value indicating whether to restrict the results to marine taxa only. Default is `FALSE`.
#' @param return_class A logical value indicating whether to include class information in the result. Default is `FALSE`.
#' @param custom_groups A named list of additional custom plankton groups (optional). The names of the list correspond to the custom group names (e.g., "Cryptophytes"), and the values should be character vectors specifying one or more of the following taxonomic levels: `phylum`, `class`, `order`, `family`, `genus`, or `scientific_name`. For example:
#'   \code{list("Green Algae" = list(class = c("Chlorophyceae", "Ulvophyceae")))}.
#'   This allows users to extend the default classifications (e.g., Cyanobacteria, Diatoms, Dinoflagellates) with their own groups.
#' @param verbose A logical value indicating whether to print progress messages. Default is TRUE.
#'
#' @return A `tibble` with two columns: `scientific_name` and `plankton_group`, where the plankton group is assigned based on taxonomic classification.
#'
#' @details The `aphia_ids` parameter is not necessary but, if provided, will improve the certainty of the
#'   matching process. If `aphia_ids` are available, they will be used directly to retrieve more accurate
#'   WoRMS records. If missing, the function will attempt to match the scientific names to Aphia IDs by
#'   querying WoRMS using the scientific name(s), with an additional fallback mechanism to match based on the
#'   first word of the scientific name.
#'
#'   To skip one of the default plankton groups, you can set the class or phylum of the respective group to an empty string (`""`).
#'   For example, to skip the "Cyanobacteria" group, you can set `cyanobacteria_class = ""` or `cyanobacteria_phylum = ""`. These
#'   taxa will then be placed in `Others`.
#'
#'   Custom groups are processed in the order they appear in the `custom_groups` list. If a taxon matches
#'   multiple custom groups, it will be assigned to the group that appears last in the list, as later matches
#'   overwrite earlier ones. For example, if `Teleaulax amphioxeia` matches both `Cryptophytes` (class-based)
#'   and a specific group `Teleaulax` (name-based), it will be assigned to `Teleaulax` if `Teleaulax` is listed after
#'   `Cryptophytes` in the `custom_groups` list.
#'
#' @examples
#' \donttest{
#' # Assign plankton groups to a list of species names
#' result <- assign_phytoplankton_group(
#'   scientific_names = c("Tripos fusus", "Diatoma", "Nodularia spumigena", "Octactis speculum"),
#'   verbose = FALSE)
#'
#' print(result)
#'
#' # Improve classification by explicitly providing Aphia IDs for ambiguous taxa
#' # Actinocyclus and Navicula are names shared by both diatoms and animals,
#' # which can lead to incorrect group assignment without an Aphia ID
#' result <- assign_phytoplankton_group(
#'   scientific_names = c("Actinocyclus", "Navicula", "Nodularia spumigena", "Tripos fusus"),
#'   aphia_ids = c(148944, 149142, NA, NA),
#'   verbose = FALSE)
#'
#' print(result)
#'
#' # Assign plankton groups using additional custom grouping
#' custom_groups <- list(
#'     Cryptophytes = list(class = "Cryptophyceae"),
#'     Ciliates = list(phylum = "Ciliophora")
#' )
#'
#' # Assign with custom groups
#' result_custom <- assign_phytoplankton_group(
#'   scientific_names = c("Teleaulax amphioxeia", "Mesodinium rubrum", "Dinophysis acuta"),
#'   aphia_ids = c(106306, 232069, 109604),
#'   custom_groups = custom_groups,         # Adding custom groups
#'   verbose = FALSE
#' )
#'
#' print(result_custom)
#' }
#'
#' @seealso \url{https://marinespecies.org/} for WoRMS website.
#' @seealso \url{https://CRAN.R-project.org/package=worrms}
#'
#' @export
assign_phytoplankton_group <- function(scientific_names, aphia_ids = NULL,
                                       diatom_class = c("Bacillariophyceae", "Coscinodiscophyceae", "Mediophyceae", "Diatomophyceae"),
                                       dinoflagellate_class = "Dinophyceae", cyanobacteria_class = "Cyanophyceae",
                                       cyanobacteria_phylum = "Cyanobacteria", match_first_word = TRUE,
                                       marine_only = FALSE, return_class = FALSE, custom_groups = list(),
                                       verbose = TRUE) {
  # Ensure input lengths match
  if (!length(aphia_ids) == length(scientific_names) & !is.null(aphia_ids)) {
    stop("'aphia_ids' and 'scientific_names' must have the same length.")
  }

  # Create a data frame to store input data
  if (is.null(aphia_ids)) {
    input_data <- tibble(aphia_id = NA_integer_, scientific_name = scientific_names)
  } else {
    input_data <- tibble(aphia_id = aphia_ids, scientific_name = scientific_names)
  }

  # Remove duplicates
  unique_data <- distinct(input_data)

  # Filter rows that have aphia_id
  unique_data <- unique_data %>%
    group_by(scientific_name) %>%
    filter(!is.na(aphia_id) | n() == 1) %>%
    ungroup()

  # Extract unique non-NA AphiaIDs
  valid_aphia_ids <- unique_data$aphia_id[!is.na(unique_data$aphia_id)]

  # Retrieve WoRMS records based on AphiaID
  if (length(valid_aphia_ids) > 0) {
    if (verbose) cat("Retrieving", length(valid_aphia_ids), "WoRMS records from input 'aphia_ids'.\n")
    aphia_records <- get_worms_records(valid_aphia_ids, verbose = verbose) %>%
      mutate(class = as.character(ifelse(status == "no content", NA, class)),
             phylum = as.character(ifelse(status == "no content", NA, phylum)))
  } else {
    aphia_records <- tibble(AphiaID = NA_integer_,
                            status = NA_character_,
                            class = NA_character_,
                            phylum = NA_character_)
  }

  # List IDs without content
  no_content <- aphia_records %>%
    filter(status == "no content")

  # List IDs with content
  aphia_records <- aphia_records %>%
    filter(!status == "no content")

  # Merge records into input data
  matched_aphia_data <- unique_data %>%
    left_join(distinct(aphia_records), by = c("aphia_id" = "AphiaID")) %>%
    filter(!is.na(aphia_id) & !is.na(status))

  # Handle entries with missing AphiaIDs
  missing_aphia_data <- unique_data[is.na(unique_data$aphia_id) | unique_data$aphia_id %in% no_content$AphiaID,]

  if (nrow(missing_aphia_data) > 0) {
    if (verbose) cat("Retrieving", nrow(missing_aphia_data), "WoRMS records from input 'scientific_names'.\n")
    missing_aphia_records <- match_worms_taxa(missing_aphia_data$scientific_name,
                                              marine_only = marine_only,
                                              verbose = verbose) %>%
      mutate(class = as.character(ifelse(status == "no content", NA, class)),
             phylum = as.character(ifelse(status == "no content", NA, phylum)))
  } else {
    missing_aphia_records <- tibble(name = NA_character_,
                                    AphiaID = NA_integer_,
                                    class = NA_character_,
                                    phylum = NA_character_)
  }

  # Merge records into input data
  matched_missing_data <- missing_aphia_data %>%
    left_join(missing_aphia_records, by = c("scientific_name" = "name")) %>%
    mutate(aphia_id = AphiaID)

  combined_data <- bind_rows(matched_aphia_data, matched_missing_data)

  # Handle still-missing entries
  unresolved_data <- combined_data %>%
    filter(is.na(aphia_id))

  if (match_first_word & nrow(unresolved_data) > 0) {
    first_words <- sub(" .*", "", unresolved_data$scientific_name)
    if (verbose) cat("Retrieving", length(first_words), "WoRMS records from the first word in 'scientific_names'.\n")
    first_word_records <- tibble(
      scientific_name = unresolved_data$scientific_name,
      match_worms_taxa(first_words,
                       marine_only = marine_only,
                       verbose = verbose)
    ) %>%
      filter(!is.na(AphiaID)) %>%
      mutate(aphia_id = AphiaID,
             rank = as.character(ifelse(status == "no content", NA, rank)),
             scientificname = as.character(ifelse(status == "no content", NA, scientificname)))

    combined_data <- combined_data %>%
      filter(!scientific_name %in% first_word_records$scientific_name) %>%
      bind_rows(first_word_records)
  }

  # Handle deleted records
  deleted_records <- combined_data %>%
    filter(status == "deleted")

  if (nrow(deleted_records) > 0) {
    if (verbose) cat("Retrieving", nrow(deleted_records), "valid 'aphia_id' records from deleted entries.\n")
    valid_deleted_records <- data.frame(
      aphia_id = deleted_records$valid_AphiaID,
      scientific_name = deleted_records$scientific_name,
      get_worms_records(deleted_records$valid_AphiaID, verbose = verbose)
    )
  } else {
    valid_deleted_records <- data.frame()
  }

  combined_data <- combined_data %>%
    filter(!scientific_name %in% valid_deleted_records$scientific_name) %>%
    bind_rows(valid_deleted_records)

  # Combine all classes
  classes <- unique(combined_data$class)
  other_classes <- classes[!classes %in% c(diatom_class, dinoflagellate_class, cyanobacteria_class)]

  # Assign plankton groups
  class_mapping <- combined_data %>%
    select(aphia_id, scientific_name, phylum, class, order, family, genus) %>%
    mutate(plankton_group = case_when(
      class %in% diatom_class ~ "Diatoms",
      class %in% dinoflagellate_class ~ "Dinoflagellates",
      class %in% cyanobacteria_class ~ "Cyanobacteria",
      phylum %in% cyanobacteria_phylum ~ "Cyanobacteria",
      TRUE ~ NA_character_
    ))

  # Apply custom groups
  for (group_name in names(custom_groups)) {
    group_criteria <- custom_groups[[group_name]]
    if (is.null(group_criteria$phylum)) group_criteria$phylum <- character(0)
    if (is.null(group_criteria$class)) group_criteria$class <- character(0)
    if (is.null(group_criteria$order)) group_criteria$order <- character(0)
    if (is.null(group_criteria$family)) group_criteria$family <- character(0)
    if (is.null(group_criteria$genus)) group_criteria$genus <- character(0)
    if (is.null(group_criteria$scientific_name)) group_criteria$scientific_name <- character(0)

    class_mapping <- class_mapping %>%
      mutate(plankton_group = case_when(
        phylum %in% group_criteria$phylum |
          class %in% group_criteria$class |
          order %in% group_criteria$order |
          family %in% group_criteria$family |  # New condition
          genus %in% group_criteria$genus |
          scientific_name %in% group_criteria$scientific_name ~ group_name,
        TRUE ~ plankton_group
      ))
  }

  # Handle unknown classifications
  class_mapping <- class_mapping %>%
    mutate(plankton_group = ifelse(is.na(plankton_group), "Other", plankton_group))

  if (return_class) {
    class_mapping <- class_mapping %>%
      select(scientific_name, class, plankton_group) %>%
      distinct()
  } else {
    class_mapping <- class_mapping %>%
      select(scientific_name, plankton_group) %>%
      distinct()
  }

  # Finalize output
  final_output <- input_data %>%
    left_join(class_mapping, by = "scientific_name") %>%
    select(-aphia_id)

  return(final_output)
}

#' Taxon matching using WoRMS (http://www.marinespecies.org/)
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function has been deprecated. Users are encouraged to use \code{\link{match_worms_taxa}} instead.
#'
#' matches latin name in data with WoRMS taxon list
#' @param names Vector of scientific names.
#' @param ask Ask user in case of multiple matches.
#' @return Data frame with scientific name, scientific name ID and match type.
#' @references Provoost P, Bosch S (2025). obistools: Tools for data enhancement and quality control. Ocean Biodiversity Information System. Intergovernmental Oceanographic Commission of UNESCO. R package version 0.1.0, <https://iobis.github.io/obistools/>.
#' @keywords internal
#' @export
match_wormstaxa <- function(names, ask = TRUE) {
  lifecycle::deprecate_warn("1.0.0", "match_wormstaxa()", "match_worms_taxa()")

  match_worms_taxa(taxa_names = names, best_match_only = ask)
}

#' Retrieve hierarchical taxonomy data from WoRMS
#'
#' Retrieves the hierarchical taxonomy for one or more AphiaIDs from the
#' World Register of Marine Species (WoRMS). Optionally, the function can
#' include all descendants of taxa at a specified rank and/or synonyms for
#' all retrieved taxa.
#'
#' @param aphia_ids Numeric vector of AphiaIDs to retrieve taxonomy for. Must
#'   not be missing or all NA.
#' @param add_descendants Logical (default FALSE). If TRUE, retrieves all
#'   child taxa for each taxon at the rank specified by `add_descendants_rank`.
#' @param add_descendants_rank Character (default `"Species"`). The taxonomic
#'   rank of descendants to retrieve. For example, if set to `"Species"`, the
#'   function will collect all species belonging to each genus present in the
#'   initial dataset.
#' @param add_synonyms Logical (default FALSE). If TRUE, retrieves synonym
#'   records for all retrieved taxa and appends them to the dataset.
#' @param add_hierarchy Logical (default FALSE). If TRUE, adds a `hierarchy`
#'   column that contains the concatenated hierarchy of each taxon (e.g. Kingdom - Phylum - Class).
#' @param add_rank_to_hierarchy Logical (default FALSE). If TRUE, the hierarchy
#'   string prepends rank names (e.g., `[Kingdom] Animalia - [Phylum] Chordata`)
#'   to each taxon name in the `hierarchy` column. Only used if `add_hierarchy = TRUE`.
#' @param verbose Logical (default TRUE). If TRUE, prints progress messages
#'   and progress bars during data retrieval.
#'
#' @return A `tibble` containing detailed WoRMS records for all requested
#'   AphiaIDs, including optional descendants and synonyms. Typical columns
#'   include:
#'   \describe{
#'     \item{AphiaID}{The AphiaID of the taxon.}
#'     \item{parentNameUsageID}{The AphiaID of the parent taxon.}
#'     \item{scientificname}{Scientific name of the taxon.}
#'     \item{rank}{Taxonomic rank (e.g., Kingdom, Phylum, Genus, Species).}
#'     \item{status}{Taxonomic status (e.g., accepted, unaccepted).}
#'     \item{valid_AphiaID}{AphiaID of the accepted taxon, if the record is a synonym.}
#'     \item{species}{Added only if a `Species` rank exists in the retrieved
#'       data and if `add_hierarchy = TRUE`; otherwise not present.}
#'     \item{parentName}{Added only if a `parentName` rank exists in the retrieved
#'       data and if `add_hierarchy = TRUE`; otherwise not present.}
#'     \item{hierarchy}{Added only if `add_hierarchy = TRUE` and hierarchy
#'       data are available. Contains a concatenated string of the taxonomic
#'       path.}
#'     \item{...}{Additional columns returned by WoRMS, including authorship
#'       and source information.}
#'   }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Validates input AphiaIDs and removes NA values.
#'   \item Retrieves the hierarchical classification for each AphiaID using
#'         `worrms::wm_classification()`.
#'   \item Optionally retrieves all descendants at the rank specified by
#'         `add_descendants_rank` if `add_descendants = TRUE`.
#'   \item Optionally retrieves synonyms for all retrieved taxa if
#'         `add_synonyms = TRUE`.
#'   \item Optionally adds a `hierarchy` column if `add_hierarchy = TRUE`.
#'   \item Returns a combined, distinct dataset of all records.
#' }
#'
#' @examples
#' \donttest{
#' # Retrieve hierarchy for a single AphiaID
#' get_worms_taxonomy_tree(aphia_ids = 109604, verbose = FALSE)
#'
#' # Retrieve hierarchy including species-level descendants
#' get_worms_taxonomy_tree(
#'   aphia_ids = c(109604, 376667),
#'   add_descendants = TRUE,
#'   verbose = FALSE
#' )
#'
#' # Retrieve hierarchy including hierarchy column
#' get_worms_taxonomy_tree(
#'   aphia_ids = c(109604, 376667),
#'   add_hierarchy = TRUE,
#'   verbose = FALSE
#' )
#' }
#'
#' @seealso \code{\link{add_worms_taxonomy}}, \code{\link{construct_dyntaxa_table}}
#' @seealso \code{\link[worrms]{wm_classification}}, \code{\link[worrms]{wm_children}}, \code{\link[worrms]{wm_synonyms}}
#' @seealso \url{https://marinespecies.org/} for the WoRMS website.
#'
#' @export
get_worms_taxonomy_tree <- function(aphia_ids,
                                    add_descendants = FALSE,
                                    add_descendants_rank = "Species",
                                    add_synonyms = FALSE,
                                    add_hierarchy = FALSE,
                                    add_rank_to_hierarchy = FALSE,
                                    verbose = TRUE) {
  # --- Input validation ---
  if (missing(aphia_ids) || all(is.na(aphia_ids))) {
    stop("No valid 'aphia_ids' provided.")
  }

  unique_ids <- unique(stats::na.omit(aphia_ids))
  if (length(unique_ids) == 0) {
    stop("All 'aphia_ids' values are NA.")
  }

  if (verbose) {
    cat("Retrieving higher taxonomy for", length(unique_ids), "unique AphiaIDs...\n")
    pb1 <- utils::txtProgressBar(min = 0, max = length(unique_ids), style = 3)
  }

  # --- Retrieve hierarchical data from WoRMS ---
  worms_list <- vector("list", length(unique_ids))

  for (i in seq_along(unique_ids)) {
    if (verbose) utils::setTxtProgressBar(pb1, i)
    id <- unique_ids[i]

    worms_list[[i]] <- tryCatch({
      hierarchy <- worrms::wm_classification(id)

      if (is.null(hierarchy) || nrow(hierarchy) == 0) {
        hierarchy <- tibble(AphiaID = id, parentNameUsageID = NA)
      } else {
        hierarchy$parentNameUsageID <- c(NA, utils::head(hierarchy$AphiaID, -1))
      }

      hierarchy
    }, error = function(e) {
      tibble(AphiaID = id, parentNameUsageID = NA)
    })
  }

  if (verbose) close(pb1)

  worms_unique <- dplyr::bind_rows(worms_list) %>%
    dplyr::distinct()

  if (!"AphiaID" %in% names(worms_unique)) {
    stop("No valid WoRMS records retrieved.")
  }

  if (verbose) {
    n_ids <- length(unique(worms_unique$AphiaID))
    cat("Retrieving records for", n_ids, "unique AphiaIDs...\n")
  }

  # --- Retrieve detailed records for all AphiaIDs ---
  worms_records <- get_worms_records(unique(worms_unique$AphiaID), verbose = verbose)

  # --- Optionally retrieve genus children ---
  if (add_descendants) {
    worms_species <- dplyr::filter(worms_records, rank == add_descendants_rank)
    unique_genera <- unique(stats::na.omit(worms_species$parentNameUsageID))

    if (length(unique_genera) > 0) {
      if (verbose) {
        cat("Retrieving child records for", length(unique_genera), "unique genera...\n")
        pb2 <- utils::txtProgressBar(min = 0, max = length(unique_genera), style = 3)
      }

      children_list <- tibble()

      for (i in seq_along(unique_genera)) {
        genus_id <- unique_genera[i]

        if (verbose) utils::setTxtProgressBar(pb2, i)

        children <- tryCatch({
          worrms::wm_children(genus_id)
        }, error = function(e) {
          NULL
        })

        if (!is.null(children) && nrow(children) > 0) {
          children_list <- dplyr::bind_rows(children_list, children)
        }
      }

      if (verbose) close(pb2)

      # Bind with main data frame
      if (nrow(children_list) > 0) {
        worms_records <- dplyr::bind_rows(worms_records, children_list) %>%
          dplyr::distinct()
      }
    }
  }

  if (add_synonyms) {
    aphia_ids_to_use <- unique(worms_records$AphiaID)

    if (verbose) {
      cat("Retrieving synonym records for", length(aphia_ids_to_use), "unique taxa...\n")
      pb3 <- utils::txtProgressBar(min = 0, max = length(aphia_ids_to_use), style = 3)
    }

    synonyms <- tibble()

    for (i in seq_along(aphia_ids_to_use)) {
      id <- aphia_ids_to_use[i]

      if (verbose) utils::setTxtProgressBar(pb3, i)

      synonym <- tryCatch({
        worrms::wm_synonyms(id)
      }, error = function(e) {
        NULL  # skip on error
      })

      if (!is.null(synonym) && nrow(synonym) > 0) {
        synonyms <- bind_rows(synonyms, synonym)
      }
    }

    # Bind with main data frame
    worms_records <- bind_rows(worms_records, synonyms) %>%
      dplyr::distinct()

    if (verbose) close(pb3)
  }

  # --- Add hierarchy ---
  if (add_hierarchy) {
    hierarchy <- get_worms_classification(aphia_ids = worms_records$AphiaID,
                                          add_rank_to_hierarchy = add_rank_to_hierarchy,
                                          verbose = verbose)

    # Add new hierarchy column
    if (is.data.frame(hierarchy) && "worms_hierarchy" %in% names(hierarchy)) {
      worms_records$hierarchy <- hierarchy$worms_hierarchy
    }

    # Add species column
    if (is.data.frame(hierarchy) && "Species" %in% names(hierarchy)) {
      worms_records$species <- hierarchy$Species

      worms_records <- worms_records %>%
        dplyr::relocate(species, .after = genus)
    }

    # Add parentName column
    if (is.data.frame(hierarchy) && "parent_name" %in% names(hierarchy)) {
      worms_records$parentName <- hierarchy$parent_name

      worms_records <- worms_records %>%
        dplyr::relocate(parentName, .after = parentNameUsageID)
    }

    # Add root parentName
    if (1 %in% unique(worms_records$parentNameUsageID)) {
      root_records <- get_worms_records(1, verbose = FALSE)

      if (nrow(root_records) > 0) {
        worms_records <- worms_records %>%
          mutate(parentName = dplyr::if_else(parentNameUsageID == 1,
                                             root_records$scientificname[1],
                                             parentName))
      }
    }
  }

  # --- Return final dataset ---
  worms_records
}

#' Retrieve hierarchical classification from WoRMS
#'
#' Retrieves the hierarchical taxonomy for one or more AphiaIDs from the
#' World Register of Marine Species (WoRMS) and returns it in a wide format.
#' Optionally, a hierarchy string column can be added that concatenates ranks.
#'
#' @param aphia_ids Numeric vector of AphiaIDs to retrieve classification for.
#'   Must not be NULL or empty. Duplicates are allowed and will be preserved
#'   in the output.
#' @param add_rank_to_hierarchy Logical (default FALSE). If TRUE, the hierarchy
#'   string prepends rank names (e.g., `[Kingdom] Animalia - [Phylum] Chordata`)
#'   to each taxon name in the `worms_hierarchy` column. Only applies if
#'   `worms_hierarchy` is present.
#' @param verbose Logical (default TRUE). If TRUE, prints progress messages
#'   and a progress bar during data retrieval.
#'
#' @return A `tibble` where each row corresponds to an input AphiaID. Typical
#'   columns include:
#'   \describe{
#'     \item{aphia_id}{The AphiaID of the taxon (matches input).}
#'     \item{scientific_name}{The last scientific name in the hierarchy for
#'       this AphiaID.}
#'     \item{taxonomic ranks}{Columns for each rank present in the WoRMS
#'       hierarchy (e.g., Kingdom, Phylum, Class, Order, Family, Genus,
#'       Species). Missing ranks are NA.}
#'     \item{worms_hierarchy}{A concatenated string of all ranks for this
#'       AphiaID. Added for every row if `wm_classification()` returned
#'       hierarchy data. Format depends on `add_rank_to_hierarchy`.}
#'   }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Validates input AphiaIDs and removes NA values.
#'   \item Retrieves the hierarchical classification for each AphiaID using
#'         `worrms::wm_classification()`.
#'   \item Converts the hierarchy to a wide format with one column per rank.
#'   \item Adds a `worms_hierarchy` string concatenating all ranks.
#'   \item Preserves input order and duplicates.
#' }
#'
#' @examples
#' \donttest{
#' # Single AphiaID
#' single_taxa <- get_worms_classification(109604, verbose = FALSE)
#' print(single_taxa)
#'
#' # Multiple AphiaIDs
#' multiple_taxa <- get_worms_classification(c(109604, 376667), verbose = FALSE)
#' print(multiple_taxa)
#'
#' # Hierarchy with ranks in the string
#' with_rank <- get_worms_classification(c(109604, 376667),
#'                                       add_rank_to_hierarchy = TRUE,
#'                                       verbose = FALSE)
#'
#' # Print hierarchy columns with ranks
#' print(with_rank$worms_hierarchy[1])
#'
#' # Compare with result when add_rank_to_hierarchy = FALSE
#' print(multiple_taxa$worms_hierarchy[1])
#' }
#'
#' @seealso \code{\link[worrms]{wm_classification}}, \url{https://marinespecies.org/}
#' @export
get_worms_classification <- function(aphia_ids,
                                     add_rank_to_hierarchy = FALSE,
                                     verbose = TRUE) {

  # --- Input validation ---
  if (is.null(aphia_ids) || length(aphia_ids) == 0) {
    stop("'aphia_ids' cannot be NULL or empty.")
  }

  input_ids <- aphia_ids  # keep original input for matching
  aphia_ids <- aphia_ids[!is.na(aphia_ids)]

  if (verbose) {
    cat("Retrieving WoRMS classification for", length(aphia_ids), "AphiaIDs.\n")
    pb <- utils::txtProgressBar(min = 0, max = length(aphia_ids), style = 3)
  }

  worms_list <- vector("list", length(aphia_ids))

  # --- Retrieve classification for each AphiaID ---
  for (i in seq_along(aphia_ids)) {
    if (verbose) utils::setTxtProgressBar(pb, i)
    id <- aphia_ids[i]

    worms_list[[i]] <- tryCatch({
      hierarchy <- worrms::wm_classification(id)

      if (is.null(hierarchy) || nrow(hierarchy) == 0) {
        tibble(aphia_id = id)
      } else {
        hierarchy %>%
          dplyr::select(-AphiaID) %>%
          dplyr::mutate(
            scientific_name = dplyr::last(scientificname),
            parent_name = if (nrow(.) > 1) scientificname[nrow(.) - 1] else NA_character_
          ) %>%
          tidyr::pivot_wider(
            names_from = rank,
            values_from = scientificname,
            values_fn = \(x) paste(unique(x), collapse = " / "),
            values_fill = NA
          ) %>%
          dplyr::mutate(
            worms_hierarchy = purrr::pmap_chr(
              dplyr::select(., -scientific_name, -parent_name),
              function(...) {
                x <- list(...)
                x <- x[!is.na(x)]
                if (add_rank_to_hierarchy) {
                  paste0("[", names(x), "] ", x) |> paste(collapse = " - ")
                } else {
                  paste(unlist(x), collapse = " - ")
                }
              }
            ),
            aphia_id = id
          )
      }

    }, error = function(e) {
      if (verbose) message("Failed to retrieve classification for AphiaID ", id, ": ", e$message)
      tibble(aphia_id = id)
    })
  }

  if (verbose) close(pb)

  # --- Combine all results ---
  df_all <- dplyr::bind_rows(worms_list)

  # --- Match output to input order, including duplicates ---
  df_all <- df_all[match(input_ids, df_all$aphia_id), ]

  dplyr::relocate(df_all, worms_hierarchy, .after = dplyr::last_col()) %>%
    dplyr::relocate(aphia_id)
}
