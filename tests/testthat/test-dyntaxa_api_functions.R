# Retrieve the API key
dyntaxa_key <- Sys.getenv("DYNTAXA_KEY")

test_id <- 238460  # Dinophysis acuta
test_genus_id <- 1010631  # Dinophysis
test_complex_id <- 235007 # Bonnemaisonia hamifera/Spermothamnion repens, not in Taxon.csv
test_species_name <- "Dinophysis acuta"

dyntaxa_url <- "https://artfakta.se/"

test_that("records are retrieved", {
  skip_if_offline()
  skip_if_resource_unavailable(dyntaxa_url)

  dyntaxa_record <- get_dyntaxa_records(test_id, dyntaxa_key)

  expect_s3_class(dyntaxa_record, "data.frame")
  expect_true(nrow(dyntaxa_record) > 0)
  expect_true(all(c("taxonId", "parentId", "secondaryParents", "sortOrder", "statusReason", "isMicrospecies", "externalComment", "redlistCategory", "excludeFromReportingSystem", "nrOfChilds", "names") %in% names(dyntaxa_record)))
})

test_that("parent ids are retrieved", {
  skip_if_offline()
  skip_if_resource_unavailable(dyntaxa_url)

  dyntaxa_parents <- get_dyntaxa_parent_ids(test_id, dyntaxa_key)

  expect_equal(class(dyntaxa_parents), "list")
  expect_true(length(dyntaxa_parents) > 0)
})

test_that("children hierarchy is retrieved", {
  skip_if_offline()
  skip_if_resource_unavailable(dyntaxa_url)

  dyntaxa_children_hierarchy <- SHARK4R:::get_dyntaxa_children_hierarchy(test_genus_id, dyntaxa_key)

  expect_s3_class(dyntaxa_children_hierarchy, "data.frame")
  expect_true(nrow(dyntaxa_children_hierarchy) > 0)
  expect_true(all(c("id", "created", "modified", "isSecondaryRelation", "isAccepted", "scientificName", "scientificNameAuthor", "scientificNameId", "swedishName", "swedishNameAuthor", "swedishNameId", "taxonCategoryId", "hasChildren", "children") %in% names(dyntaxa_children_hierarchy)))
})

test_that("parent ids are retrieved", {
  skip_if_offline()
  skip_if_resource_unavailable(dyntaxa_url)

  dyntaxa_children_ids <- SHARK4R:::get_dyntaxa_children_ids(test_genus_id, dyntaxa_key)

  expect_equal(class(dyntaxa_children_ids), "list")
  expect_true(length(dyntaxa_children_ids) > 0)
})

test_that("construct_dyntaxa_table is working", {
  skip_if_offline()
  skip_if_resource_unavailable(dyntaxa_url)

  dyntaxa_table <- construct_dyntaxa_table(test_id, dyntaxa_key, add_descendants = FALSE, add_hierarchy = TRUE)

  expect_s3_class(dyntaxa_table, "data.frame")
  expect_true(nrow(dyntaxa_table) > 0)
  expect_true(all(c("taxon_id", "name", "rank", "author", "kingdom", "phylum", "class", "order", "family", "genus", "species", "hierarchy") %in% names(dyntaxa_table)))

  dyntaxa_table_descendants <- construct_dyntaxa_table(test_id, dyntaxa_key, add_descendants = TRUE)

  expect_gt(nrow(dyntaxa_table_descendants), nrow(dyntaxa_table))

  dyntaxa_table_missing <- construct_dyntaxa_table(c(test_id, 235007), dyntaxa_key, add_missing_taxa = TRUE)
  expect_gt(nrow(dyntaxa_table_missing), nrow(dyntaxa_table))
})

test_that("taxon match is working as expected", {
  skip_if_offline()
  skip_if_resource_unavailable(dyntaxa_url)

  taxon_match <- match_dyntaxa_taxa(test_species_name, dyntaxa_key)

  expect_s3_class(taxon_match, "data.frame")
  expect_true(nrow(taxon_match) > 0)
  expect_true(all(c("search_pattern", "taxon_id", "best_match", "author", "valid_name") %in% names(taxon_match)))
})

test_that("deprecated taxon match is working as expected", {
  taxon_match <- expect_error(lifecycle::expect_deprecated(
    match_taxon_name(test_species_name, NULL)))
})

test_that("update_dyntaxa_taxonomy is working as expected", {
  skip_if_offline()
  skip_if_resource_unavailable(dyntaxa_url)

  updated_data <- update_dyntaxa_taxonomy(test_id, dyntaxa_key)

  expect_s3_class(updated_data, "data.frame")
  expect_true(nrow(updated_data) > 0)
  expect_true(all(c("dyntaxa_id", "scientific_name", "taxon_kingdom", "taxon_phylum", "taxon_class", "taxon_order", "taxon_family", "taxon_genus", "taxon_species", "taxon_hierarchy") %in% names(updated_data)))
})

test_that("is_in_dyntaxa returns logical vector", {
  skip_on_cran()
  skip_if_offline()
  skip_if_resource_unavailable(dyntaxa_url)

  # Replace with your real DYNTAXA_KEY for local testing
  Sys.setenv(DYNTAXA_KEY = Sys.getenv("DYNTAXA_KEY"))

  taxa <- c("Skeletonema marinoi", "Nonexistent species")

  result <- is_in_dyntaxa(taxa, verbose = FALSE)

  expect_type(result, "logical")
  expect_length(result, length(taxa))
})

test_that("is_in_dyntaxa detects unmatched taxa", {
  skip_on_cran()
  skip_if_offline()
  skip_if_resource_unavailable(dyntaxa_url)

  taxa <- c("Skeletonema marinoi", "Nonexistent species")

  # Capture messages when verbose = TRUE
  expect_message(res <- is_in_dyntaxa(taxa, verbose = TRUE), "Unmatched taxa found")
  expect_length(res, length(taxa))
  expect_false(all(res)) # At least one should be FALSE
})

test_that("match_dyntaxa calls is_in_dyntaxa and warns about deprecation", {
  skip_on_cran()
  skip_if_offline()
  skip_if_resource_unavailable(dyntaxa_url)

  taxa <- c("Skeletonema marinoi", "Nonexistent species")

  expect_warning(res <- match_dyntaxa(taxa), "deprecated")
  expect_type(res, "logical")
  expect_length(res, length(taxa))
})

test_that("is_in_dyntaxa stops if subscription key is missing", {
  skip_on_cran()
  skip_if_offline()
  skip_if_resource_unavailable(dyntaxa_url)

  Sys.setenv(DYNTAXA_KEY = "")

  expect_error(is_in_dyntaxa("Skeletonema marinoi"), "No Dyntaxa subscription key provided")
})
