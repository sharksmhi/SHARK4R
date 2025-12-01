url <- "https://www.marinespecies.org/"
test_aphia_id <- 109604 # Dinophysis acuta
test_aphia_id_higher <- 7 # Chromista
test_scientific_name <- "Dinophysis acuta"
test_genus_name <- strsplit(test_scientific_name, " ")[[1]][1]
test_names <- c("Abra", "Dinophysis", "ljkf hlqsdkf")

test_that("add_worms_taxonomy works", {
  skip_if_offline()
  skip_if_resource_unavailable(url)

  worms_taxonomy <- add_worms_taxonomy(c(test_aphia_id, NA), c(test_scientific_name, strsplit(test_scientific_name, " ")[[1]][1]),
                                       add_rank_to_hierarchy = TRUE)

  expect_s3_class(worms_taxonomy, "data.frame")
  expect_equal(nrow(worms_taxonomy), 2)
  expected_names <- c(
    "aphia_id", "scientific_name", "worms_kingdom", "worms_phylum",
    "worms_class", "worms_order", "worms_family", "worms_genus",
    "worms_species", "worms_hierarchy"
  )

  expect_true(all(expected_names %in% names(worms_taxonomy)))
})

test_that("deprecated arguments works in add_worms_taxonomy", {
  skip_if_offline()
  skip_if_resource_unavailable(url)

  lifecycle::expect_deprecated(
    lifecycle::expect_deprecated(
      add_worms_taxonomy(aphia_id = c(test_aphia_id, NA),
                         scientific_name = c(test_scientific_name, strsplit(test_scientific_name, " ")[[1]][1]),
                         add_rank_to_hierarchy = TRUE)))
})

test_that("get_worms_records works", {
  skip_if_offline()
  skip_if_resource_unavailable(url)

  worms_records <- get_worms_records(test_aphia_id)

  expect_s3_class(worms_records, "data.frame")
  expect_equal(nrow(worms_records), 1)
  expected_names <- c(
    "AphiaID", "url", "scientificname", "authority", "status",
    "unacceptreason", "taxonRankID", "rank", "valid_AphiaID", "valid_name",
    "valid_authority", "parentNameUsageID", "kingdom", "phylum", "class",
    "order", "family", "genus", "citation", "lsid",
    "isMarine", "isBrackish", "isFreshwater", "isTerrestrial", "isExtinct",
    "match_type", "modified"
  )

  expect_true(all(expected_names %in% names(worms_records)))
})

test_that("get_worms_records works when taxa does not exist", {
  skip_if_offline()
  skip_if_resource_unavailable(url)

  worms_no_content <- get_worms_records(-1)

  expect_s3_class(worms_no_content, "data.frame")
  expect_equal(nrow(worms_no_content), 1)

  expect_true(all(c("AphiaID", "status") %in% names(worms_no_content)))
})

test_that("get_worms_records works with deprecated arguments", {
  skip_if_offline()
  skip_if_resource_unavailable(url)

  lifecycle::expect_deprecated(get_worms_records(aphia_id = -1))
})

test_that("match_worms_taxa works when taxa does not exist", {
  skip_if_offline()
  skip_if_resource_unavailable(url)

  worms_name_no_content <- match_worms_taxa("nonexistent_taxa_12345")

  expect_s3_class(worms_name_no_content, "data.frame")
  expect_equal(nrow(worms_name_no_content), 1)

  expect_true(all(c("name", "AphiaID", "status") %in% names(worms_name_no_content)))
})

test_that("match_worms_taxa works when request is in bulk", {
  skip_if_offline()
  skip_if_resource_unavailable(url)

  worms_name_no_content <- match_worms_taxa(test_scientific_name, bulk = TRUE)

  expect_s3_class(worms_name_no_content, "data.frame")
  expect_equal(nrow(worms_name_no_content), 1)

  expect_true(all(c("name", "AphiaID", "status") %in% names(worms_name_no_content)))
})

test_that("match_worms_taxa works with non-existing taxa when request is in bulk", {
  skip_if_offline()
  skip_if_resource_unavailable(url)

  worms_name_no_content <- match_worms_taxa("notaname", bulk = TRUE)

  expect_s3_class(worms_name_no_content, "data.frame")
  expect_equal(nrow(worms_name_no_content), 1)

  expect_true(all(c("name", "AphiaID", "status") %in% names(worms_name_no_content)))
})

test_that("deprecated get_worms_records_name works", {
  skip_if_offline()
  skip_if_resource_unavailable(url)

  worms_name_no_content <- lifecycle::expect_deprecated(get_worms_records_name("nonexistent_taxa_12345"))
})

test_that("assign_phytoplankton_group works", {
  skip_if_offline()
  skip_if_resource_unavailable(url)

  phytoplankton_group <- assign_phytoplankton_group(c(test_scientific_name,
                                                      paste(test_genus_name, "nonsense")))

  expect_s3_class(phytoplankton_group, "data.frame")
  expect_equal(nrow(phytoplankton_group), 2)

  expect_true(all(c("scientific_name", "plankton_group") %in% names(phytoplankton_group)))

  phytoplankton_group2 <- assign_phytoplankton_group(test_scientific_name, return_class = TRUE)

  expect_s3_class(phytoplankton_group2, "data.frame")
  expect_equal(nrow(phytoplankton_group2), 1)

  expect_true(all(c("scientific_name", "class", "plankton_group") %in% names(phytoplankton_group2)))
})

test_that("assign_phytoplankton_group works with custom groups", {
  skip_if_offline()
  skip_if_resource_unavailable(url)

  custom_groups <- list(
    Cryptophytes = list(class = "Cryptophyceae"),
    Ciliates = list(phylum = "Ciliophora")
  )

  phytoplankton_custom_group <- assign_phytoplankton_group(
    scientific_names = c("Teleaulax amphioxeia", "Mesodinium rubrum"),
    aphia_ids = c(106306, 232069),
    custom_groups = custom_groups,         # Adding custom groups
    verbose = TRUE
  )

  expect_s3_class(phytoplankton_custom_group, "data.frame")
  expect_equal(nrow(phytoplankton_custom_group), 2)

  expect_true(all(c("scientific_name", "plankton_group") %in% names(phytoplankton_custom_group)))
})

test_that("deprecated match_worms_taxa_interactive works as expected", {
  lifecycle::expect_deprecated(match_wormstaxa(test_names, ask = FALSE))
})

test_that("deprecated update_worms_taxonomy with deprecated argument works as expected", {
  lifecycle::expect_deprecated(lifecycle::expect_deprecated(
    update_worms_taxonomy(aphiaid = test_aphia_id))
  )
})

test_that("match_worms_taxa handles empty or NA taxa gracefully", {
  res_empty <- match_worms_taxa(c("", NA))
  expect_s3_class(res_empty, "data.frame")
  expect_true("" %in% res_empty$name)
  expect_true(all(c("status", "AphiaID", "rank", "scientificname") %in% names(res_empty)))
})

test_that("clean_taxon removes problematic characters correctly", {
  taxa <- c("Karenia/brevis", "Amphidinium|sp.", "Test#1", "Weird_\\name?")
  clean_taxa <- vapply(taxa, SHARK4R:::clean_taxon, character(1))
  expect_true(all(grepl("^[A-Za-z0-9 _]+$", clean_taxa)))
})

test_that("Function returns a data.frame with expected columns", {
  skip_if_offline()
  skip_if_resource_unavailable(url)
  result <- get_worms_taxonomy_tree(test_aphia_id_higher, verbose = FALSE)
  expect_s3_class(result, "data.frame")
  expect_true(all(c("AphiaID", "scientificname", "rank") %in% colnames(result)))
})

test_that("Function handles multiple AphiaIDs", {
  skip_if_offline()
  skip_if_resource_unavailable(url)
  result <- get_worms_taxonomy_tree(c(test_aphia_id_higher, 2), verbose = FALSE)  # 231772 = Dinophysaceae
  expect_true(length(unique(result$AphiaID)) >= 2)
})

test_that("Function returns parentNameUsageID column", {
  skip_if_offline()
  skip_if_resource_unavailable(url)
  result <- get_worms_taxonomy_tree(test_aphia_id_higher, verbose = FALSE)
  expect_true("parentNameUsageID" %in% colnames(result))
})

test_that("add_descendants adds extra children", {
  skip_if_offline()
  skip_if_resource_unavailable(url)
  result_no_children <- get_worms_taxonomy_tree(test_aphia_id, add_descendants = FALSE, verbose = FALSE)
  result_with_children <- get_worms_taxonomy_tree(test_aphia_id, add_descendants = TRUE, verbose = TRUE)
  expect_true(nrow(result_with_children) > nrow(result_no_children))
})

test_that("add_synonyms adds synonym records", {
  skip_if_offline()
  skip_if_resource_unavailable(url)
  result_no_synonyms <- get_worms_taxonomy_tree(test_aphia_id_higher, add_synonyms = FALSE, verbose = FALSE)
  result_with_synonyms <- get_worms_taxonomy_tree(test_aphia_id_higher, add_synonyms = TRUE, verbose = TRUE)
  # There should be at least as many rows when synonyms are included
  expect_true(nrow(result_with_synonyms) >= nrow(result_no_synonyms))
})

test_that("add_hierarchy adds extra column", {
  skip_if_offline()
  skip_if_resource_unavailable(url)
  result_no_hierarchy <- get_worms_taxonomy_tree(test_aphia_id, add_hierarchy = FALSE, verbose = FALSE)
  result_with_hierarchy <- get_worms_taxonomy_tree(test_aphia_id, add_hierarchy = TRUE, verbose = TRUE)
  expect_true(ncol(result_with_hierarchy) > ncol(result_no_hierarchy))
})

test_that("Function fails gracefully with NA or missing AphiaIDs", {
  expect_error(get_worms_taxonomy_tree(NA), "No valid 'aphia_ids' provided")
})

test_that("Function handles invalid AphiaIDs without crashing", {
  skip_if_offline()
  skip_if_resource_unavailable(url)
  result <- get_worms_taxonomy_tree(999999999, verbose = FALSE)  # very unlikely AphiaID
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})
