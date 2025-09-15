url <- "https://www.marinespecies.org/"
test_aphia_id <- 109604 # Dinophysis acuta
test_scientific_name <- "Dinophysis acuta"

test_that("add_worms_taxonomy works", {
  skip_if_offline()
  skip_if_resource_unavailable(url)

  worms_taxonomy <- add_worms_taxonomy(c(test_aphia_id, NA), c(test_scientific_name, strsplit(test_scientific_name, " ")[[1]][1]))

  expect_s3_class(worms_taxonomy, "data.frame")
  expect_equal(nrow(worms_taxonomy), 2)
  expected_names <- c(
    "aphia_id", "scientific_name", "worms_kingdom", "worms_phylum",
    "worms_class", "worms_order", "worms_family", "worms_genus",
    "worms_species", "worms_hierarchy"
  )

  expect_true(all(expected_names %in% names(worms_taxonomy)))
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

test_that("get_worms_records_name works when taxa does not exist", {
  skip_if_offline()
  skip_if_resource_unavailable(url)

  worms_name_no_content <- get_worms_records_name("nonexistent_taxa_12345")

  expect_s3_class(worms_name_no_content, "data.frame")
  expect_equal(nrow(worms_name_no_content), 1)

  expect_true(all(c("name", "AphiaID", "status") %in% names(worms_name_no_content)))
})

test_that("assign_phytoplankton_group works", {
  skip_if_offline()
  skip_if_resource_unavailable(url)

  phytoplankton_group <- assign_phytoplankton_group(test_scientific_name)

  expect_s3_class(phytoplankton_group, "data.frame")
  expect_equal(nrow(phytoplankton_group), 1)

  expect_true(all(c("scientific_name", "plankton_group") %in% names(phytoplankton_group)))
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
