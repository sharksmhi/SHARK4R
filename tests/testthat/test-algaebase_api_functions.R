algaebase_key <- Sys.getenv("ALGAEBASE_KEY")
test_species <- "Dinophysis acuta"

test_that("parse_scientific_names works", {
  species_parsed <- parse_scientific_names(test_species)

  expect_s3_class(species_parsed, "data.frame")
  expect_equal(nrow(species_parsed), 1)
  expect_named(species_parsed, c("genus", "species"))
})

test_that("match_algaebase works", {
  skip_if_offline()
  skip_if_resource_unavailable("https://algaebase.org/")

  species_parsed <- parse_scientific_names(test_species)

  algaebase_data <- match_algaebase(species_parsed$genus, species_parsed$species, algaebase_key)

  expect_s3_class(algaebase_data, "data.frame")
  expect_equal(nrow(algaebase_data), 1)
  expected_names <- c(
    "genus", "species", "id", "accepted_name",
    "input_name", "input_match", "currently_accepted", "genus_only",
    "kingdom", "phylum", "class", "order",
    "family", "infrasp", "long_name", "taxonomic_status",
    "nomenclatural_status", "taxon_rank", "mod_date", "authorship"
  )

  expect_true(all(expected_names %in% names(algaebase_data)))
})
