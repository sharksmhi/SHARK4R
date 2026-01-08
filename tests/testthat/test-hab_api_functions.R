test_that("get_toxin_list works", {
  skip_if_offline()
  skip_if_resource_unavailable("https://toxins.hais.ioc-unesco.org/")

  toxin_list <- suppressWarnings(get_toxin_list())

  expect_s3_class(toxin_list, "data.frame")
  expect_true(nrow(toxin_list) > 0)
  expected_names <- c(
    "id", "toxin_group", "recommended_name", "synonyms",
    "recommended_acronym", "acronyms", "cas_number", "alternative_cas_numbers",
    "formula", "exact_mono_isotopic_mass", "molfile", "alternative_molfiles",
    "smiles", "alternative_smiles", "inchi_key", "alternative_inchi_keys",
    "inchi", "alternative_inchies", "spectra_available", "certified",
    "non_certified_reference_material", "chemical_analysis_research",
    "chemical_analysis_standardized", "chemical_analysis_validated",
    "chemical_analysis_official", "structure_recognition_assays_research",
    "structure_recognition_assays_standardized", "structure_recognition_assays_validated",
    "structure_recognition_assays_official", "functional_assays_research",
    "functional_assays_standardized", "functional_assays_validated",
    "functional_assays_official", "animal_assays_research",
    "animal_assays_standardized", "animal_assays_validated",
    "animal_assays_official", "regulatory_status", "risk_assessment",
    "molecular_targets_known", "tef_available", "haedat_identifier", "note",
    "human_toxic_syndrome", "organ_system_toxicity", "molecular_targets",
    "certified_links", "algal_species", "vector_species", "references",
    "detection_references", "chem_info_references", "regulation_references",
    "taxon_references", "tef_references"
  )

  expect_named(toxin_list, expected_names)
})

test_that("get_hab_list works", {
  skip_if_offline()
  skip_if_resource_unavailable("https://www.marinespecies.org/")

  hab_list <- get_hab_list()

  expect_s3_class(hab_list, "data.frame")
  expect_true(nrow(hab_list) > 0)

  expected_names <- c(
    "AphiaID", "ScientificName", "Authority",
    "AphiaID_accepted", "ScientificName_accepted", "Authority_accepted",
    "Fossil", "Kingdom", "Phylum",
    "Class", "Order", "Family",
    "taxonRank", "Genus", "Subgenus",
    "Species", "Subspecies", "Marine",
    "Brackish", "Fresh", "Terrestrial",
    "taxonomicStatus", "Qualitystatus", "Unacceptreason",
    "DateLastModified", "LSID", "Parent AphiaID",
    "Storedpath"
  )

  expect_true(all(expected_names %in% names(hab_list)))
})

test_that("get_hab_list works with non-toxic taxa only", {
  skip_if_offline()
  skip_if_resource_unavailable("https://www.marinespecies.org/")

  hab_list <- get_hab_list(harmful_non_toxic_only = TRUE,
                           verbose = FALSE)

  expect_s3_class(hab_list, "data.frame")
  expect_true(nrow(hab_list) > 0)

  expected_names <- c(
    "AphiaID", "ScientificName", "Authority",
    "AphiaID_accepted", "ScientificName_accepted", "Authority_accepted",
    "Fossil", "Kingdom", "Phylum",
    "Class", "Order", "Family",
    "taxonRank", "Genus", "Marine",
    "Brackish", "Fresh", "Terrestrial",
    "taxonomicStatus", "Unacceptreason",
    "DateLastModified", "LSID", "Parent AphiaID"
  )

  expect_true(all(expected_names %in% names(hab_list)))
})

test_that("extract_complete_toxins extracts full toxin objects", {
  json <- paste0(
    '{"toxins": [',
    '{"id": 1, "name": "A"},',
    '{"id": 2, "name": "B"}',
    ']}'
  )

  objs <- extract_complete_toxins(json)

  expect_type(objs, "character")
  expect_length(objs, 2)
  expect_true(all(grepl('"id":', objs)))
  expect_true(any(grepl('"id": 1', objs)))
  expect_true(any(grepl('"id": 2', objs)))
})

test_that("extract_complete_toxins ignores incomplete trailing objects", {
  partial <- paste0(
    '{"toxins": [',
    '{"id": 1, "name": "A"},',
    '{"id": 2, "name": "B"},',
    '{"id": 3, "name": "C"'  # no closing brace
  )

  objs <- extract_complete_toxins(partial)

  # Should recover 2 complete objects
  expect_length(objs, 2)
  expect_false(any(grepl('"id": 3', objs)))
})

test_that("extract_complete_toxins handles double commas and whitespace", {
  json <- paste0(
    '{"toxins": [',
    '{"id": 1, "x": 10},,',
    ' {"id": 2, "x": 20"}',
    ']}'
  )

  objs <- extract_complete_toxins(json)

  expect_length(objs, 2)
  expect_true(any(grepl('"id": 1', objs)))
  expect_true(any(grepl('"id": 2', objs)))
})

test_that("extract_complete_toxins returns empty vector when no array found", {
  json <- '{"nothing_here": []}'

  objs <- extract_complete_toxins(json)

  expect_length(objs, 0)
})

test_that("repair_toxins_json wraps extracted objects in proper JSON", {
  json <- paste0(
    '{"toxins": [',
    '{"id": 1}, {"id": 2}',
    ']}'
  )

  repaired <- repair_toxins_json(json)
  parsed <- jsonlite::fromJSON(repaired)

  expect_s3_class(parsed$toxins, "data.frame")
  expect_equal(nrow(parsed$toxins), 2)
})

test_that("repair_toxins_json drops incomplete objects", {
  json <- paste0(
    '{"toxins": [',
    '{"id": 1},',
    '{"id": 2},',
    '{"id": 3'   # incomplete
  )

  repaired <- repair_toxins_json(json)
  parsed <- jsonlite::fromJSON(repaired)

  expect_equal(nrow(parsed$toxins), 2)
  expect_false(any(parsed$toxins$id == 3))
})
