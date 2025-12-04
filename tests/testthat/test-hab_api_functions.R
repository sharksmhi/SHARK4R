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
