test_that("read_ptbx works", {
  ptbx_file <- system.file("extdata", "Anholt E_2024-09-15_0-10m.xlsx", package = "SHARK4R")

  ptbx_data <- read_ptbx(ptbx_file)

  expect_s3_class(ptbx_data, "data.frame")
  expect_true(nrow(ptbx_data) == 89)
  expect_named(ptbx_data, c('scientific_full_name', 'taxon_class', 'scientific_name', 'size_class',
                            'method_step', 'count_area_number', 'locked_at_area', 'counted_units',
                            'counted_units_list', 'abundance_class', 'coefficient', 'abundance_units_l',
                            'volume_mm3_l', 'carbon_ugc_l', 'volume_um3_unit', 'carbon_pgc_unit',
                            'variable_comment', 'trophic_type', 'unit_type', 'species_flag_code', 'cf',
                            'bvol_list', 'bvol_list_calc'))
})
