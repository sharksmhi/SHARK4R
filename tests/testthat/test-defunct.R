test_that("Defunct SHARKdata functions fail", {
  expect_error(get_shark_table())
  expect_error(check_data_version())
  expect_error(update_data())
  expect_error(download_sharkdata())
  expect_error(load_dataset_names())
  expect_error(load_sharkdata())
  expect_error(load_dataset_types())
  expect_error(download_file())
  expect_error(year_filter())
  expect_error(data_deliverer_filter())
  expect_error(read_data())
  expect_error(validate_dataset_names())
})

test_that("Defunct Dyntaxa functions fail", {
  expect_error(load_dyntaxa_taxonomy())
  expect_error(read_species_list())
  expect_error(gather_species_info())
  expect_error(add_species_info())
})

test_that("Defunct WoRMS functions fail", {
  expect_error(load_worms_taxonomy())
  expect_error(gather_worms_species_info())
  expect_error(add_worms_species_info())
})

test_that("Other defunct functions fail", {
  expect_error(get_geographical_info())
  expect_error(filter_outdated_datasets())
})
