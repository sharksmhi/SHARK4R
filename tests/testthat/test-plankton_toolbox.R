smhi_url <- "https://smhi.se"
ices_url <- "https://www.ices.dk"

test_that("read_ptbx works", {
  ptbx_file <- system.file("extdata", "Anholt_E_2024-09-15_0-10m.xlsx", package = "SHARK4R")

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

test_that("get_nomp_list returns a tibble for latest available year", {
  skip_on_cran()
  skip_if_offline()
  skip_if_resource_unavailable(smhi_url)

  df <- get_nomp_list()

  expect_s3_class(df, "tbl_df")
  expect_true(nrow(df) > 0)
  expect_true(ncol(df) > 0)
})

test_that("get_nomp_list returns a tibble for a specific year", {
  skip_on_cran()
  skip_if_offline()
  skip_if_resource_unavailable(smhi_url)

  # Use a year you know exists, e.g., 2023
  df <- get_nomp_list(year = 2023)
  expect_s3_class(df, "tbl_df")
  expect_true(nrow(df) > 0)
})

test_that("get_nomp_list errors when specified file does not exist", {
  skip_on_cran()
  skip_if_offline()
  skip_if_resource_unavailable(smhi_url)

  expect_error(
    get_nomp_list(file = "non_existing_file.xlsx"),
    "Specified file not found in the zip archive"
  )
})

test_that("get_nomp_list returns first Excel file by default", {
  skip_on_cran()
  skip_if_offline()
  skip_if_resource_unavailable(smhi_url)

  zip_path <- cache_nomps_zip(year = 2023)
  tmp_dir <- tempdir()
  unzipped_files <- unzip(zip_path, exdir = tmp_dir)
  first_excel <- unzipped_files[grepl("\\.xlsx$", unzipped_files)][1]

  df_default <- get_nomp_list(year = 2023)
  df_explicit <- readxl::read_excel(first_excel, guess_max = 10000, progress = FALSE)

  expect_equal(dim(df_default), dim(df_explicit))
})

test_that("get_peg_list returns a tibble", {
  skip_on_cran()
  skip_if_offline()
  skip_if_resource_unavailable(ices_url)

  df <- get_peg_list()

  expect_s3_class(df, "tbl_df")
  expect_true(nrow(df) > 0)
  expect_true(ncol(df) > 0)
})

test_that("get_peg_list can force re-download", {
  skip_on_cran()
  skip_if_offline()
  skip_if_resource_unavailable(ices_url)

  df <- get_peg_list(force = TRUE)

  expect_s3_class(df, "tbl_df")
  expect_true(nrow(df) > 0)
})

test_that("get_peg_list errors when a non-existent file is requested", {
  skip_on_cran()
  skip_if_offline()
  skip_if_resource_unavailable(ices_url)

  expect_error(
    get_peg_list(file = "non_existing_file.xlsx"),
    "Specified file not found in the zip archive"
  )
})

test_that("get_peg_list prints the year in the message", {
  skip_on_cran()
  skip_if_offline()
  skip_if_resource_unavailable(ices_url)

  expect_message(
    get_peg_list(),
    "Reading PEG biovolume Excel file for year: \\d{4}"
  )
})

test_that("get_peg_list reads a specific file if requested", {
  skip_on_cran()
  skip_if_offline()
  skip_if_resource_unavailable(ices_url)

  zip_path <- cache_peg_zip()
  tmp_dir <- tempdir()
  unzipped_files <- unzip(zip_path, exdir = tmp_dir)

  # Pick an Excel file inside the zip
  excel_files <- unzipped_files[grepl("\\.xlsx$", unzipped_files, ignore.case = TRUE)]
  if (length(excel_files) > 0) {
    file_to_test <- basename(excel_files[1])
    df <- get_peg_list(file = file_to_test)
    expect_s3_class(df, "tbl_df")
    expect_true(nrow(df) > 0)
  } else {
    skip("No Excel files found in PEG zip for testing specific file read.")
  }
})
