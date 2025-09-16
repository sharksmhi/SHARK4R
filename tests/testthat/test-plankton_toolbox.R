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

# NOMP tests
test_that("get_nomp_list returns a tibble for latest available year", {
  skip_on_cran()
  skip_if_offline()
  skip_if_resource_unavailable(smhi_url)

  df <- get_nomp_list(clean_cache_days = 1)

  expect_s3_class(df, "tbl_df")
  expect_true(nrow(df) > 0)
  expect_true(ncol(df) > 0)
})

test_that("get_nomp_list returns a tibble for a specific year", {
  skip_on_cran()
  skip_if_offline()
  skip_if_resource_unavailable(smhi_url)

  df <- get_nomp_list(year = 2023, clean_cache_days = 1)
  expect_s3_class(df, "tbl_df")
  expect_true(nrow(df) > 0)
})

test_that("get_nomp_list errors when specified file does not exist", {
  skip_on_cran()
  skip_if_offline()
  skip_if_resource_unavailable(smhi_url)

  expect_error(
    get_nomp_list(file = "non_existing_file.xlsx", clean_cache_days = 1),
    "Specified file not found in the zip archive"
  )
})

test_that("get_nomp_list returns first Excel file by default", {
  skip_on_cran()
  skip_if_offline()
  skip_if_resource_unavailable(smhi_url)

  zip_path <- cache_nomp_zip(year = 2023)
  tmp_dir <- tempdir()
  unzipped_files <- unzip(zip_path, exdir = tmp_dir)
  first_excel <- unzipped_files[grepl("\\.xlsx$", unzipped_files)][1]

  df_default <- get_nomp_list(year = 2023, clean_cache_days = 1)
  df_explicit <- readxl::read_excel(first_excel, guess_max = 10000, progress = FALSE)

  expect_equal(dim(df_default), dim(df_explicit))
})

# PEG tests
test_that("get_peg_list returns a tibble", {
  skip_on_cran()
  skip_if_offline()
  skip_if_resource_unavailable(ices_url)

  df <- get_peg_list(clean_cache_days = 1)

  expect_s3_class(df, "tbl_df")
  expect_true(nrow(df) > 0)
  expect_true(ncol(df) > 0)
})

test_that("get_peg_list can force re-download", {
  skip_on_cran()
  skip_if_offline()
  skip_if_resource_unavailable(ices_url)

  df <- get_peg_list(force = TRUE, clean_cache_days = 1)

  expect_s3_class(df, "tbl_df")
  expect_true(nrow(df) > 0)
})

test_that("get_peg_list errors when a non-existent file is requested", {
  skip_on_cran()
  skip_if_offline()
  skip_if_resource_unavailable(ices_url)

  expect_error(
    get_peg_list(file = "non_existing_file.xlsx", clean_cache_days = 1),
    "Specified file not found in the zip archive"
  )
})

test_that("get_peg_list prints the year in the message", {
  skip_on_cran()
  skip_if_offline()
  skip_if_resource_unavailable(ices_url)

  expect_message(
    get_peg_list(clean_cache_days = 1),
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

  excel_files <- unzipped_files[grepl("\\.xlsx$", unzipped_files, ignore.case = TRUE)]
  if (length(excel_files) > 0) {
    file_to_test <- basename(excel_files[1])
    df <- get_peg_list(file = file_to_test, clean_cache_days = 1)
    expect_s3_class(df, "tbl_df")
    expect_true(nrow(df) > 0)
  } else {
    skip("No Excel files found in PEG zip for testing specific file read.")
  }
})

# SHARK codes tests
test_that("get_shark_codes returns a tibble", {
  skip_on_cran()
  skip_if_offline()
  skip_if_resource_unavailable(smhi_url)

  df <- get_shark_codes(clean_cache_days = 1)

  expect_s3_class(df, "tbl_df")
  expect_true(nrow(df) > 0)
})

test_that("clean_shark4r_cache removes old files and keeps recent ones", {
  tmp_cache <- file.path(tempdir(), "SHARK4R_test_cache")
  dir.create(tmp_cache, showWarnings = FALSE)

  old_file <- file.path(tmp_cache, "old_file.zip")
  new_file <- file.path(tmp_cache, "new_file.zip")
  writeLines("dummy", old_file)
  writeLines("dummy", new_file)

  Sys.setFileTime(old_file, Sys.time() - 2*24*60*60)
  Sys.setFileTime(new_file, Sys.time())

  expect_message(clean_shark4r_cache(days = 1, cache_dir = tmp_cache), "Removed 1 file")

  remaining_files <- list.files(tmp_cache, full.names = TRUE)
  expect_true(file.exists(new_file))
  expect_false(file.exists(old_file))
  expect_length(remaining_files, 1)

  unlink(tmp_cache, recursive = TRUE)
})

test_that("clean_shark4r_cache does nothing if no old files", {
  tmp_cache <- file.path(tempdir(), "SHARK4R_test_cache")
  dir.create(tmp_cache, showWarnings = FALSE)

  new_file <- file.path(tmp_cache, "new_file.zip")
  writeLines("dummy", new_file)
  Sys.setFileTime(new_file, Sys.time())

  expect_message(clean_shark4r_cache(days = 1, cache_dir = tmp_cache),
                 "No files older than 1 days to remove.")

  unlink(tmp_cache, recursive = TRUE)
})

test_that("clean_shark4r_cache handles missing cache directory gracefully", {
  tmp_cache <- file.path(tempdir(), "SHARK4R_test_cache_nonexistent")
  if (dir.exists(tmp_cache)) unlink(tmp_cache, recursive = TRUE)

  expect_message(clean_shark4r_cache(days = 1, cache_dir = tmp_cache),
                 "No SHARK4R cache directory found.")
})
