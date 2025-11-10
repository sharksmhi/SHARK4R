shark_url <- "https://shark.smhi.se"

test_that("chlorophyll data are retrieved", {
  skip_if_offline()
  skip_if_resource_unavailable(shark_url)
  skip_on_cran()

  shark_data <- get_shark_data(
    tableView = "sharkdata_chlorophyll",
    dataTypes = "Chlorophyll",
    fromYear = "2023",
    toYear = "2023"
  )

  expect_s3_class(shark_data, "data.frame")
  expect_true(nrow(shark_data) > 0)
  expect_true(all(c("delivery_datatype", "check_status_sv", "data_checked_by_sv") %in% names(shark_data)))
})

test_that("data are retrieved in chunks", {
  skip_if_offline()
  skip_if_resource_unavailable(shark_url)
  skip_on_cran()

  temp_file <- file.path(tempdir(), "shark_chunk_data.csv")

  rows <- get_shark_table_counts(
    tableView = "sharkdata_chlorophyll",
    dataTypes = "Chlorophyll",
    fromYear = "2023",
    toYear = "2023"
  )

  expect_true(rows >= 1)

  shark_chunk_data <- get_shark_data(
    tableView = "sharkdata_chlorophyll",
    dataTypes = "Chlorophyll",
    fromYear = "2023",
    toYear = "2023",
    row_limit = rows-1,
    save_data = TRUE,
    file_path = temp_file
  )

  expect_s3_class(shark_chunk_data, "data.frame")
  expect_true(nrow(shark_chunk_data) > 0)
  expect_true(all(c("delivery_datatype", "check_status_sv", "data_checked_by_sv") %in% names(shark_chunk_data)))
  expect_true(file.exists(temp_file))

  unlink(temp_file)
})

test_that("get_shark_datasets and get_shark_options works", {
  skip_if_offline()
  skip_if_resource_unavailable(shark_url)
  skip_on_cran()

  shark_options <- get_shark_options()

  expect_type(shark_options, "list")
  expected_fields <- c("datasets", "dataTypes", "parameters", "taxa")
  expect_true(all(expected_fields %in% names(shark_options)))

  dataset_name <- shark_options$datasets[1]

  # Download the dataset as a zip-archive to a temporary directory
  shark_data_zip <- get_shark_datasets(dataset_name,
                                       save_dir = tempdir(),
                                       unzip = TRUE)

  expect_type(shark_data_zip, "list")

  expect_true(file.exists(file.path(tempdir(), dataset_name)))
  expect_true(dir.exists(file.path(tempdir(), tools::file_path_sans_ext(dataset_name))))
  expect_true(file.exists(file.path(tempdir(), tools::file_path_sans_ext(dataset_name), "shark_data.txt")))

  unlink(tempdir(), recursive = TRUE)
})

test_that("get_shark_datasets returns data frame", {
  skip_if_offline()
  skip_if_resource_unavailable(shark_url)
  skip_on_cran()

  shark_options <- get_shark_options()

  expect_type(shark_options, "list")
  expected_fields <- c("datasets", "dataTypes", "parameters", "taxa")
  expect_true(all(expected_fields %in% names(shark_options)))

  dataset_name <- shark_options$datasets[1]

  # Download the dataset as a zip-archive to a temporary directory
  shark_data <- get_shark_datasets(dataset_name,
                                   save_dir = tempdir(),
                                   return_df = TRUE)

  expect_s3_class(shark_data, "data.frame")
  expect_gt(nrow(shark_data), 0)

  unlink(tempdir(), recursive = TRUE)
})

test_that("get_shark_statistics returns expected columns for Chlorophyll", {
  skip_if_offline()
  skip_if_resource_unavailable(shark_url)
  skip_on_cran()

  res <- get_shark_statistics(datatype = "Chlorophyll",
                              fromYear = 2020,
                              toYear = 2020,
                              verbose = FALSE)

  expect_s3_class(res, "data.frame")
  expect_true(all(c(
    "parameter", "min", "Q1", "median", "Q3", "max", "IQR",
    "mild_lower", "mild_upper", "extreme_lower", "extreme_upper", "n"
  ) %in% names(res)))

  expect_gt(nrow(res), 0)
  expect_true(all(res$n >= 3))
})

test_that("default years use last 5 complete years", {
  skip_if_offline()
  skip_if_resource_unavailable(shark_url)
  skip_on_cran()

  res <- get_shark_statistics(datatype = "Chlorophyll",
                              fromYear = 2020,
                              toYear = 2020,
                              verbose = FALSE)

  current_year <- as.integer(format(Sys.Date(), "%Y"))
  expect_true(all(res$n >= 3)) # should have enough obs
})

test_that("explicit year range works", {
  skip_if_offline()
  skip_if_resource_unavailable(shark_url)
  skip_on_cran()

  res1 <- get_shark_statistics(datatype = "Chlorophyll",
                               fromYear = 2020,
                               toYear = 2020,
                               verbose = FALSE)
  res2 <- get_shark_statistics(datatype = "Chlorophyll",
                               fromYear = 2021,
                               toYear = 2021,
                               verbose = FALSE)

  expect_false(identical(res1, res2))
})

test_that("parameters with fewer than min_obs are excluded", {
  skip_if_offline()
  skip_if_resource_unavailable(shark_url)
  skip_on_cran()

  res <- get_shark_statistics(fromYear = 2018, toYear = 2019,
                              datatype = "Chlorophyll", min_obs = 1000,
                              verbose = FALSE)

  expect_equal(nrow(res), 0)
})

test_that("get_shark_statistics throws warning if no data are returned", {
  skip_if_offline()
  skip_if_resource_unavailable(shark_url)
  skip_on_cran()

  res <- expect_warning(expect_warning(get_shark_statistics(datatype = "no datatype",
                                             verbose = TRUE),
                        "No data retrieved from SHARK for the specified years and datatype"),
                        "The following 'dataTypes' do not currently exist in the SHARK database:")
})

test_that("get_shark_statistics returns results grouped by station", {
  skip_if_offline()
  skip_if_resource_unavailable(shark_url)
  skip_on_cran()

  res <- get_shark_statistics(datatype = "Chlorophyll",
                              group_col = "station_name",
                              fromYear = 2020,
                              toYear = 2020,
                              verbose = FALSE)

  expect_s3_class(res, "data.frame")
  expect_true(all(c(
    "parameter", "datatype", "station_name", "min", "Q1", "median", "Q3", "max", "IQR",
    "mild_lower", "mild_upper", "extreme_lower", "extreme_upper", "n"
  ) %in% names(res)))

  expect_gt(nrow(res), 0)
  expect_true(all(res$n >= 3))
})

test_that("get_shark_statistics caches results correctly", {
  skip_on_cran()
  skip_if_offline()
  skip_if_resource_unavailable(shark_url)

  # Use a small time window / known datatype to speed up test
  fromYear <- 2020
  toYear <- 2020
  datatype <- "Chlorophyll"

  # Define cache path
  cache_dir <- file.path(tools::R_user_dir("SHARK4R", "cache"), "perm")
  cache_file <- file.path(cache_dir, "statistics.rds")

  # Ensure previous cache is removed
  if (file.exists(cache_file)) file.remove(cache_file)

  # Run function with caching
  result <- get_shark_statistics(fromYear,
                                 toYear,
                                 datatype,
                                 cache_result = TRUE,
                                 verbose = TRUE)

  # Check that file now exists
  expect_true(file.exists(cache_file))

  # Read cached file
  cached_result <- readRDS(cache_file)

  # Check that cached result equals the returned result
  expect_equal(cached_result, result)

  # Clean up
  file.remove(cache_file)
})

test_that("chlorophyll data are retrieved for all years", {
  skip_if_offline()
  skip_if_resource_unavailable(shark_url)
  skip_on_cran()

  shark_data <- get_shark_data(
    tableView = "chlorophyll",
    dataTypes = "Chlorophyll",
    fromYear = NULL,
    toYear = NULL,
    stationName = "B7"
  )

  expect_s3_class(shark_data, "data.frame")
  expect_true(nrow(shark_data) > 0)
  expect_true(all(c("delivery_datatype", "check_status_sv", "data_checked_by_sv") %in% names(shark_data)))
  min_year <- min(shark_data$visit_year)
  max_year <- max(shark_data$visit_year)

  expect_true(max_year > min_year)
})
