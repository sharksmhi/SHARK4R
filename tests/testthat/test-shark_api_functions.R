shark_url <- "https://shark.smhi.se"

test_that("chlorophyll data are retrieved", {
  skip_if_offline()
  skip_if_resource_unavailable(shark_url)

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
