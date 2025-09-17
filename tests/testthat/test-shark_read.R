# Define the path to the test data zip file
zip_path <- test_path("test_data/test_data.zip")

# Define the temporary directory for unzipping
temp_dir <- file.path(tempdir(), "shark_read")

# Unzip the test data
unzip(zip_path, exdir = temp_dir)

test_that("shark_read_deliv works", {
  delivery <- shark_read_deliv(file.path(temp_dir, "Format_Bacterioplankton production.xlsx"), skip = 3)

  expect_true(is.data.frame(delivery))
  expect_equal(ncol(delivery), 54)
  expect_equal(nrow(delivery), 0)
})

test_that("shark_read_deliv_xls works", {
  delivery <- shark_read_deliv_xls(file.path(temp_dir, "Format_Bacterioplankton production.xls"), skip = 3)

  expect_true(is.data.frame(delivery))
  expect_equal(ncol(delivery), 54)
  expect_equal(nrow(delivery), 0)
})

test_that("shark_read works", {
  shark_data <- shark_read(file.path(temp_dir, "sharkweb_data.txt"))

  expect_true(is.data.frame(shark_data))
  expect_equal(ncol(shark_data), 81)
  expect_equal(nrow(shark_data), 18)
})

test_that("shark_read_zip works", {
  shark_data_zip <- shark_read_zip(file.path(temp_dir, "SHARK_Chlorophyll_2022_SMHI_version_2023-04-28.zip"))

  expect_true(is.data.frame(shark_data_zip))
  expect_equal(ncol(shark_data_zip), 72)
  expect_equal(nrow(shark_data_zip), 115)
})

unlink(temp_dir, recursive = TRUE)
