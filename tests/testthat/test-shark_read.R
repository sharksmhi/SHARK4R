# Setup temporary directory
# Setup: unzip test data
zip_path <- test_path("test_data/test_data.zip")
exdir <- file.path(tempdir(), "read_shark")
dir.create(exdir, showWarnings = FALSE)
unzip(zip_path, exdir = exdir)

# -----------------------------
# Test read_shark_deliv (.xlsx)
# -----------------------------
test_that("read_shark_deliv reads valid .xlsx", {
  file <- file.path(exdir, "Format_Bacterioplankton production.xlsx")
  result <- read_shark_deliv(file, skip = 3, sheet = 4)

  expect_true(is.data.frame(result))
  expect_gt(nrow(result), 0)
  expect_gt(ncol(result), 0)
})

test_that("read_shark_deliv handles non-existent file", {
  file <- file.path(exdir, "nonexistent.xlsx")
  expect_message(result <- read_shark_deliv(file), "does not exist")
  expect_null(result)
})

# -----------------------------
# Test read_shark_deliv (.xls)
# -----------------------------
test_that("read_shark_deliv reads valid .xls", {
  file <- file.path(exdir, "Format_Bacterioplankton production.xls")
  result <- read_shark_deliv(file, skip = 3, sheet = 4)

  expect_true(is.data.frame(result))
  expect_gt(nrow(result), 0)
  expect_gt(ncol(result), 0)
})

# -----------------------------
# Test read_shark (tab-delimited)
# -----------------------------
test_that("read_shark reads valid .txt", {
  file <- file.path(exdir, "sharkweb_data.txt")
  result <- read_shark(file)

  expect_true(is.data.frame(result))
  expect_gt(nrow(result), 0)
  expect_gt(ncol(result), 0)
})

test_that("read_shark transform encoding", {
  file <- file.path(exdir, "sharkweb_data.txt")
  expect_message(read_shark(file, encoding = "utf_8"), "Detected encoding")
  result <- read_shark(file, encoding = "utf_8")

  expect_true(is.data.frame(result))
  expect_gt(nrow(result), 0)
  expect_gt(ncol(result), 0)
})

test_that("read_shark reads valid .txt", {
  file <- file.path(exdir, "sharkweb_data.txt")
  expect_warning(read_shark(file,
                            encoding = "no-encoding"),
                 "'encoding' must be one of")

  result <- suppressWarnings(read_shark(file,
                                        encoding = "no-encoding"))

  expect_gt(nrow(result), 0)
  expect_gt(ncol(result), 0)

  expect_warning(read_shark(file,
                            delimiters = "no-delim"),
                 "Invalid 'delimiters'")

  result <- suppressWarnings(read_shark(file,
                                        delimiters = "no-delim"))

  expect_true(is.data.frame(result))
  expect_gt(nrow(result), 0)
  expect_gt(ncol(result), 0)
})

# -----------------------------
# Test read_shark
# -----------------------------
test_that("read_shark reads zip with shark_data.txt", {
  file <- file.path(exdir, "SHARK_Chlorophyll_2022_SMHI_version_2023-04-28.zip")
  result <- read_shark(file)

  expect_true(is.data.frame(result))
  expect_gt(nrow(result), 0)
  expect_gt(ncol(result), 0)
})

test_that("read_shark reads zip with shark_data.txt", {
  file <- file.path(exdir, "SHARK_Chlorophyll_2022_SMHI_version_2023-04-28.zip")
  expect_message(read_shark(file, encoding = "utf_8"), "Detected encoding")
  result <- read_shark(file, encoding = "utf_8")

  expect_true(is.data.frame(result))
  expect_gt(nrow(result), 0)
  expect_gt(ncol(result), 0)
})

test_that("read_shark reads valid .txt", {
  file <- file.path(exdir, "SHARK_Chlorophyll_2022_SMHI_version_2023-04-28.zip")
  expect_warning(read_shark(file,
                                encoding = "no-encoding"),
                 "'encoding' must be one of")

  result <- suppressWarnings(read_shark(file,
                                            encoding = "no-encoding"))

  expect_gt(nrow(result), 0)
  expect_gt(ncol(result), 0)

  expect_warning(read_shark(file,
                                delimiters = "no-delim"),
                 "Invalid 'delimiters'")

  result <- suppressWarnings(read_shark(file,
                                            delimiters = "no-delim"))

  expect_true(is.data.frame(result))
  expect_gt(nrow(result), 0)
  expect_gt(ncol(result), 0)
})

# -----------------------------
# Deprecation works
# -----------------------------
test_that("read_shark reads zip with shark_data.txt", {
  file1 <- file.path(exdir, "sharkweb_data.txt")
  file2 <- file.path(exdir, "SHARK_Chlorophyll_2022_SMHI_version_2023-04-28.zip")
  file3 <- file.path(exdir, "Format_Bacterioplankton production.xlsx")
  file4 <- file.path(exdir, "Format_Bacterioplankton production.xls")

  lifecycle::expect_deprecated(shark_read(file1))
  lifecycle::expect_deprecated(shark_read_zip(file2))
  lifecycle::expect_deprecated(shark_read_deliv_xls(file4, skip = 3, sheet = 4))
  lifecycle::expect_deprecated(shark_read_deliv(file3, skip = 3, sheet = 4))
})

# Cleanup
unlink(exdir)
