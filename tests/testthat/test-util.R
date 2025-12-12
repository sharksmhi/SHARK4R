test_that("check lon lat works as expected", {
  x <- SHARK4R:::check_lonlat(data.frame(), report = TRUE)
  expect_equal(2, nrow(x))
  expect_true(all(grepl("missing", x$message)))
  x <- SHARK4R:::check_lonlat(data.frame(sample_latitude_dd = ""), report = TRUE)
  expect_equal(2, nrow(x))
  expect_true(all(grepl("(missing)|(numeric)", x$message)))
  x <- SHARK4R:::check_lonlat(data.frame(sample_latitude_dd = 1), report = TRUE)
  expect_equal(1, nrow(x))
  expect_true(all(grepl("(missing)|(numeric)", x$message)))
  x <- SHARK4R:::check_lonlat(data.frame(sample_longitude_dd = "", sample_latitude_dd = ""), report = TRUE)
  expect_equal(2, nrow(x))
  expect_true(all(grepl("numeric", x$message)))
  x <- SHARK4R:::check_lonlat(data.frame(sample_longitude_dd = "", sample_latitude_dd = 1), report = TRUE)
  expect_equal(1, nrow(x))
  expect_true(all(grepl("numeric", x$message)))
  x <- SHARK4R:::check_lonlat(data.frame(sample_longitude_dd = 1, sample_latitude_dd = 1), report = TRUE)
  expect_equal(0, NROW(x))
  expect_error(SHARK4R:::check_lonlat(data.frame(), report = FALSE), "missing")
})

test_that("cache call works", {
  n <- 5
  set.seed(42)
  original <- SHARK4R:::cache_call("random5", expression(runif(n)))
  set.seed(50)
  same <- SHARK4R:::cache_call("random5", expression(runif(n)))
  set.seed(50)
  different <- SHARK4R:::cache_call("random5diff", expression(runif(n)))
  set.seed(50)
  original2 <- SHARK4R:::cache_call("random5", expression(runif(n, min = 0, max = 1) ))
  expect_equal(length(original), n)
  expect_equal(original, same)
  expect_false(any(original == different))
  expect_equal(original2, different)
  expect_gte(length(SHARK4R:::list_cache()), 3)
  # only run on Travis as clearing the cache between the test runs is annoying
  if(identical(Sys.getenv("TRAVIS"), "true")) {
    clean_shark4r_cache(0)
    expect_equal(length(SHARK4R:::list_cache()), 0)
  }
})

test_that("get_xy_clean_duplicates works", {
  n <- 100
  set.seed(42)
  lots_duplicates <- tibble(sample_longitude_dd = as.numeric(sample(1:10, n, replace = TRUE)), sample_latitude_dd = as.numeric(sample(1:10, n, replace = TRUE)))
  lots_duplicates[5,] <- list(NA, 1.0)
  lots_duplicates[6,] <- list(2.0, NA)
  lots_duplicates[7,] <- list(NA, NA)
  xy <- SHARK4R:::get_xy_clean_duplicates(lots_duplicates)

  replicate <- tibble(sample_longitude_dd = rep(NA, n), sample_latitude_dd = rep(NA, n))
  replicate[xy$isclean,] <- xy$uniquesp[xy$duplicated_lookup,]
  replicate[!xy$isclean,] <- lots_duplicates[!xy$isclean,]
  expect_equal(lots_duplicates, replicate)
})

test_that("check_setup downloads and sets up products and scripts", {
  skip_on_cran()
  skip_if_offline()
  skip_if_resource_unavailable("https://github.com/")

  tmpdir <- file.path(tempdir(), paste0("shark_setup_", as.integer(Sys.time())))
  dir.create(tmpdir)

  result <- check_setup(path = tmpdir, verbose = TRUE)

  expect_true(dir.exists(result$products))

  unlink(tmpdir, recursive = TRUE)
})

test_that("check_setup skips download if already present", {

  tmpdir <- file.path(tempdir(), paste0("shark_setup_", as.integer(Sys.time())))
  dir.create(tmpdir)

  products <- file.path(tmpdir, "products")
  dir.create(products, recursive = TRUE)

  result <- check_setup(path = tmpdir, verbose = TRUE)

  expect_true(dir.exists(result$products))

  unlink(tmpdir, recursive = TRUE)
})

test_that("check_setup forces re-download when force = TRUE", {
  skip_on_cran()
  skip_if_offline()
  skip_if_resource_unavailable("https://github.com/")

  tmpdir <- file.path(tempdir(), paste0("shark_setup_", as.integer(Sys.time())))
  dir.create(tmpdir)

  products <- file.path(tmpdir, "products")
  dir.create(products, recursive = TRUE)

  result <- check_setup(path = tmpdir, force = TRUE, verbose = FALSE)

  expect_true(dir.exists(result$products))

  unlink(tmpdir, recursive = TRUE)
})

test_that("translate_shark_datatype translates known names correctly", {
  expect_equal(translate_shark_datatype("Grey seal"), "GreySeal")
  expect_equal(translate_shark_datatype("Harbour Porpoise"), "HarbourPorpoise")
  expect_equal(translate_shark_datatype("Bacterioplankton"), "Bacterioplankton")
})

test_that("translate_shark_datatype works for vectors", {
  input <- c("Grey seal", "Phytoplankton", "Ringed seal")
  expected <- c("GreySeal", "Phytoplankton", "RingedSeal")
  expect_equal(translate_shark_datatype(input), expected)
})

test_that("translate_shark_datatype handles unknown names gracefully", {
  input <- c("Grey seal", "UnknownType")
  expect_warning(
    out <- translate_shark_datatype(input),
    "Unknown datatype"
  )
  expect_equal(out, c("GreySeal", "UnknownType"))
})

test_that("translate_shark_datatype returns NULL for NULL input", {
  expect_null(translate_shark_datatype(NULL))
})

test_that("check_outliers works with minimal dataset", {
  skip_on_cran()
  skip_if_offline()
  skip_if_resource_unavailable("https://github.com/")

  # Load threshold data
  thresholds <- load_shark4r_stats(verbose = TRUE)

  # Create a minimal test dataset
  example_data <- tibble(
    station_name = c("S1", "S2", "S3"),
    sample_date = as.Date(c("2025-01-01", "2025-01-02", "2025-01-03")),
    sample_id = 1:3,
    shark_sample_id_md5 = letters[1:3],
    sample_min_depth_m = c(0, 5, 2),
    sample_max_depth_m = c(1, 6, 4),
    parameter = c("Chlorophyll", "Chlorophyll", "Chlorophyll"),
    value = c(0.5, 5, 50),  # third value should be an outlier
    delivery_datatype = c("Measured", "Measured", "Measured")
  )

  # Use a simple threshold dataframe
  example_thresholds <- tibble(
    parameter = "Chlorophyll",
    datatype = "Measured",
    extreme_upper = 10
  )

  # Run check_outliers
  out <- check_outliers(
    data = example_data,
    parameter = "Chlorophyll",
    datatype = "Measured",
    threshold_col = "extreme_upper",
    thresholds = example_thresholds,
    return_df = TRUE,
    verbose = FALSE
  )

  # Expect a data frame
  expect_s3_class(out, "data.frame")

  # Expect only the third row (value 50) to be flagged
  expect_equal(nrow(out), 1)
  expect_equal(out$value, 50)

  # Also test direction = "below"
  example_thresholds$extreme_lower <- 1
  out_below <- check_outliers(
    data = example_data,
    parameter = "Chlorophyll",
    datatype = "Measured",
    threshold_col = "extreme_lower",
    thresholds = example_thresholds,
    direction = "below",
    return_df = TRUE,
    verbose = FALSE
  )

  # Only first row (0.5) is below threshold
  expect_equal(nrow(out_below), 1)
  expect_equal(out_below$value, 0.5)
})

test_that("load_shark4r_fields() successfully loads .field_definitions", {
  skip_on_cran()
  skip_if_offline()
  skip_if_resource_unavailable("https://github.com/")

  defs <- load_shark4r_fields(verbose = TRUE)

  expect_type(defs, "list")
  expect_true(all(c("Phytoplankton", "Zooplankton") %in% names(defs)))
  expect_true(all(c("required", "recommended") %in% names(defs$Phytoplankton)))

  expect_true(is.character(defs$Phytoplankton$required))
  expect_true(is.character(defs$Phytoplankton$recommended))
})

test_that("load_shark4r_fields() prints messages when verbose = TRUE", {
  skip_on_cran()
  skip_if_offline()
  skip_if_resource_unavailable("https://github.com/")

  expect_message(load_shark4r_fields(verbose = TRUE),
                 "Downloading SHARK4R field definitions from GitHub")
})

test_that("convert_ddmm_to_dd handles basic DDMM inputs", {
  expect_equal(convert_ddmm_to_dd(c(5733, 6045)), c(57 + 33/60, 60 + 45/60))
})

test_that("convert_ddmm_to_dd handles inputs with fractional minutes", {
  expect_equal(convert_ddmm_to_dd(c("573345", "604523")),
               c(57 + 33.45/60, 60 + 45.23/60))
})

test_that("convert_ddmm_to_dd handles inputs with non-numeric characters", {
  expect_equal(convert_ddmm_to_dd(c("57°33'", "60°45'23\"")),
               c(57 + 33/60, 60 + 45.23/60))
})

test_that("convert_ddmm_to_dd returns NA for too short input", {
  expect_true(is.na(convert_ddmm_to_dd(c("123"))))
})

test_that("convert_ddmm_to_dd returns a numeric vector without names", {
  res <- convert_ddmm_to_dd(c(5733, 6045))
  expect_true(is.numeric(res))
  expect_null(names(res))
})

test_that("convert_ddmm_to_dd handles NA input", {
  expect_equal(convert_ddmm_to_dd(c(NA, 5733)), c(NA, 57 + 33/60))
})

test_that("is_check returns TRUE only when _R_CHECK_PACKAGE_NAME_ is set", {
  # Save old value
  old <- Sys.getenv("_R_CHECK_PACKAGE_NAME_", unset = NA)

  # Variable missing or empty
  Sys.unsetenv("_R_CHECK_PACKAGE_NAME_")
  expect_false(is_check())

  Sys.setenv("_R_CHECK_PACKAGE_NAME_" = "")
  expect_false(is_check())

  # Variable set to non empty string
  Sys.setenv("_R_CHECK_PACKAGE_NAME_" = "mypkg")
  expect_true(is_check())

  # Restore original environment
  if (is.na(old)) {
    Sys.unsetenv("_R_CHECK_PACKAGE_NAME_")
  } else {
    Sys.setenv("_R_CHECK_PACKAGE_NAME_" = old)
  }
})
