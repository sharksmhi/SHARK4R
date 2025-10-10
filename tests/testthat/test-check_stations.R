test_that("check_nominal_station returns coordinates only when suspected nominal", {
  # Case where positions are NOT suspected
  data_ok <- dplyr::tibble(
    sample_date = c("2025-09-17", "2025-09-18"),
    station_name = c("N7 OST NIDINGEN", "KA6"),
    sample_latitude_dd = c(57.3, 56.6),
    sample_longitude_dd = c(12.0, 12.6)
  )

  expect_message(result2 <- check_nominal_station(data_ok),
                 "Positions are not suspected to be nominal")
  expect_null(result2)
})

test_that("check_nominal_station returns coordinates only when suspected nominal", {
  # Case where positions are NOT suspected
  data_nominal <- dplyr::tibble(
    sample_date = c("2025-09-17", "2025-09-18"),
    station_name = c("KA6", "KA6"),
    sample_latitude_dd = c(56.6, 56.6),
    sample_longitude_dd = c(12.6, 12.6)
  )

  expect_message(result3 <- check_nominal_station(data_nominal),
                 "Suspected nominal positions reported! Is this correct?")
  expect_true(is.data.frame(result3))
  expect_equal(nrow(result3), 1)
})

test_that("match_station identifies matched and unmatched stations", {
  station_names <- c("N7 OST NIDINGEN", "NonexistentStation")

  expect_message(match_station(station_names), "WARNING: Unmatched stations found")
})

test_that("match_station finds all stations without unmatched", {
  station_names <- c("N7 OST NIDINGEN", "KA6")

  expect_message(match_station(station_names), "All stations found")
})

test_that("match_station finds all stations with synonym matching", {
  station_names <- c("BY5", "FLADEN1")

  expect_message(match_station(station_names), "All stations found")
})

test_that("deprecated nominal_station returns coordinates only when suspected nominal", {
  # Case where positions are NOT suspected
  data_ok <- dplyr::tibble(
    sample_date = c("2025-09-17", "2025-09-18"),
    station_name = c("N7 OST NIDINGEN", "KA6"),
    sample_latitude_dd = c(57.3, 56.6),
    sample_longitude_dd = c(12.0, 12.6)
  )

  lifecycle::expect_deprecated(nominal_station(data_ok))
})

test_that("match_station reads a station_file correctly", {

  # Create a temporary station file
  tmp_file <- tempfile(fileext = ".txt")

  # Write a small example station table
  station_data <- data.frame(
    STATION_NAME = c("ST1", "ST2", "ST3"),
    LATITUDE = c(58.5, 58.6, 58.7),
    LONGITUDE = c(15.0, 16.0, 17.0)
  )

  readr::write_delim(station_data, tmp_file, delim = "\t")

  # Test input: some stations match, some don't
  stations_to_check <- c("ST1", "STX")

  # Run match_station with custom station file
  result <- match_station(stations_to_check, station_file = tmp_file)

  # Expect a data frame with correct match_type
  expect_s3_class(result, "data.frame")
  expect_equal(result$reported_station_name, stations_to_check)
  expect_equal(result$match_type, c(TRUE, FALSE))

  # Clean up
  unlink(tmp_file)
})

test_that("match_station reads a station_file correctly", {

  # Create a temporary station file
  tmp_file <- tempfile(fileext = ".txt")

  # Write a small example station table
  station_data <- data.frame(
    STATION_NAME = c("ST1", "ST2", "ST3"),
    LATITUDE = c(58.5, 58.6, 58.7),
    LONGITUDE = c(15.0, 16.0, 17.0)
  )

  readr::write_delim(station_data, tmp_file, delim = "\t")

  # Test input: some stations match, some don't
  stations_to_check <- c("ST1", "STX")

  # Run match_station with custom station file
  result <- match_station(stations_to_check, station_file = tmp_file)

  # Expect a data frame with correct match_type
  expect_s3_class(result, "data.frame")
  expect_equal(result$reported_station_name, stations_to_check)
  expect_equal(result$match_type, c(TRUE, FALSE))

  # Clean up
  unlink(tmp_file)
})

# Create a small mock station_db
mock_station_db <- dplyr::tibble(
  STATION_NAME = c("ST1", "ST2"),
  LATITUDE_WGS84_SWEREF99_DD = c(58.5, 58.6),
  LONGITUDE_WGS84_SWEREF99_DD = c(15.0, 16.0),
  OUT_OF_BOUNDS_RADIUS = c(1000, 1000), # meters
  SYNONYM_NAMES = c("Station One", "Station Two")
)

# Helper function to write a temp station file
write_temp_station_file <- function(db) {
  tmp <- tempfile(fileext = ".txt")
  readr::write_delim(db, tmp, delim = "\t")
  tmp
}

# ---- Tests ----
test_that("basic functionality: matches within distance", {
  df <- tibble(
    station_name = c("ST1", "ST2"),
    sample_longitude_dd = c(15.0, 16.0),
    sample_latitude_dd  = c(58.5, 58.6)
  )

  tmp_file <- write_temp_station_file(mock_station_db)

  res <- check_station_distance(df,
                                station_file = tmp_file,
                                plot_leaflet = FALSE)

  # expect_true(all(res$match_type))
  expect_true(all(res$within_limit))
  expect_true(all(res$distance_m == 0))

  # Clean up
  unlink(tmp_file)
})

test_that("basic functionality: matches within distance using station.txt bundle", {
  df <- tibble(
    station_name = c("N7 OST NIDINGEN", "KA6"),
    sample_longitude_dd = c(15.0, 16.0),
    sample_latitude_dd  = c(58.5, 58.6)
  )

  res <- check_station_distance(df,
                                plot_leaflet = FALSE)

  # expect_true(all(res$match_type))
  expect_true(!all(res$within_limit))
  expect_true(all(res$distance_m > 0))
})

test_that("basic functionality: matches within synonyms", {
  df <- tibble(
    station_name = c("Station One", "Station Two"),
    sample_longitude_dd = c(15.0, 16.0),
    sample_latitude_dd  = c(58.5, 58.6)
  )

  tmp_file <- write_temp_station_file(mock_station_db)

  res <- check_station_distance(df,
                                station_file = tmp_file,
                                plot_leaflet = FALSE)

  # expect_true(all(res$match_type))
  expect_true(all(res$within_limit))
  expect_true(all(res$distance_m == 0))

  # Clean up
  unlink(tmp_file)
})

test_that("unmatched stations trigger warnings", {
  df <- tibble(
    station_name = c("ST1", "STX"),
    sample_longitude_dd = c(15.0, 16.0),
    sample_latitude_dd  = c(58.5, 58.6)
  )

  tmp_file <- write_temp_station_file(mock_station_db)

  expect_message(res <- check_station_distance(df,
                                               station_file = tmp_file,
                                               plot_leaflet = FALSE),
                 "Unmatched stations found")

  # expect_true(res$match_type[1])
  # expect_false(res$match_type[2])
  expect_true(is.na(res$distance_m[2]))
  expect_true(is.na(res$within_limit[2]))

  # Clean up
  unlink(tmp_file)
})

test_that("stations outside distance limit trigger warnings", {
  df <- tibble(
    station_name = c("ST1", "ST2"),
    sample_longitude_dd = c(15.02, 16.05),
    sample_latitude_dd  = c(58.52, 58.65)
  )

  tmp_file <- write_temp_station_file(mock_station_db)

  expect_message(res <- check_station_distance(df,
                                               station_file = tmp_file,
                                               plot_leaflet = FALSE),
                 "outside the allowed distance limit")

  expect_false(all(res$within_limit))

  # Clean up
  unlink(tmp_file)
})

test_that("missing required columns in data triggers error", {
  df <- tibble(
    bad_name = c("ST1"),
    sample_longitude_dd = 15.0,
    sample_latitude_dd = 58.5
  )

  tmp_file <- write_temp_station_file(mock_station_db)

  expect_error(check_station_distance(df,
                                      station_file = tmp_file,
                                      plot_leaflet = FALSE),
               "Missing required column\\(s\\) in input data")

  # Clean up
  unlink(tmp_file)
})

test_that("missing required columns in station_db triggers error", {
  broken_db <- mock_station_db %>% select(-OUT_OF_BOUNDS_RADIUS)
  tmp_file <- write_temp_station_file(broken_db)

  df <- tibble(
    station_name = "ST1",
    sample_longitude_dd = 15.0,
    sample_latitude_dd = 58.5
  )

  expect_error(check_station_distance(df, station_file = tmp_file),
               "Missing required column\\(s\\) in station database")

  # Clean up
  unlink(tmp_file)
})

test_that("empty input returns empty results", {
  df <- tibble(station_name = character(),
               sample_longitude_dd = numeric(),
               sample_latitude_dd = numeric())

  res <- check_station_distance(df, plot_leaflet = FALSE)
  expect_equal(nrow(res), 0)
})

test_that("plot_leaflet = TRUE returns a leaflet object", {
  # Mock station_db
  station_db <- dplyr::tibble(
    STATION_NAME = c("ST1", "ST2"),
    LATITUDE_WGS84_SWEREF99_DD = c(58.5, 58.6),
    LONGITUDE_WGS84_SWEREF99_DD = c(15.0, 16.0),
    OUT_OF_BOUNDS_RADIUS = c(1000, 1000),
    SYNONYM_NAMES = c("Station One", "Station Two")
  )

  # Temp file for station database
  tmp_file <- tempfile(fileext = ".txt")
  readr::write_delim(station_db, tmp_file, delim = "\t")

  # Reported stations
  df <- tibble(
    station_name = c("ST1", "ST2", "FAKE"),
    sample_longitude_dd = c(15.0, 16.05, 16.5),
    sample_latitude_dd  = c(58.5, 58.65, 58.7)
  )

  # Expect no error and return a leaflet map when plot_leaflet = TRUE
  m <- check_station_distance(df,
                              station_file = tmp_file,
                              plot_leaflet = TRUE,
                              only_bad = FALSE)

  # Expect no error and return a leaflet map when plot_leaflet = TRUE and only_bad = TRUE
  m2 <- check_station_distance(df,
                               station_file = tmp_file,
                               plot_leaflet = TRUE,
                               only_bad = TRUE)



  # Class check
  expect_s3_class(m, "leaflet")
  expect_s3_class(m, "htmlwidget")
  expect_s3_class(m2, "leaflet")
  expect_s3_class(m2, "htmlwidget")

  # Provider check
  expect_true(any(grepl("CartoDB.Positron", m$x$calls[[1]]$args[[1]])))
  expect_true(any(grepl("CartoDB.Positron", m2$x$calls[[1]]$args[[1]])))

  # Extract lat ranges
  m_lat <- m$x$limits$lat
  m2_lat <- m2$x$limits$lat

  # Expect different limits when only_bad = TRUE
  expect_true(m_lat[1] != m2_lat[1])

  # Clean up
  unlink(tmp_file)
})

test_that("match_station reads a bundled station_file correctly", {
  # Clear NODC_CONFIG to avoid interference
  Sys.setenv("NODC_CONFIG" = "")

  # Run match_station with custom station file
  result <- expect_message(match_station("N7 OST NIDINGEN"), "Using station.txt from SHARK4R bundle")
})

test_that("match_station reads a NODC_CONFIG station_file correctly", {

  tempdir <- tempdir()

  # Clear NODC_CONFIG to avoid interference
  Sys.setenv("NODC_CONFIG" = tempdir)

  # Create a temporary station file
  tmp_file <- file.path(tempdir, "station.txt")

  # Write a small example station table
  station_data <- data.frame(
    STATION_NAME = c("ST1", "ST2", "ST3"),
    LATITUDE = c(58.5, 58.6, 58.7),
    LONGITUDE = c(15.0, 16.0, 17.0)
  )

  readr::write_delim(station_data, tmp_file, delim = "\t")

  # Test input: some stations match, some don't
  stations_to_check <- c("ST1", "STX")

  # Run match_station with custom station file
  expect_message(match_station(stations_to_check), "NODC_CONFIG")

  # Clean up
  unlink(tmp_file)

  # Clear NODC_CONFIG to avoid interference
  Sys.setenv("NODC_CONFIG" = "")
})

test_that("check_station_distance reads a NODC_CONFIG station_file correctly", {

  tempdir <- tempdir()

  # Clear NODC_CONFIG to avoid interference
  Sys.setenv("NODC_CONFIG" = tempdir)

  df <- dplyr::tibble(
    station_name = c("ST1", "ST2"),
    sample_longitude_dd = c(15.0, 16.0),
    sample_latitude_dd  = c(58.5, 58.6)
  )

  # Create a temporary station file
  tmp_file <- file.path(tempdir, "station.txt")

  readr::write_delim(mock_station_db, tmp_file, delim = "\t")

  expect_message(check_station_distance(df,
                                        plot_leaflet = FALSE), "NODC_CONFIG")

  # Clean up
  unlink(tmp_file)

  # Clear NODC_CONFIG to avoid interference
  Sys.setenv("NODC_CONFIG" = "")
})
