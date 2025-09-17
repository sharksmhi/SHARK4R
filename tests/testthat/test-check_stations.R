test_that("nominal_station returns coordinates only when suspected nominal", {
  # Case where positions are NOT suspected
  data_ok <- dplyr::tibble(
    sample_date = c("2025-09-17", "2025-09-18"),
    station_name = c("N7 OST NIDINGEN", "KA6"),
    sample_latitude_dd = c(57.3, 56.6),
    sample_longitude_dd = c(12.0, 12.6)
  )

  expect_message(result2 <- nominal_station(data_ok),
                 "Positions are not suspected to be nominal")
  expect_null(result2)
})

test_that("match_station identifies matched and unmatched stations", {
  station_names <- c("N7 OST NIDINGEN", "NonexistentStation")

  expect_message(match_station(station_names), "WARNING: Unmatched stations found")
})

test_that("match_station finds all stations without unmatched", {
  station_names <- c("N7 OST NIDINGEN", "KA6")

  expect_message(match_station(station_names), "All stations found")
})

test_that("check_station_distance identifies unmatched stations", {
  station_names <- c("N7 OST NIDINGEN", "NonexistentStation")

  expect_message(check_station_distance(station_names), "WARNING: Unmatched stations found")
})

test_that("check_station_distance warns for stations outside distance", {
  station_names <- c("N7 OST NIDINGEN", "KA6")

  expect_message(check_station_distance(station_names), "WARNING: Matched stations found but they are outside distance limit")
})
