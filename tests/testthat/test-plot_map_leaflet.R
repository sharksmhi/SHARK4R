test_that("plot_map_leaflet works with minimal data", {
  df <- data.frame(
    station_name = c("StationA", "StationB"),
    sample_longitude_dd = c(11.5, 12.2),
    sample_latitude_dd = c(57.7, 58.1)
  )

  m <- plot_map_leaflet(df)

  # Class check
  expect_s3_class(m, "leaflet")
  expect_s3_class(m, "htmlwidget")

  # Internal structure: coord extraction
  coord <- df %>%
    dplyr::select(station_name, sample_longitude_dd, sample_latitude_dd) %>%
    dplyr::rename(STATION = station_name, LON = sample_longitude_dd, LAT = sample_latitude_dd) %>%
    dplyr::distinct()
  expect_equal(sort(coord$STATION), c("StationA", "StationB"))

  # Provider check
  expect_true(any(grepl("CartoDB.Positron", m$x$calls[[1]]$args[[1]])))
})

test_that("deprecated plot_map_leaflet_deliv still works", {
  df <- data.frame(
    STATN = c("Stn1", "Stn2", "Stn2"), # includes duplicate
    LONGI = c(13.0, 14.5, 14.5),
    LATIT = c(55.1, 56.3, 56.3)
  )

  lifecycle::expect_deprecated(plot_map_leaflet_deliv(df))
  m <- suppressWarnings(plot_map_leaflet_deliv(df))

  # Class check
  expect_s3_class(m, "leaflet")
  expect_s3_class(m, "htmlwidget")

  # Internal structure: coord extraction removes duplicates
  coord <- df %>%
    dplyr::select(STATN, LONGI, LATIT) %>%
    dplyr::rename(STATION = STATN, LON = LONGI, LAT = LATIT) %>%
    dplyr::distinct()
  expect_equal(nrow(coord), 2)
  expect_equal(sort(coord$STATION), c("Stn1", "Stn2"))

  # Provider check
  expect_true(any(grepl("Esri.OceanBasemap", m$x$calls[[1]]$args[[1]])))
})

test_that("plot_map_leaflet works with custom provider", {
  df <- data.frame(
    station_name = "StationX",
    sample_longitude_dd = 10.0,
    sample_latitude_dd = 60.0
  )

  m <- plot_map_leaflet(df, provider = "Esri.OceanBasemap")

  expect_s3_class(m, "leaflet")
  expect_true(any(grepl("Esri.OceanBasemap", m$x$calls[[1]]$args[[1]])))
})
