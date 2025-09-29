test_that("scatterplot returns a valid plotly object", {
  df <- data.frame(
    station_name = c("Stn1", "Stn2"),
    value = c(10, 20),
    parameter = "Param",
    unit = "mg/L",
    sample_date = as.Date(c("2020-01-01", "2020-01-02"))
  )

  p <- scatterplot(df, hline = 15)

  # Class check
  expect_s3_class(p, "plotly")
  expect_s3_class(p, "htmlwidget")

  # Get the unique plot ID
  id <- p$x$cur_data[[1]]

  # Check trace attributes
  trace <- p$x$attrs[[id]]
  expect_equal(trace$type, "scatter")
  expect_equal(trace$mode, "markers")
  expect_equal(as.character(trace$x)[2], "station_name")
  expect_equal(as.character(trace$y)[2], "value")
  expect_true(trace$visible)

  # Check layout updatemenus
  um <- p$x$layoutAttrs[[id]]$updatemenus
  expect_equal(length(um), 3)

  # Y-axis menu
  expect_equal(um[[1]]$buttons[[1]]$label, "Param")
  expect_equal(names(um[[1]]$buttons[[1]]$args[[1]]), "visible")

  # X-axis menu
  expect_equal(um[[2]]$buttons[[1]]$label, "X = STATION_NAME")
  expect_equal(um[[2]]$buttons[[2]]$label, "X = SAMPLE_DATE")
})

test_that("scatterplot throws an error if columns are missing", {
  df2 <- data.frame(
    station_name = c("Stn1", "Stn2"),
    value = c(10, 20),
    sample_date = as.Date(c("2020-01-01", "2020-01-02"))
  )

  expect_error(scatterplot(df2))
})

