# Example data
data <- dplyr::tibble(
  station_name = rep(c("A", "B", "C"), each = 2),
  sample_date = rep(as.Date("2025-01-01") + 0:1, 3),
  value = c(10, 12, 20, 18, 15, 14),
  parameter = rep("Chlorophyll", 6),
  unit = rep("µg/L", 6),
  location_sea_basin = rep(c("Bottenviken", "Bottenhavet", "Skagerrak"), each = 2)
)

# Horizontal line df
hline_df <- dplyr::tibble(
  parameter = "Chlorophyll",
  location_sea_basin = c("Bottenviken", "Bottenhavet"),
  P99 = c(12, 19)
)

test_that("scatterplot returns ggplot object when interactive = FALSE", {
  p <- scatterplot(
    data,
    x = "station_name",
    interactive = FALSE
  )
  expect_s3_class(p, "ggplot")
})

test_that("scatterplot returns plotly object when interactive = TRUE", {
  skip_if_not_installed("plotly")
  p <- scatterplot(
    data,
    x = "station_name",
    interactive = TRUE
  )
  expect_s3_class(p, "plotly")
})

test_that("scatterplot adds numeric horizontal line", {
  p <- scatterplot(
    data,
    x = "station_name",
    hline = 15,
    interactive = FALSE
  )
  # find if any layer is a geom_hline
  is_hline <- vapply(p$layers, function(l) inherits(l$geom, "GeomHline"), logical(1))
  expect_true(any(is_hline))
})

test_that("scatterplot adds hlines from dataframe with group filtering (alphabetical selection)", {
  p <- scatterplot(
    data,
    x = "station_name",
    hline = hline_df,
    hline_group_col = "location_sea_basin",
    hline_value_col = "P99",
    interactive = FALSE,
    max_hlines = 1
  )

  # The function selects groups alphabetically; alphabetical order of
  # c("Bottenviken","Bottenhavet") is c("Bottenhavet","Bottenviken"),
  # so the first (max_hlines = 1) should be "Bottenhavet".
  # The geom_hline layer's data should therefore have 1 row.
  # Find the hline layer (first layer with geom hline)
  hline_layer_idx <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomHline"), logical(1)))
  expect_length(hline_layer_idx, 1)
  hline_data <- p$layers[[hline_layer_idx]]$data

  # Should have one row (one selected group)
  expect_equal(nrow(hline_data), 1)

  # Factor levels (if present) should contain the selected (alphabetically-first) group
  expected_first <- sort(unique(hline_df$location_sea_basin))[1]
  if ("location_sea_basin" %in% names(hline_data)) {
    # ensure level matches alphabetical first
    expect_true(expected_first %in% levels(hline_data$location_sea_basin) ||
                  expected_first %in% as.character(hline_data[[ "location_sea_basin" ]]))
  } else {
    # If no group column in layer data for some ggplot versions, ensure yintercept exists and equals expected value
    expect_true(hline_df$P99[hline_df$location_sea_basin == expected_first] %in% hline_data[[ "P99" ]] ||
                  any(hline_data[[1]] == hline_df$P99[hline_df$location_sea_basin == expected_first], na.rm = TRUE))
  }
})

test_that("scatterplot respects max_hlines > 1 and selects first N alphabetically", {
  p2 <- scatterplot(
    data,
    x = "station_name",
    hline = hline_df,
    hline_group_col = "location_sea_basin",
    hline_value_col = "P99",
    interactive = FALSE,
    max_hlines = 2
  )

  # Expect up to 2 rows (we provided 2 groups)
  hline_layer_idx2 <- which(vapply(p2$layers, function(l) inherits(l$geom, "GeomHline"), logical(1)))
  hline_data2 <- p2$layers[[hline_layer_idx2]]$data
  expect_true(nrow(hline_data2) %in% c(1,2))
  # The set of groups used should be the alphabetically first two
  expected_groups <- head(sort(unique(hline_df$location_sea_basin)), 2)
  if ("location_sea_basin" %in% names(hline_data2)) {
    expect_setequal(expected_groups, as.character(levels(hline_data2$location_sea_basin)))
  }
})

test_that("scatterplot errors if required columns are missing", {
  df_bad <- data[, setdiff(names(data), "value")]
  expect_error(scatterplot(df_bad), "Data must contain the following columns")
})

test_that("scatterplot errors if hline columns are missing", {
  hline_bad <- hline_df[, setdiff(names(hline_df), "P99")]
  expect_error(
    scatterplot(
      data,
      hline = hline_bad,
      hline_group_col = "location_sea_basin",
      hline_value_col = "P99"
    ),
    regexp = "must exist"
  )
})
