# Sample thresholds
test_thresholds <- dplyr::tibble(
  parameter = c("TestParam1", "TestParam2"),
  datatype = c("TypeA", "TypeA"),
  extreme_upper = c(10, 100),
  mild_upper = c(5, 50)
)

# Sample data
test_data <- dplyr::tibble(
  station_name = c("S1", "S2", "S3"),
  sample_date = as.Date(c("2025-01-01", "2025-01-02", "2025-01-03")),
  sample_id = 1:3,
  shark_sample_id_md5 = letters[1:3],
  sample_min_depth_m = c(0, 5, 10),
  sample_max_depth_m = c(1, 6, 11),
  parameter = c("TestParam1", "TestParam1", "TestParam1"),
  value = c(8, 12, 9),
  delivery_datatype = c("TypeA", "TypeA", "TypeA")
)

test_that("check_outliers returns datatable for outliers", {
  out <- check_outliers(
    data = test_data,
    parameter = "TestParam1",
    datatype = "TypeA",
    threshold_col = "extreme_upper",
    thresholds = test_thresholds
  )

  expect_s3_class(out, "datatables")
  expect_equal(nrow(out$x$data), 1) # Only the second row exceeds threshold
  expect_equal(out$x$data$value, 12)
})

test_that("check_outliers prints message if no outliers", {
  data_no_outliers <- test_data %>% mutate(value = value - 10)

  expect_message(
    check_outliers(
      data = data_no_outliers,
      parameter = "TestParam1",
      datatype = "TypeA",
      threshold_col = "extreme_upper",
      thresholds = test_thresholds
    ),
    "is within the extreme_upper range"
  )
})

test_that("check_outliers errors if threshold_col is missing", {
  expect_warning(
    check_outliers(
      data = test_data,
      parameter = "TestParam1",
      datatype = "TypeA",
      threshold_col = "nonexistent_col",
      thresholds = test_thresholds
    ),
    "Column nonexistent_col not found in thresholds dataframe"
  )
})

test_that("check_outliers errors if parameter not in thresholds", {
  expect_warning(
    check_outliers(
      data = test_data,
      parameter = "NonexistentParam",
      datatype = "TypeA",
      threshold_col = "extreme_upper",
      thresholds = test_thresholds
    ),
    "No thresholds found for NonexistentParam and TypeA"
  )
})

test_that("check_outliers only filters by datatype", {
  data_mixed <- test_data %>%
    dplyr::add_row(
      station_name = "S4",
      sample_date = as.Date("2025-01-04"),
      sample_id = 4,
      shark_sample_id_md5 = "d",
      sample_min_depth_m = 2,
      sample_max_depth_m = 3,
      parameter = "TestParam1",
      value = 200,
      delivery_datatype = "TypeB"
    )

  out <- check_outliers(
    data = data_mixed,
    parameter = "TestParam1",
    datatype = "TypeA",
    threshold_col = "extreme_upper",
    thresholds = test_thresholds
  )

  # Only TypeA row above threshold should be flagged
  expect_equal(nrow(out$x$data), 1)
  expect_equal(out$x$data$delivery_datatype, "TypeA")
})


# List the outlier-check functions you want to test:
outlier_checks <- list(
  bacterial_production               = check_bacterial_production,
  bacterial_concentration            = check_bacterial_concentration,
  bacterial_carbon                   = check_bacterial_carbon,
  chlorophyll_conc                   = check_chlorophyll_conc,
  picoplankton_abundance             = check_picoplankton_abundance,
  picoplankton_biovol                = check_picoplankton_biovol,
  picoplankton_carbon                = check_picoplankton_carbon,
  picoplankton_counted               = check_picoplankton_counted,
  zooplankton_abund                  = check_zooplankton_abund,
  zooplankton_counted                = check_zooplankton_counted,
  zooplankton_length_mean            = check_zooplankton_length_mean,
  zooplankton_length_median          = check_zooplankton_length_median,
  zooplankton_wetweight              = check_zooplankton_wetweight,
  zooplankton_carbon                 = check_zooplankton_carbon,
  zooplankton_wetweight_volume       = check_zooplankton_wetweight_volume,
  zooplankton_wetweight_area         = check_zooplankton_wetweight_area,
  phytoplankton_abund                = check_phytoplankton_abund,
  phytoplankton_biovol               = check_phytoplankton_biovol,
  phytoplankton_carbon               = check_phytoplankton_carbon,
  phytoplankton_counted              = check_phytoplankton_counted,
  primaryproduction_carbonprod       = check_primaryproduction_carbonprod,
  primaryproduction_carbonprodlight  = check_primaryproduction_carbonprodlight,
  primaryproduction_carbonprod_hour  = check_primaryproduction_carbonprod_hour,
  epibenthos_counted                 = check_epibenthos_counted,
  epibenthos_dryweight               = check_epibenthos_dryweight,
  epibenthos_specdistr_maxdepth      = check_epibenthos_specdistr_maxdepth,
  epibenthos_specdistr_mindepth      = check_epibenthos_specdistr_mindepth,
  harbourseal_counted                = check_harbourseal_counted,
  greyseal_counted                   = check_greyseal_counted,
  zoobenthos_BQIm                    = check_zoobenthos_BQIm,
  zoobenthos_abund                   = check_zoobenthos_abund,
  zoobenthos_counted                 = check_zoobenthos_counted,
  zoobenthos_wetweight               = check_zoobenthos_wetweight,
  ringedseal_calccounted             = check_ringedseal_calccounted,
  harbporp_positivemin               = check_harbporp_positivemin
)

manual_param_map <- list(
  bacterial_production                = "Bacterial carbon production",
  bacterial_concentration             = "Bacterial abundance",
  bacterial_carbon                    = "Bacterial cell carbon content",
  chlorophyll_conc                    = "Chlorophyll-a",
  picoplankton_abundance              = "Abundance",
  picoplankton_biovol                 = "Biovolume concentration",
  picoplankton_carbon                 = "Carbon concentration",
  picoplankton_counted                = "# counted",
  zooplankton_abund                   = "Abundance",
  zooplankton_counted                 = "# counted",
  zooplankton_length_mean             = "Length (mean)",
  zooplankton_length_median           = "Length (median)",
  zooplankton_wetweight               = "Wet weight",
  zooplankton_carbon                  = "Carbon content",
  zooplankton_wetweight_volume        = "Wet weight/volume",
  zooplankton_wetweight_area          = "Wet weight/area",
  phytoplankton_abund                 = "Abundance",
  phytoplankton_biovol                = "Biovolume concentration",
  phytoplankton_carbon                = "Carbon concentration",
  phytoplankton_counted               = "# counted",
  primaryproduction_carbonprod        = "Carbon production",
  primaryproduction_carbonprodlight   = "Carbon prod in light",
  primaryproduction_carbonprod_hour   = "Carbon production/hour",
  epibenthos_counted                  = "# counted",
  epibenthos_dryweight                = "Dry weight",
  epibenthos_specdistr_maxdepth       = "Species distribution max depth",
  epibenthos_specdistr_mindepth       = "Species distribution min depth",
  harbourseal_counted                 = "# counted",
  greyseal_counted                    = "# counted",
  zoobenthos_BQIm                     = "BQIm",
  zoobenthos_abund                    = "Abundance",
  zoobenthos_counted                  = "# counted",
  zoobenthos_wetweight                = "Wet weight",
  ringedseal_calccounted              = "Calculated # counted",
  harbporp_positivemin                = "Porpoise positive minutes"
)

# Run tests for each function
for (fname in names(outlier_checks)) {
  fn <- outlier_checks[[fname]]

  print(fname) # for debugging

  test_that(paste0("deprecated ", fname, " function still works"), {
    # extreme_threshold <- get_extreme_threshold(fn, fname, manual_map = manual_map)
    extreme_threshold <- .threshold_values$extreme_upper[.threshold_values$parameter == manual_param_map[fname][[1]]]
    if (is.null(extreme_threshold) || !is.numeric(extreme_threshold)) {
      skip(paste("No numeric extreme threshold for", fname))
    }
    # param_string <- get_parameter_string(fn, fname, manual_map = manual_param_map)
    param_string <- .threshold_values$parameter[.threshold_values$parameter == manual_param_map[fname][[1]]]
    datatype_string <- .threshold_values$datatype[.threshold_values$parameter == manual_param_map[fname][[1]]]

    # Build a small tibble with the columns these functions select/expect
    df_trigger <- dplyr::tibble(
      station_name = "S1",
      sample_date = as.Date("2020-01-01"),
      sample_id = "id1",
      shark_sample_id_md5 = "md5_1",
      sample_min_depth_m = 1,
      sample_max_depth_m = 2,
      parameter = param_string,
      delivery_datatype = datatype_string,
      # make value exceed the extreme threshold by 1 (works for numeric thresholds)
      value = as.numeric(extreme_threshold) + 1
    )

    # Expect deprecated warning
    lifecycle::expect_deprecated(
      out <- fn(df_trigger)
    )
  })
}

test_that("check_outliers stops if custom_group missing from data", {
  expect_error(
    check_outliers(
      data = test_data,
      parameter = "TestParam1",
      datatype = "TypeA",
      thresholds = test_thresholds,
      custom_group = "nonexistent_column"
    ),
    "Grouping column nonexistent_column not found in data"
  )
})

test_that("check_outliers works with custom_group", {
  # Add a grouping column to data and thresholds
  data_grouped <- test_data %>% mutate(group = c("A", "A", "B"))
  thresholds_grouped <- test_thresholds %>% mutate(group = c("A", "B"))

  out <- check_outliers(
    data = data_grouped,
    parameter = "TestParam1",
    datatype = "TypeA",
    thresholds = thresholds_grouped,
    threshold_col = "extreme_upper",
    custom_group = "group",
    return_df = TRUE
  )

  # Only the second row in group A exceeds threshold
  expect_equal(nrow(out), 1)
  expect_equal(out$value, 12)
})

test_that("check_outliers returns data.frame if return_df = TRUE", {
  out <- check_outliers(
    data = test_data,
    parameter = "TestParam1",
    datatype = "TypeA",
    threshold_col = "extreme_upper",
    thresholds = test_thresholds,
    return_df = TRUE
  )

  expect_s3_class(out, "data.frame")
})
