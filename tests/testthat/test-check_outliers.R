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

# Define any manual fallback thresholds (add as needed)
manual_map <- list(
  bacterial_production                = 1200706084,
  bacterial_concentration             = 6779382000,
  bacterial_carbon                    = 20.76,
  chlorophyll_conc                    = 9.4,
  picoplankton_abundance              = 133564616,
  picoplankton_biovol                 = 0.09323008,
  picoplankton_carbon                 = 20.85692,
  picoplankton_counted                = 733,
  zooplankton_abund                   = 1731.232,
  zooplankton_counted                 = 86,
  zooplankton_length_mean             = 1898.325,
  zooplankton_length_median           = 1899,
  zooplankton_wetweight               = 1.3,
  zooplankton_carbon                  = 6.16,
  zooplankton_wetweight_volume        = 15.54263,
  zooplankton_wetweight_area          = 593.9886,
  phytoplankton_abund                 = 62920,
  phytoplankton_biovol                = 0.02397705,
  phytoplankton_carbon                = 2.653602,
  phytoplankton_counted               = 82,
  primaryproduction_carbonprod        = 58.41079,
  primaryproduction_carbonprodlight   = 58.41079,
  primaryproduction_carbonprod_hour   = 18.6775,
  epibenthos_counted                  = 138,
  epibenthos_dryweight                = 0.367895,
  epibenthos_specdistr_maxdepth       = 44.425,
  epibenthos_specdistr_mindepth       = 20.65,
  harbourseal_counted                 = 260,
  greyseal_counted                    = 632,
  zoobenthos_BQIm                     = 26.96423,
  zoobenthos_abund                    = 290,
  zoobenthos_counted                  = 33,
  zoobenthos_wetweight                = 0.859,
  ringedseal_calccounted              = 41.6792,
  harbporp_positivemin                = 299
)

manual_param_map <- list(
  bacterial_production                = "Bacterial production",
  bacterial_concentration             = "Bacterial concentration",
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

  test_that(paste0(fname, " returns datatable and message when extreme outlier present"), {
    extreme_threshold <- get_extreme_threshold(fn, fname, manual_map = manual_map)
    if (is.null(extreme_threshold) || !is.numeric(extreme_threshold)) {
      skip(paste("No numeric extreme threshold for", fname))
    }
    param_string <- get_parameter_string(fn, fname, manual_map = manual_param_map)

    # Build a small tibble with the columns these functions select/expect
    df_trigger <- dplyr::tibble(
      station_name = "S1",
      sample_date = as.Date("2020-01-01"),
      sample_id = "id1",
      shark_sample_id_md5 = "md5_1",
      sample_min_depth_m = 1,
      sample_max_depth_m = 2,
      parameter = param_string,
      # make value exceed the extreme threshold by 1 (works for numeric thresholds)
      value = as.numeric(extreme_threshold) + 1
    )

    # Expect a WARNING message and that the function returns a datatable object (htmlwidget)
    expect_message(
      out <- fn(df_trigger),
      regexp = "WARNING",
      info = paste("function:", fname)
    )

    # Returned value must be a DT htmlwidget (non-NULL)
    expect_true(!is.null(out), info = paste("function should return a datatable when outlier present:", fname))
    expect_true(inherits(out, "htmlwidget") || inherits(out, "datatables"),
                info = paste("expected returned object to be a DT htmlwidget for", fname))
  })

  test_that(paste0(fname, " returns NULL and 'within range' message when no extreme outlier present"), {
    extreme_threshold <- get_extreme_threshold(fn, fname, manual_map = manual_map)
    if (is.null(extreme_threshold) || !is.numeric(extreme_threshold)) {
      skip(paste("No numeric extreme threshold for", fname))
    }
    param_string <- get_parameter_string(fn, fname, manual_map = manual_param_map)

    df_ok <- dplyr::tibble(
      station_name = "S2",
      sample_date = as.Date("2020-02-02"),
      sample_id = "id2",
      shark_sample_id_md5 = "md5_2",
      sample_min_depth_m = 1,
      sample_max_depth_m = 2,
      parameter = param_string,
      value = as.numeric(extreme_threshold) - 1
    )

    expect_message(
      out2 <- fn(df_ok),
      regexp = "within range",
      info = paste("function:", fname)
    )

    expect_true(is.null(out2), info = paste("function should return NULL when no outlier present:", fname))
  })
}
