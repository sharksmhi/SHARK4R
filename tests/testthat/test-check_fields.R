test_that("check_datatype returns empty tibble when all required and recommended fields are present", {
  data <- dplyr::tibble(
    visit_year = 2024,
    station_name = "Station A",
    sample_project_name_en = "Project",
    sample_orderer_name_en = "Orderer",
    platform_code = "PLAT",
    sample_date = as.Date("2024-01-01"),
    sample_latitude_dd = 56.0,
    sample_longitude_dd = 12.0,
    positioning_system_code = "GPS",
    water_depth_m = 10,
    monitoring_station_type_code = "TYPE",
    monitoring_purpose_code = "PURP",
    monitoring_program_code = "PROG",
    reporting_institute_name_en = "Institute",
    analytical_laboratory_name_en = "Lab"
  )

  result <- check_datatype(data, level = "warning")
  expect_equal(nrow(result), 0)
})

test_that("check_datatype detects missing required fields", {
  data <- dplyr::tibble(
    station_name = "Station A"
  )

  result <- check_datatype(data, level = "error")
  expect_true("visit_year" %in% result$field)
  expect_true(all(result$level == "error"))
})

test_that("check_datatype detects empty values in required fields", {
  data <- dplyr::tibble(
    visit_year = c(2024, NA),
    station_name = c("Station A", "Station B"),
    sample_project_name_en = c("Proj", "Proj"),
    sample_orderer_name_en = c("Ord", "Ord"),
    platform_code = c("PLAT", "PLAT"),
    sample_date = as.Date(c("2024-01-01", "2024-01-02")),
    sample_latitude_dd = c(56.0, 57.0),
    sample_longitude_dd = c(12.0, 13.0),
    positioning_system_code = c("GPS", "GPS"),
    water_depth_m = c(10, 20)
  )

  result <- check_datatype(data, level = "error")
  expect_true(any(result$field == "visit_year"))
  expect_true(any(result$row == 2))
  expect_true(all(result$level == "error"))
})

test_that("check_datatype ignores recommended fields when level = 'error'", {
  data <- dplyr::tibble(
    visit_year = 2024,
    station_name = "Station A",
    sample_project_name_en = "Proj",
    sample_orderer_name_en = "Ord",
    platform_code = "PLAT",
    sample_date = as.Date("2024-01-01"),
    sample_latitude_dd = 56.0,
    sample_longitude_dd = 12.0,
    positioning_system_code = "GPS",
    water_depth_m = 10
  )

  result <- check_datatype(data, level = "error")
  expect_equal(nrow(result), 0)  # no warnings for recommended
})

test_that("check_datatype detects missing recommended fields when level = 'warning'", {
  data <- dplyr::tibble(
    visit_year = 2024,
    station_name = "Station A",
    sample_project_name_en = "Proj",
    sample_orderer_name_en = "Ord",
    platform_code = "PLAT",
    sample_date = as.Date("2024-01-01"),
    sample_latitude_dd = 56.0,
    sample_longitude_dd = 12.0,
    positioning_system_code = "GPS",
    water_depth_m = 10
  )

  result <- check_datatype(data, level = "warning")
  expect_true("monitoring_station_type_code" %in% result$field)
  expect_true(any(result$level == "warning"))
})

test_that("check_datatype detects empty values in recommended fields when level = 'warning'", {
  data <- dplyr::tibble(
    visit_year = 2024,
    station_name = "Station A",
    sample_project_name_en = "Proj",
    sample_orderer_name_en = "Ord",
    platform_code = "PLAT",
    sample_date = as.Date("2024-01-01"),
    sample_latitude_dd = 56.0,
    sample_longitude_dd = 12.0,
    positioning_system_code = "GPS",
    water_depth_m = 10,
    monitoring_station_type_code = NA,
    monitoring_purpose_code = "PURP",
    monitoring_program_code = "PROG",
    reporting_institute_name_en = "Institute",
    analytical_laboratory_name_en = "Lab"
  )

  result <- check_datatype(data, level = "warning")
  expect_true(any(result$field == "monitoring_station_type_code"))
  expect_true(any(result$row == 1))
  expect_true(all(result$level == "warning"))
})

test_that("check_datatype catches multiple missing required fields", {
  data <- dplyr::tibble(
    station_name = "Station A" # missing many other required
  )

  result <- check_datatype(data, level = "error")
  expect_true(all(c("visit_year", "sample_project_name_en") %in% result$field))
  expect_true(all(result$level == "error"))
})

test_that("check_fields errors on unknown datatype", {
  df <- dplyr::tibble()
  expect_error(check_fields(df, "not_a_type"), "Unknown datatype")
})

test_that("check_fields returns empty tibble when all required fields are present", {
  defs <- list(
    TestType = list(required = c("id", "value"), recommended = c("comment"))
  )

  df <- dplyr::tibble(id = 1, value = "x", comment = "ok")

  errors <- check_fields(df, "TestType", level = "error", field_definitions = defs)

  expect_true(is.data.frame(errors))
  expect_equal(nrow(errors), 0)
})

test_that("check_fields catches missing required fields", {
  defs <- list(
    TestType = list(required = c("id", "value"), recommended = character())
  )

  df <- dplyr::tibble(id = 1) # missing 'value'

  errors <- check_fields(df, "TestType", level = "error", field_definitions = defs)

  expect_true("value" %in% errors$field)
  expect_true(all(errors$level == "error"))
  expect_match(errors$message, "is missing")
})

test_that("check_fields catches empty values in required fields", {
  defs <- list(
    TestType = list(required = c("id", "value"), recommended = character())
  )

  df <- dplyr::tibble(id = c(1, 2), value = c("", NA))

  errors <- check_fields(df, "TestType", level = "error", field_definitions = defs)

  expect_equal(unique(errors$field), "value")
  expect_true(all(errors$level == "error"))
  expect_true(all(errors$row %in% c(1, 2)))
  expect_match(errors$message[1], "Empty value")
})

test_that("check_fields warns on missing recommended fields when level = 'warning'", {
  defs <- list(
    TestType = list(required = "id", recommended = c("comment", "extra"))
  )

  df <- dplyr::tibble(id = 1)

  errors <- check_fields(df, "TestType", level = "warning", field_definitions = defs)

  expect_setequal(errors$field, c("comment", "extra"))
  expect_true(all(errors$level == "warning"))
  expect_match(errors$message[1], "Recommended field")
})

test_that("check_fields warns on empty values in recommended fields", {
  defs <- list(
    TestType = list(required = "id", recommended = "comment")
  )

  df <- dplyr::tibble(id = 1, comment = c("", NA, "ok"))

  errors <- check_fields(df, "TestType", level = "warning", field_definitions = defs)

  expect_equal(unique(errors$field), "comment")
  expect_true(all(errors$level == "warning"))
  expect_true(all(errors$row %in% c(1, 2)))
  expect_match(errors$message[1], "Empty value")
})

test_that("check_fields works with custom field_definitions", {
  custom_defs <- list(
    CustomType = list(required = "foo", recommended = "bar")
  )

  df <- dplyr::tibble(foo = "ok", bar = "ok")

  errors <- check_fields(df, "CustomType", level = "warning", field_definitions = custom_defs)

  expect_true(is.data.frame(errors))
  expect_equal(nrow(errors), 0)
})

### Tests for other check_ functions

check_functions <- list(
  Bacterioplankton = check_Bacterioplankton,
  Chlorophyll      = check_Chlorophyll,
  Epibenthos       = check_Epibenthos,
  EpibenthosDropvideo = check_EpibenthosDropvideo,
  GreySeal         = check_GreySeal,
  HarbourPorpoise  = check_HarbourPorpoise,
  HarbourSeal      = check_HarbourSeal,
  PhysicalChemical = check_PhysicalChemical,
  Phytoplankton    = check_Phytoplankton,
  Picoplankton     = check_Picoplankton,
  PrimaryProduction= check_PrimaryProduction,
  RingedSeal       = check_RingedSeal,
  SealPathology    = check_SealPathology,
  Sedimentation         = check_Sedimentation,
  Zoobenthos       = check_Zoobenthos,
  Zooplankton      = check_Zooplankton,
  deliv_Bacterioplankton = check_deliv_Bacterioplankton,
  deliv_Chlorophyll = check_deliv_Chlorophyll,
  deliv_Epibenthos = check_deliv_Epibenthos,
  deliv_EpibenthosDropvideo = check_deliv_EpibenthosDropvideo,
  deliv_GreySeal = check_deliv_GreySeal,
  deliv_HarbourPorpoise = check_deliv_HarbourPorpoise,
  deliv_HarbourSeal = check_deliv_HarbourSeal,
  deliv_PhysicalChemical = check_deliv_PhysicalChemical,
  deliv_Phytoplankton = check_deliv_Phytoplankton,
  deliv_Picoplankton = check_deliv_Picoplankton,
  deliv_PrimaryProduction = check_deliv_PrimaryProduction,
  deliv_RingedSeal = check_deliv_RingedSeal,
  deliv_SealPathology = check_deliv_SealPathology,
  deliv_Sedimentation = check_deliv_Sedimentation,
  deliv_Zoobenthos = check_deliv_Zoobenthos,
  deliv_Zooplankton = check_deliv_Zooplankton
)

test_that("deprecated check functions still works", {
  skip_if_offline()
  skip_if_resource_unavailable("https://smhi.se/")

  for (fname in names(check_functions)) {
    fn <- check_functions[[fname]]

    print(fname) # for debugging

    if (grepl("deliv_", fname)) {
      required <- find_required_fields(fname)
      recommended <- c()
    } else {
      required <- .field_definitions[[fname]]$required
      recommended <- .field_definitions[[fname]]$recommended
    }

    # Create data frame with all required fields filled
    df <- dplyr::as_tibble(setNames(rep(list("x"), length(required)), required))

    lifecycle::expect_deprecated(fn(df, level = "error"))

    # Create data frame with all fields filled
    df <- dplyr::as_tibble(setNames(rep(list("x"), length(c(required, recommended))),
                                    c(required, recommended)))

    lifecycle::expect_deprecated(fn(df, level = "warning"))
  }
})
