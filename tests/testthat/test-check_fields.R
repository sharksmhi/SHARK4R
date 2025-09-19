test_that("check_datatype returns empty tibble when all required and recommended fields are present", {
  data <- dplyr::tibble(
    visit_year = 2024,
    station_name = "Station A",
    sample_project_name_sv = "Project",
    sample_orderer_name_sv = "Orderer",
    platform_code = "PLAT",
    sample_date = as.Date("2024-01-01"),
    sample_latitude_dd = 56.0,
    sample_longitude_dd = 12.0,
    positioning_system_code = "GPS",
    water_depth_m = 10,
    monitoring_station_type_code = "TYPE",
    monitoring_purpose_code = "PURP",
    monitoring_program_code = "PROG",
    reporting_institute_name_sv = "Institute",
    analytical_laboratory_name_sv = "Lab"
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
    sample_project_name_sv = c("Proj", "Proj"),
    sample_orderer_name_sv = c("Ord", "Ord"),
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
    sample_project_name_sv = "Proj",
    sample_orderer_name_sv = "Ord",
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
    sample_project_name_sv = "Proj",
    sample_orderer_name_sv = "Ord",
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
    sample_project_name_sv = "Proj",
    sample_orderer_name_sv = "Ord",
    platform_code = "PLAT",
    sample_date = as.Date("2024-01-01"),
    sample_latitude_dd = 56.0,
    sample_longitude_dd = 12.0,
    positioning_system_code = "GPS",
    water_depth_m = 10,
    monitoring_station_type_code = NA,
    monitoring_purpose_code = "PURP",
    monitoring_program_code = "PROG",
    reporting_institute_name_sv = "Institute",
    analytical_laboratory_name_sv = "Lab"
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
  expect_true(all(c("visit_year", "sample_project_name_sv") %in% result$field))
  expect_true(all(result$level == "error"))
})

### Tests for other check_ functions

check_functions <- list(
  Bacterioplankton = check_Bacterioplankton,
  Chlorophyll      = check_Chlorophyll,
  Epibenthos       = check_Epibenthos,
  Dropvideo        = check_EpibenthosDropvideo,
  Greyseal         = check_GreySeal,
  Harbourporpoise  = check_HarbourPorpoise,
  HarbourSeal      = check_HarbourSeal,
  PhysicalChemical = check_PhysicalChemical,
  Phytoplankton    = check_Phytoplankton,
  Picoplankton     = check_Picoplankton,
  PrimaryProduction= check_PrimaryProduction,
  RingedSeal       = check_RingedSeal,
  SealPathology    = check_SealPathology,
  Sediment         = check_Sedimentation,
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

test_that("check functions return empty tibble when all required fields are present", {
  for (fname in names(check_functions)) {
    fn <- check_functions[[fname]]

    # print(fname) # for debugging

    required <- formals(fn)$required %||% body(fn)[[3]][[3]] # extract inside function
    # safer: just call the function once on an empty tibble and look at the error field
    required <- eval(body(fn)[[3]][[3]])

    # Create data frame with all required fields filled
    df <- dplyr::as_tibble(setNames(rep(list("x"), length(required)), required))

    errors <- fn(df, level = "error")

    expect_true(is.data.frame(errors))
    expect_equal(nrow(errors), 0, info = paste("Function:", fname))
  }
})

test_that("check functions catch single missing required field", {
  for (fname in names(check_functions)) {
    fn <- check_functions[[fname]]

    required <- eval(body(fn)[[3]][[3]])
    df <- dplyr::as_tibble(setNames(rep(list("x"), length(required)), required))
    df <- df %>% dplyr::select(-1) # drop first required column

    errors <- fn(df, level = "error")

    expect_true(any(errors$field == required[1]))
    expect_true(all(errors$level == "error"))
  }
})

test_that("check functions catch multiple missing required fields", {
  for (fname in names(check_functions)) {
    fn <- check_functions[[fname]]

    required <- eval(body(fn)[[3]][[3]])
    df <- dplyr::as_tibble(setNames(rep(list("x"), length(required)), required))
    df <- df %>% dplyr::select(-c(1:3)) # drop 3 required fields

    errors <- fn(df, level = "error")

    expect_true(all(required[1:3] %in% errors$field))
    expect_true(all(errors$level == "error"))
  }
})

test_that("check functions catch empty values in required fields", {
  for (fname in names(check_functions)) {
    fn <- check_functions[[fname]]

    # Extract required fields programmatically from the function body
    required <- eval(body(fn)[[3]][[3]])

    # Create 3 rows of dummy data
    df <- dplyr::as_tibble(
      setNames(
        replicate(length(required), rep("x", 3), simplify = FALSE),
        required
      )
    )

    # Insert empty and NA values into the first required column
    df[[required[1]]] <- c("", "x", NA)

    errors <- fn(df, level = "error")

    expect_true(any(errors$field == required[1]))
    expect_true(any(errors$row %in% c(1, 3)))
  }
})

test_that("check functions catch missing recommended fields (level = 'warning')", {
  for (fname in names(check_functions)) {
    fn <- check_functions[[fname]]

    # print(fname) # for debugging

    required <- get_vector_from_body(fn, "required")
    recommended <- get_vector_from_body(fn, "recommended")

    df <- dplyr::as_tibble(setNames(rep(list("x"), length(required)), required))
    # no recommended fields included

    errors <- fn(df, level = "warning")

    # add 'level' column if missing
    if (!"level" %in% names(errors)) {
      errors$level <- character(0)
    }

    # add 'field' column if missing
    if (!"field" %in% names(errors)) {
      errors$field <- character(0)
    }

    expect_true(all(recommended %in% errors$field))
    expect_true(all(errors$level == "warning"))
  }
})

test_that("check functions report all required and recommended fields present", {
  for (fname in names(check_functions)) {

    print(fname) # for debugging

    fn <- check_functions[[fname]]

    required <- get_vector_from_body(fn, "required")
    recommended <- get_vector_from_body(fn, "recommended")

    # Build tibble with all required + recommended filled
    df <- dplyr::as_tibble(
      setNames(rep(list("x"), length(c(required, recommended))), c(required, recommended))
    )

    # Run with warning level to check both required & recommended
    expect_message({
      errors <- fn(df, level = "warning")
    }, "All required fields present|All recommended fields present")

    expect_true(is.data.frame(errors))
    expect_equal(nrow(errors), 0, info = paste("Function:", fname))
  }
})

test_that("check functions catch empty values in recommended fields (level = 'warning')", {
  for (fname in names(check_functions)) {
    fn <- check_functions[[fname]]

    required <- get_vector_from_body(fn, "required")
    recommended <- get_vector_from_body(fn, "recommended")

    # only run if function has recommended fields
    if (length(recommended) == 0) next

    # Create 3 rows of dummy data for required
    df <- dplyr::as_tibble(
      setNames(
        replicate(length(required), rep("x", 3), simplify = FALSE),
        required
      )
    )

    # Add recommended column with empty and NA values
    df[[recommended[1]]] <- c("", NA, "x")

    errors <- fn(df, level = "warning")

    expect_true(any(errors$field == recommended[1]))
    expect_true(any(errors$level == "warning"))
  }
})
