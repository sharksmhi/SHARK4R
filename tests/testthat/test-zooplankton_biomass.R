make_zoo_base <- function(params, values, units = NA_character_,
                          aphia_id = 104251, dev_stage_code = "AD",
                          size_class = NA_character_) {
  dplyr::tibble(
    platform_code = "77SE",
    station_name = "ANHOLT E",
    sample_date = as.Date("2023-06-01"),
    sample_time = "10:00",
    sample_min_depth_m = 0,
    sample_max_depth_m = 30,
    aphia_id = aphia_id,
    sex_code = NA_character_,
    dev_stage_code = dev_stage_code,
    size_class = size_class,
    parameter = params,
    value = values,
    unit = units
  )
}

test_that("calc_zooplankton_biomass computes concentration and integrated biomass", {
  dw <- 10^((2.965 * log10(800)) - 7.713)
  zoo <- make_zoo_base(
    params = c("Dry weight (mean)", "Abundance", "Integrated abundance"),
    values = c(dw, 120, 3600),
    units  = c("ug", "ind/m3", "ind/m2")
  )

  result <- calc_zooplankton_biomass(zoo, append = FALSE)

  conc <- result[result$parameter == "Biomass concentration", ]
  integ <- result[result$parameter == "Integrated biomass", ]

  expect_equal(nrow(conc), 1)
  expect_equal(nrow(integ), 1)
  expect_equal(conc$value, dw * 120 / 1000)
  expect_equal(integ$value, dw * 3600 / 1000)
  expect_equal(conc$unit, "mg/m3")
  expect_equal(integ$unit, "mg/m2")
})

test_that("calc_zooplankton_biomass computes dry weight internally when missing", {
  zoo <- make_zoo_base(
    params = c("Length (mean)", "Abundance"),
    values = c(800, 120),
    units  = c("um", "ind/m3")
  )

  result <- calc_zooplankton_biomass(zoo, append = FALSE)
  expected_dw <- 10^((2.965 * log10(800)) - 7.713)

  expect_equal(nrow(result), 1)
  expect_equal(result$parameter, "Biomass concentration")
  expect_equal(result$value, expected_dw * 120 / 1000)
})

test_that("calc_zooplankton_biomass matches one-to-one on the observation key", {
  dw <- 10^((2.965 * log10(800)) - 7.713)
  zoo <- dplyr::bind_rows(
    make_zoo_base(
      params = c("Dry weight (mean)", "Abundance"),
      values = c(dw, 100),
      units  = c("ug", "ind/m3"),
      size_class = "small"
    ),
    make_zoo_base(
      params = c("Dry weight (mean)", "Abundance"),
      values = c(dw * 2, 50),
      units  = c("ug", "ind/m3"),
      size_class = "large"
    )
  )

  result <- calc_zooplankton_biomass(zoo, append = FALSE)

  expect_equal(nrow(result), 2)
  expect_setequal(
    result$value,
    c(dw * 100 / 1000, dw * 2 * 50 / 1000)
  )
})

test_that("calc_zooplankton_biomass appends biomass rows while preserving input", {
  dw <- 10^((2.965 * log10(800)) - 7.713)
  zoo <- make_zoo_base(
    params = c("Dry weight (mean)", "Abundance"),
    values = c(dw, 120),
    units  = c("ug", "ind/m3")
  )

  result <- calc_zooplankton_biomass(zoo, append = TRUE)

  expect_equal(nrow(result), 3)
  expect_equal(sum(result$parameter == "Biomass concentration"), 1)
})

test_that("calc_zooplankton_biomass drops NA biomass rows by default", {
  zoo <- make_zoo_base(
    params = c("Dry weight (mean)", "Abundance"),
    values = c(NA_real_, 120),
    units  = c("ug", "ind/m3")
  )

  result <- calc_zooplankton_biomass(zoo, append = FALSE)

  expect_equal(nrow(result), 0)
})

test_that("calc_zooplankton_biomass can keep NA biomass rows", {
  zoo <- make_zoo_base(
    params = c("Abundance"),
    values = c(120),
    units  = c("ind/m3"),
    aphia_id = 999999
  )

  result <- calc_zooplankton_biomass(
    zoo,
    append = FALSE,
    drop_na_values = FALSE
  )

  expect_equal(nrow(result), 1)
  expect_true(is.na(result$value))
})

test_that("calc_zooplankton_biomass errors on missing required columns", {
  zoo <- dplyr::tibble(
    parameter = "Abundance",
    value = 120
  )

  expect_error(
    calc_zooplankton_biomass(zoo),
    "Missing required columns"
  )
})
