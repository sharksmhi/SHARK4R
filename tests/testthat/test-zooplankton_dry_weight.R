test_that("calc_zooplankton_dry_weight uses adult coefficients for non-nauplii", {
  zoo <- dplyr::tibble(
    parameter = "Length (mean)",
    value = 800,
    aphia_id = 149755,
    dev_stage_code = "AD"
  )

  result <- calc_zooplankton_dry_weight(zoo, append = FALSE, keep_reference = TRUE)

  expected <- 10^((2.965 * log10(800)) - 7.713)

  expect_equal(result$parameter, "Dry weight")
  expect_equal(result$value, expected)
  expect_equal(result$dry_weight_reference, "Hay 1991")
  expect_equal(result$dw_match_type, "adult")
})

test_that("calc_zooplankton_dry_weight uses taxon-specific nauplii coefficients when available", {
  zoo <- dplyr::tibble(
    parameter = "Length (mean)",
    value = 250,
    aphia_id = 104464,
    dev_stage_code = "NP"
  )

  result <- calc_zooplankton_dry_weight(zoo, append = FALSE, keep_reference = TRUE)

  expected <- 10^((2.03 * log10(250)) - 5.38)

  expect_equal(result$value, expected)
  expect_equal(result$dry_weight_reference, "Hygum et al 2000")
  expect_equal(result$dw_match_type, "nauplii_taxon_specific")
})

test_that("calc_zooplankton_dry_weight falls back to general nauplii coefficients", {
  zoo <- dplyr::tibble(
    parameter = "Length (mean)",
    value = 160,
    aphia_id = 149755,
    dev_stage_code = "NP"
  )

  result <- calc_zooplankton_dry_weight(zoo, append = FALSE, keep_reference = TRUE)

  expected <- 10^((2.231 * log10(160)) - 5.483)

  expect_equal(result$value, expected)
  expect_equal(
    result$dry_weight_reference,
    "Hay 1991 (same as for Pseudocalanus)"
  )
  expect_equal(
    result$dry_weight_reference_taxon,
    "copepod nauplii *all copepod species"
  )
  expect_equal(result$dw_match_type, "nauplii_general")
})

test_that("calc_zooplankton_dry_weight appends dry-weight rows and preserves original rows", {
  zoo <- dplyr::tibble(
    parameter = c("Length (mean)", "Abundance"),
    value = c(800, 12),
    aphia_id = c(149755, 149755),
    dev_stage_code = c("AD", "AD")
  )

  result <- calc_zooplankton_dry_weight(zoo, append = TRUE)

  expect_equal(nrow(result), 3)
  expect_equal(sum(result$parameter == "Dry weight"), 1)
  expect_equal(sum(result$parameter == "Abundance"), 1)
  expect_equal(sum(result$parameter == "Length (mean)"), 1)
})

test_that("calc_zooplankton_dry_weight errors on missing required columns", {
  zoo <- dplyr::tibble(
    parameter = "Length (mean)",
    value = 800,
    aphia_id = 149755
  )

  expect_error(
    calc_zooplankton_dry_weight(zoo),
    "Missing required columns: dev_stage_code"
  )
})

test_that("calc_zooplankton_dry_weight keeps NA when no adult coefficients are available", {
  zoo <- dplyr::tibble(
    parameter = "Length (mean)",
    value = 500,
    aphia_id = 999999,
    dev_stage_code = "AD"
  )

  result <- calc_zooplankton_dry_weight(zoo, append = FALSE, keep_reference = TRUE)

  expect_true(is.na(result$value))
  expect_true(is.na(result$dw_match_type))
  expect_true(is.na(result$dry_weight_reference))
})
