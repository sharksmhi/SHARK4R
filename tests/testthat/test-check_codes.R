test_that("get_shark_codes works", {
  skip_if_offline()
  skip_if_resource_unavailable("https://smhi.se/")

  codelist <- get_shark_codes()

  expect_s3_class(codelist, "tbl_df")
  expect_true(nrow(codelist) > 0)
  expect_true(all(c("Data_field", "Code", "Note", "Reference") %in% colnames(codelist)))
})

test_that("check_code works", {
  skip_if_offline()
  skip_if_resource_unavailable("https://smhi.se/")
  data <- data.frame(sample_project_name_en = c("NAT National Base Activity", "Unknown Project"))
  check_codes <- check_codes(data)

  expect_s3_class(check_codes, "data.frame")
  expect_true(nrow(check_codes) > 0)
  expect_true(all(c("reported_code", "match_type") %in% colnames(check_codes)))
})

test_that("deprecated check_code_proj works", {
  skip_if_offline()
  skip_if_resource_unavailable("https://smhi.se/")
  data <- data.frame(sample_project_name_sv = c("NAT National Base Activity", "Unknown Project"))
  check_codes <- lifecycle::expect_deprecated(check_code_proj(data))
})
