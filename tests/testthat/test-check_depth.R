test_that("check_depth detects character depths", {
  df_char <- data.frame(value = c("<5", "10", ">20"), stringsAsFactors = FALSE)

  # Expect the ERROR message and that the returned object is a DT datatable
  expect_message(res <- check_depth(df_char),
                 "ERROR: Expected numerical value is formatted as character")
  expect_s3_class(res, "datatables")
})

test_that("check_depth passes with numeric depths", {
  df_num <- data.frame(value = c(5, 10, 20))

  expect_message(res <- check_depth(df_num),
                 "Expected numerical value is formatted as numerical and no character values were found")
  expect_null(res)
})

test_that("check_depth errors when 'value' column is missing", {
  expect_error(check_depth(data.frame(x = 1)), "data must contain a 'value' column")
})

