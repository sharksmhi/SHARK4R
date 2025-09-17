test_that("check_value_logical detects invalid characters", {
  df_char <- data.frame(value = c("<5", "10", "maybe", ">20"), stringsAsFactors = FALSE)

  expect_message(
    res <- check_value_logical(df_char),
    "ERROR: Expected numerical/logical value but found invalid characters"
  )
  expect_s3_class(res, "datatables")

  # Extract values from DT object and check contents
  extracted <- res$x$data$value
  expect_true(all(c("<5", "maybe", ">20") %in% extracted))
})

test_that("check_value_logical passes with numeric values", {
  df_num <- data.frame(value = c(1, 2.5, 10))

  expect_message(
    res <- check_value_logical(df_num),
    "Expected values are correctly formatted"
  )
  expect_null(res)
})

test_that("check_value_logical passes with logical values", {
  df_log <- data.frame(value = c(TRUE, FALSE, TRUE))

  expect_message(
    res <- check_value_logical(df_log),
    "Expected values are correctly formatted"
  )
  expect_null(res)
})

test_that("check_value_logical errors if value column missing", {
  expect_error(check_value_logical(data.frame(x = 1)), "data must contain a 'value' column")
})

test_that("check_zero_value detects zeros (datatable)", {
  df_zero_value <- data.frame(
    station_name = c("A", "B"),
    sample_date = as.Date(c("2020-01-01", "2020-01-02")),
    sample_id = c("s1", "s2"),
    shark_sample_id_md5 = c("abc", "def"),
    sample_min_depth_m = c(5, 10),
    sample_max_depth_m = c(10, 20),
    value = c(0, 5)
  )

  expect_message(
    res <- check_zero_value(df_zero_value),
    "ERROR: Value contain zeroes"
  )
  expect_s3_class(res, "datatables")
})

test_that("check_zero_value detects zeros (return_df = TRUE)", {
  df_zero_value <- data.frame(
    station_name = c("A", "B"),
    sample_date = as.Date(c("2020-01-01", "2020-01-02")),
    sample_id = c("s1", "s2"),
    shark_sample_id_md5 = c("abc", "def"),
    sample_min_depth_m = c(5, 10),
    sample_max_depth_m = c(10, 20),
    value = c(0, 5)
  )

  expect_message(
    res <- check_zero_value(df_zero_value, return_df = TRUE),
    "ERROR: Value contain zeroes"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(res$value, 0)  # only zeros should be returned
})

test_that("check_zero_value passes when no zeros", {
  df_zero_value <- data.frame(value = c(1, 2, 3))

  expect_message(
    res <- check_zero_value(df_zero_value),
    "No zero values were found"
  )
  expect_null(res)
})

test_that("check_zero_value errors if value column missing", {
  expect_error(check_zero_value(data.frame(x = 1)), "data must contain a 'value' column")
})

df_zero_position <- data.frame(
  station_name = c("A", "B", "C"),
  sample_date = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03")),
  sample_id = c("s1", "s2", "s3"),
  shark_sample_id_md5 = c("abc", "def", "ghi"),
  sample_min_depth_m = c(5, 10, 15),
  sample_max_depth_m = c(10, 20, 25),
  sample_longitude_dd = c(0, 15, 0),
  sample_latitude_dd = c(55, 0, 60)
)

test_that("check_zero_positions detects zero longitude", {
  res <- check_zero_positions(df_zero_position, coord = "longitude", return_logical = TRUE)
  expect_equal(res, c(TRUE, FALSE, TRUE))
})

test_that("check_zero_positions detects zero latitude", {
  res <- check_zero_positions(df_zero_position, coord = "latitude", return_logical = TRUE)
  expect_equal(res, c(FALSE, TRUE, FALSE))
})

test_that("check_zero_positions detects zero in both coordinates", {
  res <- check_zero_positions(df_zero_position, coord = "both", return_logical = TRUE)
  expect_equal(res, c(TRUE, TRUE, TRUE))
})

test_that("check_zero_positions returns correct data.frame with return_df", {
  res <- check_zero_positions(df_zero_position, coord = "both", return_df = TRUE)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 3)
})

test_that("check_zero_positions returns DT datatable by default", {
  expect_s3_class(check_zero_positions(df_zero_position, coord = "both"), "datatables")
})

test_that("check_zero_positions returns FALSE when no zeros", {
  df_no_zero <- data.frame(sample_longitude_dd = c(12, 15), sample_latitude_dd = c(55, 56))
  expect_equal(check_zero_positions(df_no_zero, coord = "both", return_logical = TRUE), c(FALSE, FALSE))
})

test_that("warns when both return_df and return_logical are TRUE", {
  expect_warning(
    res <- check_zero_positions(df_zero_position, return_df = TRUE, return_logical = TRUE),
    "Both return_df and return_logical are TRUE"
  )
  expect_type(res, "logical")
})

df_epibenthos_totcover <- data.frame(
  station_name = c("A","B","C"),
  sample_date = as.Date(c("2020-01-01","2020-01-02","2020-01-03")),
  sample_id = c("s1","s2","s3"),
  shark_sample_id_md5 = c("abc","def","ghi"),
  sample_min_depth_m = c(5,10,15),
  sample_max_depth_m = c(10,20,25),
  parameter = c("Total cover of all species (%)","Total cover of all species (%)","Other parameter"),
  value = c(110, 95, 50)
)

test_that("returns logical vector correctly", {
  res <- check_epibenthos_totcover_logical(df_epibenthos_totcover, return_logical = TRUE)
  expect_type(res, "logical")
  expect_length(res, nrow(df_epibenthos_totcover))
  expect_equal(res, c(TRUE, FALSE, FALSE))
})

test_that("returns data.frame with return_df", {
  res <- check_epibenthos_totcover_logical(df_epibenthos_totcover, return_df = TRUE)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$value, 110)
})

test_that("returns DT datatable by default", {
  res <- check_epibenthos_totcover_logical(df_epibenthos_totcover)
  expect_s3_class(res, "datatables")
})

test_that("returns NULL if no rows exceed 100%", {
  df2 <- df_epibenthos_totcover
  df2$value <- c(90, 95, 50)
  expect_message(res <- check_epibenthos_totcover_logical(df2),
                 "within 0-100%")
  expect_null(res)
})

test_that("errors if required columns missing", {
  expect_error(check_epibenthos_totcover_logical(data.frame(x=1)),
               "data must contain column\\(s\\): parameter, value")
})

test_that("warns when both return_df and return_logical are TRUE", {
  expect_warning(
    res <- check_epibenthos_totcover_logical(df_epibenthos_totcover, return_df = TRUE, return_logical = TRUE),
    "Both return_df and return_logical are TRUE"
  )
  expect_type(res, "logical")
})

df_epibenthos_coverpercent <- data.frame(
  station_name = c("A","B","C"),
  sample_date = as.Date(c("2020-01-01","2020-01-02","2020-01-03")),
  sample_id = c("s1","s2","s3"),
  shark_sample_id_md5 = c("abc","def","ghi"),
  sample_min_depth_m = c(5,10,15),
  sample_max_depth_m = c(10,20,25),
  parameter = c("Cover (%)","Cover (%)","Other parameter"),
  value = c(105, 95, 50)
)

test_that("returns logical vector correctly", {
  res <- check_epibenthos_coverpercent_logical(df_epibenthos_coverpercent, return_logical = TRUE)
  expect_type(res, "logical")
  expect_length(res, nrow(df_epibenthos_coverpercent))
  expect_equal(res, c(TRUE, FALSE, FALSE))
})

test_that("returns data.frame with return_df", {
  res <- check_epibenthos_coverpercent_logical(df_epibenthos_coverpercent, return_df = TRUE)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$value, 105)
})

test_that("returns DT datatable by default", {
  res <- check_epibenthos_coverpercent_logical(df_epibenthos_coverpercent)
  expect_s3_class(res, "datatables")
})

test_that("returns NULL if no rows exceed 100%", {
  df2 <- df_epibenthos_coverpercent
  df2$value <- c(90, 95, 50)
  expect_message(res <- check_epibenthos_coverpercent_logical(df2),
                 "within 0-100%")
  expect_null(res)
})

test_that("errors if required columns missing", {
  expect_error(check_epibenthos_coverpercent_logical(data.frame(x=1)),
               "data must contain column\\(s\\): parameter, value")
})

test_that("warns when both return_df and return_logical are TRUE", {
  expect_warning(
    res <- check_epibenthos_coverpercent_logical(df_epibenthos_coverpercent, return_df = TRUE, return_logical = TRUE),
    "Both return_df and return_logical are TRUE"
  )
  expect_type(res, "logical")
})

df_epibenthos_cover <- data.frame(
  station_name = c("A","B","C"),
  sample_date = as.Date(c("2020-01-01","2020-01-02","2020-01-03")),
  sample_id = c("s1","s2","s3"),
  shark_sample_id_md5 = c("abc","def","ghi"),
  sample_min_depth_m = c(5,10,15),
  sample_max_depth_m = c(10,20,25),
  parameter = c("Cover","Cover","Other parameter"),
  value = c(120, 95, 50)
)

test_that("returns logical vector correctly", {
  res <- check_epibenthos_cover_logical(df_epibenthos_cover, return_logical = TRUE)
  expect_type(res, "logical")
  expect_length(res, nrow(df_epibenthos_cover))
  expect_equal(res, c(TRUE, FALSE, FALSE))
})

test_that("returns data.frame with return_df", {
  res <- check_epibenthos_cover_logical(df_epibenthos_cover, return_df = TRUE)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$value, 120)
})

test_that("returns DT datatable by default", {
  res <- check_epibenthos_cover_logical(df_epibenthos_cover)
  expect_s3_class(res, "datatables")
})

test_that("returns NULL if no rows exceed 100%", {
  df2 <- df_epibenthos_cover
  df2$value <- c(90, 95, 50)
  expect_message(res <- check_epibenthos_cover_logical(df2),
                 "within 0-100%")
  expect_null(res)
})

test_that("warns when both return_df and return_logical are TRUE", {
  expect_warning(
    res <- check_epibenthos_cover_logical(df_epibenthos_cover, return_df = TRUE, return_logical = TRUE),
    "Both return_df and return_logical are TRUE"
  )
  expect_type(res, "logical")
})

test_that("errors if required columns missing", {
  expect_error(check_epibenthos_cover_logical(data.frame(x=1)),
               "data must contain column\\(s\\): parameter, value")
})

df_coverclass_logical <- data.frame(
  station_name = c("A","B","C"),
  sample_date = as.Date(c("2020-01-01","2020-01-02","2020-01-03")),
  sample_id = c("s1","s2","s3"),
  shark_sample_id_md5 = c("abc","def","ghi"),
  sample_min_depth_m = c(5,10,15),
  sample_max_depth_m = c(10,20,25),
  parameter = c("Cover class","Cover class","Other parameter"),
  value = c(12, 8, 5)
)

test_that("returns logical vector correctly", {
  res <- check_epibenthos_coverclass_logical(df_coverclass_logical, return_logical = TRUE)
  expect_type(res, "logical")
  expect_length(res, nrow(df_coverclass_logical))
  expect_equal(res, c(TRUE, FALSE, FALSE))
})

test_that("returns data.frame with return_df", {
  res <- check_epibenthos_coverclass_logical(df_coverclass_logical, return_df = TRUE)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$value, 12)
})

test_that("returns DT datatable by default", {
  res <- check_epibenthos_coverclass_logical(df_coverclass_logical)
  expect_s3_class(res, "datatables")
})

test_that("returns NULL if no rows exceed 10", {
  df2 <- df_coverclass_logical
  df2$value <- c(5, 8, 3)
  expect_message(res <- check_epibenthos_coverclass_logical(df2),
                 "within 0-10")
  expect_null(res)
})

test_that("warns when both return_df and return_logical are TRUE", {
  expect_warning(
    res <- check_epibenthos_coverclass_logical(df_coverclass_logical, return_df = TRUE, return_logical = TRUE),
    "Both return_df and return_logical are TRUE"
  )
  expect_type(res, "logical")
})

test_that("errors if required columns missing", {
  expect_error(check_epibenthos_coverclass_logical(data.frame(x=1)),
               "data must contain column\\(s\\): parameter, value")
})

df_epibenthos_sedimentdepos <- data.frame(
  station_name = c("A","B","C"),
  sample_date = as.Date(c("2020-01-01","2020-01-02","2020-01-03")),
  sample_id = c("s1","s2","s3"),
  shark_sample_id_md5 = c("abc","def","ghi"),
  sample_min_depth_m = c(5,10,15),
  sample_max_depth_m = c(10,20,25),
  parameter = c("Sediment deposition cover (%)","Sediment deposition cover (%)","Other parameter"),
  value = c(110, 95, 50)
)

test_that("returns logical vector correctly", {
  res <- check_epibenthos_sedimentdepos_logical(df_epibenthos_sedimentdepos, return_logical = TRUE)
  expect_type(res, "logical")
  expect_length(res, nrow(df_epibenthos_sedimentdepos))
  expect_equal(res, c(TRUE, FALSE, FALSE))
})

test_that("returns data.frame with return_df", {
  res <- check_epibenthos_sedimentdepos_logical(df_epibenthos_sedimentdepos, return_df = TRUE)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$value, 110)
})

test_that("returns DT datatable by default", {
  res <- check_epibenthos_sedimentdepos_logical(df_epibenthos_sedimentdepos)
  expect_s3_class(res, "datatables")
})

test_that("returns NULL if no rows exceed 100%", {
  df2 <- df_epibenthos_sedimentdepos
  df2$value <- c(90, 95, 50)
  expect_message(res <- check_epibenthos_sedimentdepos_logical(df2),
                 "within 0-100%")
  expect_null(res)
})

test_that("warns when both return_df and return_logical are TRUE", {
  expect_warning(
    res <- check_epibenthos_sedimentdepos_logical(df_epibenthos_sedimentdepos, return_df = TRUE, return_logical = TRUE),
    "Both return_df and return_logical are TRUE"
  )
  expect_type(res, "logical")
})

test_that("errors if required columns missing", {
  expect_error(check_epibenthos_sedimentdepos_logical(data.frame(x=1)),
               "data must contain column\\(s\\): parameter, value")
})

df_epibenthos_abundclass <- data.frame(
  station_name = c("A","B","C"),
  sample_date = as.Date(c("2020-01-01","2020-01-02","2020-01-03")),
  sample_id = c("s1","s2","s3"),
  shark_sample_id_md5 = c("abc","def","ghi"),
  sample_min_depth_m = c(5,10,15),
  sample_max_depth_m = c(10,20,25),
  parameter = c("Abundance class","Abundance class","Other parameter"),
  value = c(12, 8, 5)
)

test_that("returns logical vector correctly", {
  res <- check_epibenthos_abundclass_logical(df_epibenthos_abundclass, return_logical = TRUE)
  expect_type(res, "logical")
  expect_length(res, nrow(df_epibenthos_abundclass))
  expect_equal(res, c(TRUE, FALSE, FALSE))
})

test_that("returns data.frame with return_df", {
  res <- check_epibenthos_abundclass_logical(df_epibenthos_abundclass, return_df = TRUE)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$value, 12)
})

test_that("returns DT datatable by default", {
  res <- check_epibenthos_abundclass_logical(df_epibenthos_abundclass)
  expect_s3_class(res, "datatables")
})

test_that("returns NULL if no rows exceed 10", {
  df2 <- df_epibenthos_abundclass
  df2$value <- c(5, 8, 3)
  expect_message(res <- check_epibenthos_abundclass_logical(df2),
                 "within 0-10")
  expect_null(res)
})

test_that("warns when both return_df and return_logical are TRUE", {
  expect_warning(
    res <- check_epibenthos_abundclass_logical(df_epibenthos_abundclass, return_df = TRUE, return_logical = TRUE),
    "Both return_df and return_logical are TRUE"
  )
  expect_type(res, "logical")
})

test_that("errors if required columns missing", {
  expect_error(check_epibenthos_abundclass_logical(data.frame(x=1)),
               "data must contain column\\(s\\): parameter, value")
})

df_BQIm_test <- data.frame(
  station_name = c("A","B","C","D"),
  sample_date = as.Date(c("2020-01-01","2020-01-02","2020-01-03","2020-01-04")),
  sample_id = c("s1","s2","s3","s4"),
  shark_sample_id_md5 = c("abc","def","ghi","jkl"),
  sample_min_depth_m = c(5,10,15,20),
  sample_max_depth_m = c(10,20,25,30),
  parameter = c("Abundance","BQIm","Abundance","BQIm"),
  value = c(0, 5, 10, 0)
)

test_that("returns logical vector correctly", {
  res <- check_zoobenthos_BQIm_logical(df_BQIm_test, return_logical = TRUE)
  expect_type(res, "logical")
  expect_length(res, nrow(df_BQIm_test))
  expect_equal(res, c(TRUE, TRUE, FALSE, FALSE))
})

test_that("returns data.frame with return_df", {
  res <- check_zoobenthos_BQIm_logical(df_BQIm_test, return_df = TRUE)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_equal(res$value, c(0,5))
})

test_that("returns DT datatable by default", {
  res <- check_zoobenthos_BQIm_logical(df_BQIm_test)
  expect_s3_class(res, "datatables")
})

test_that("returns NULL if no logical violations", {
  df2 <- df_BQIm_test
  df2$value <- c(10, 0, 5, 0)
  expect_message(res <- check_zoobenthos_BQIm_logical(df2),
                 "follow logical assumption")
  expect_null(res)
})

test_that("warns when both return_df and return_logical are TRUE", {
  expect_warning(
    res <- check_zoobenthos_BQIm_logical(df_BQIm_test, return_df = TRUE, return_logical = TRUE),
    "Both return_df and return_logical are TRUE"
  )
  expect_type(res, "logical")
})

test_that("errors if required columns missing", {
  expect_error(check_zoobenthos_BQIm_logical(data.frame(x=1)),
               "data must contain column\\(s\\): parameter, value")
})

df_wetweight_test <- data.frame(
  station_name = c("A","B","C"),
  sample_date = as.Date(c("2020-01-01","2020-01-02","2020-01-03")),
  sample_id = c("s1","s2","s3"),
  shark_sample_id_md5 = c("abc","def","ghi"),
  sample_min_depth_m = c(5,10,15),
  sample_max_depth_m = c(10,20,25),
  parameter = c("Wet weight","Wet weight","Other parameter"),
  value = c(0, 2, 5)
)

test_that("returns logical vector correctly", {
  res <- check_zoobenthos_wetweight_logical(df_wetweight_test, return_logical = TRUE)
  expect_type(res, "logical")
  expect_length(res, nrow(df_wetweight_test))
  expect_equal(res, c(TRUE, FALSE, FALSE))
})

test_that("returns data.frame with return_df", {
  res <- check_zoobenthos_wetweight_logical(df_wetweight_test, return_df = TRUE)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$value, 0)
})

test_that("returns DT datatable by default", {
  res <- check_zoobenthos_wetweight_logical(df_wetweight_test)
  expect_s3_class(res, "datatables")
})

test_that("returns NULL if no rows have zero Wet weight", {
  df2 <- df_wetweight_test
  df2$value <- c(1, 2, 5)
  expect_message(res <- check_zoobenthos_wetweight_logical(df2),
                 "follow logical assumption")
  expect_null(res)
})

test_that("warns when both return_df and return_logical are TRUE", {
  expect_warning(
    res <- check_zoobenthos_wetweight_logical(df_wetweight_test, return_df = TRUE, return_logical = TRUE),
    "Both return_df and return_logical are TRUE"
  )
  expect_type(res, "logical")
})

test_that("errors if required columns missing", {
  expect_error(check_zoobenthos_wetweight_logical(data.frame(x=1)),
               "data must contain column\\(s\\): parameter, value")
})
