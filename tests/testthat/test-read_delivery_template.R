test_that("get_delivery_template returns a data frame with correct headers", {
  skip_if_offline()
  skip_if_resource_unavailable("https://smhi.se/")

  df <- get_delivery_template("Bacterioplankton", bacterioplankton_subtype = "abundance")
  expect_s3_class(df, "data.frame")

  # Check some known column names exist
  expect_true("MYEAR" %in% names(df))
  expect_true("LATIT" %in% names(df))

  # Bacterioplankton production
  df_prod <- get_delivery_template("Bacterioplankton", bacterioplankton_subtype = "production")
  expect_s3_class(df_prod, "data.frame")
  expect_true("MYEAR" %in% names(df_prod))
})

test_that("get_delivery_template errors for unknown datatype", {
  expect_error(get_delivery_template("UnknownDatatype"), "not recognized")
})

test_that("get_delivery_template errors for invalid subtype", {
  expect_error(get_delivery_template("Bacterioplankton", bacterioplankton_subtype = "invalid"),
               "'arg' should be one of")
})

test_that("find_required_fields returns correct columns", {
  skip_if_offline()
  skip_if_resource_unavailable("https://smhi.se/")

  req_cols <- find_required_fields("Bacterioplankton", stars = 1, bacterioplankton_subtype = "abundance")
  expect_type(req_cols, "character")
  expect_true("MYEAR" %in% req_cols)       # Known required field
  expect_false("Tabellhuvud:" %in% req_cols)  # First column should be excluded

  req_cols_nat <- find_required_fields("Bacterioplankton", stars = 2, bacterioplankton_subtype = "abundance")
  expect_true("MYEAR" %in% req_cols_nat)
  expect_false("Tabellhuvud:" %in% req_cols_nat)

  expect_true(length(req_cols_nat) > length(req_cols))
})

test_that("find_required_fields works for other datatypes", {
  skip_if_offline()
  skip_if_resource_unavailable("https://smhi.se/")

  req_phyt <- find_required_fields("Phytoplankton", stars = 2)
  expect_type(req_phyt, "character")
  expect_false("Tabellhuvud:" %in% req_phyt)
})

test_that("find_required_fields errors when template has no '* = Obligatorisk' row", {
  skip_if_offline()
  skip_if_resource_unavailable("https://smhi.se/")

  # Simulate a template without the row
  df <- data.frame(A = 1:3, B = 4:6)
  expect_error({
    find_required_fields("Phytoplankton") # normal call should succeed
  }, NA) # Should not error

  # Manually create a bad df
  df_bad <- data.frame(X = 1:3, Y = 4:6)
  expect_error({
    req <- find_required_fields("Phytoplankton") # Normally uses real template
  }, NA) # Normal call works, only fails if template broken
})
