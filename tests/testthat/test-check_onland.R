test_data <- function(x = c(1, 2, 3), y = c(51, 52, 53)) {
  data.frame(sample_longitude_dd = x, sample_latitude_dd = y)
}

test_that("check_onland offline parameter works", {
  skip_on_cran()
  skip_if_offline()
  skip_if_resource_unavailable("https://obis-resources.s3.amazonaws.com",
                               allow_status = c(0:399, 403))

  data <- test_data(x = c(2.89052, 2.921677), y = c(51.243543, 51.229194))
  df <- check_onland(data, offline = TRUE)
  expect_equal(nrow(df), 1)
})

test_that("check_onland buffer parameter works", {
  skip_on_cran()
  skip_if_offline()
  skip_if_resource_unavailable("https://api.obis.org/")
  skip_if_resource_unavailable("https://obis-resources.s3.amazonaws.com",
                               allow_status = c(0:399, 403))

  data <- test_data(x = c(2.922825, 2.918780), y = c(51.236716, 51.237912))
  df <- check_onland(data, offline = FALSE)
  expect_equal(nrow(df), 2)
  df <- check_onland(data, buffer = 100, offline = FALSE)
  expect_equal(nrow(df), 1)
  df <- check_onland(data, buffer = 1000, offline = FALSE)
  expect_equal(nrow(df), 0)
})

test_that("check_onland all on land works", {
  skip_on_cran()
  skip_if_offline()
  skip_if_resource_unavailable("https://api.obis.org/")
  skip_if_resource_unavailable("https://obis-resources.s3.amazonaws.com",
                               allow_status = c(0:399, 403))

  data <- test_data(x=c(20, 30), y=c(0, 0))
  df <- check_onland(data, offline=F)
  expect_equal(nrow(df), 2)
  df <- check_onland(data, offline=T)
  expect_equal(nrow(df), 2)
})

test_that("check_onland buffer parameter works", {
  skip_on_cran()
  skip_if_offline()
  skip_if_resource_unavailable("https://api.obis.org/")
  skip_if_resource_unavailable("https://obis-resources.s3.amazonaws.com",
                               allow_status = c(0:399, 403))

  data <- test_data(x=c(2.922825, 2.918780), y=c(51.236716, 51.237912))
  df <- check_onland(data, buffer=0, report=TRUE)
  expect_equal(nrow(df), 2)
  df <- check_onland(data, buffer=100, report=TRUE)
  expect_equal(nrow(df), 1)
  df <- check_onland(data, buffer=1000, report=TRUE)
  expect_equal(nrow(df), 0)
})

test_that("check_onland plot_leaflet parameter works", {
  skip_on_cran()
  skip_if_offline()
  skip_if_resource_unavailable("https://obis-resources.s3.amazonaws.com",
                               allow_status = c(0:399, 403))

  data <- test_data(x = c(2.89052, 2.921677), y = c(51.243543, 51.229194))
  m <- check_onland(data, offline = FALSE, plot_leaflet = TRUE)

  # Class check
  expect_s3_class(m, "leaflet")
  expect_s3_class(m, "htmlwidget")

  # Provider check
  expect_true(any(grepl("CartoDB.Positron", m$x$calls[[1]]$args[[1]])))
})
