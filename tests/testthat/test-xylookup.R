test_data <- function(x=c(1,2,3), y=c(51,52,53)) {
  data.frame(sample_longitude_dd = x, sample_latitude_dd = y)
}

test_that("lookup_xy returns correct data", {
  skip_on_cran()
  skip_if_offline()
  skip_if_resource_unavailable("https://api.obis.org/")

  results <- lookup_xy(test_data(), as_data_frame = FALSE)
  expect_equal(length(results), 3)
  r1 <- results[[1]]
  expect_equal(sort(names(r1)), c("grids", "shoredistance"))
  expect_true("bathymetry" %in% names(r1$grids))
})

test_that("lookup_xy results filtering works", {
  skip_on_cran()
  skip_if_offline()
  skip_if_resource_unavailable("https://api.obis.org/")

  results <- lookup_xy(test_data(), areas = TRUE)
  expect_equal(nrow(results), 3)
  expect_true(all(c("shoredistance", "bathymetry", "sssalinity", "sstemperature", "obis") %in% colnames(results)))
  results <- lookup_xy(test_data(), areas = FALSE, grids = FALSE, shoredistance = TRUE)
  expect_equal(results[1,,drop=F]$shoredistance, 1604)
  expect_false(any(c("bathymetry", "obis") %in% colnames(results)))
  results <- lookup_xy(test_data(), areas = FALSE, grids = TRUE, shoredistance = FALSE)
  expect_false(any(c("shoredistance", "obis") %in% colnames(results)))
  expect_equal(results[1,,drop=F]$bathymetry, 2)
})

test_that("lookup_xy empty areas works", {
  skip_on_cran()
  skip_if_offline()
  skip_if_resource_unavailable("https://api.obis.org/")

  results <- lookup_xy(test_data(x=90,y=60), areas = TRUE, shoredistance = FALSE, grids = FALSE)
  expect_equal(nrow(results), 1)
  expect_equal(ncol(results), 0)
})

test_that("lookup_xy duplicate coordinates works", {
  skip_on_cran()
  skip_if_offline()
  skip_if_resource_unavailable("https://api.obis.org/")

  results <- lookup_xy(test_data(x=c(90,90,0,1,2,1),y=c(60,60,0,4,3,4)), as_data_frame = FALSE)
  expect_equal(length(results), 6)
  expect_equal(results[[1]],results[[2]])
  expect_equal(results[[4]],results[[6]])

  results <- lookup_xy(test_data(x=c(90,90,0,1,2,1),y=c(60,60,0,4,3,4)), as_data_frame = TRUE)
  expect_equal(nrow(results), 6)
  expect_equal(as.list(results[1,]),as.list(results[2,]))
  expect_equal(as.list(results[4,]),as.list(results[6,]))
})

test_that("lookup_xy mix of valid and invalid coordinates works", {
  skip_on_cran()
  skip_if_offline()
  skip_if_resource_unavailable("https://api.obis.org/")

  data <- test_data(x=c(90,"90",NA,-181,2,181,0),y=c(-91,60,0,4,91,4,0))
  expect_error(lookup_xy(data, as_data_frame = FALSE))
  expect_error(lookup_xy(data, as_data_frame = TRUE))

  data <- test_data(x=c(90,NA,-181,2,181,0),y=c(-91,0,4,91,4,0))
  results <- lookup_xy(data, as_data_frame = FALSE)
  expect_equal(length(results), 6)
  expect_equal(results[[1]], NULL)
  expect_equal(sort(names(results[[6]])), c("grids", "shoredistance"))

  results <- lookup_xy(data, as_data_frame = TRUE)
  expect_equal(nrow(results), 6)
  expect_equal(sum(is.na(results$shoredistance)), 5)
  expect_false(is.na(results$shoredistance[6]))
})

test_that("lookup_xy no data works", {
  skip_on_cran()
  skip_if_offline()
  skip_if_resource_unavailable("https://api.obis.org/")

  data <- test_data(x=numeric(0),y=numeric(0))
  expect_equal(length(lookup_xy(data, as_data_frame = FALSE)), 0)
  expect_equal(nrow(lookup_xy(data, as_data_frame = TRUE)), 0)
  data <- test_data(x=as.numeric(c(NA, NA)),y=as.numeric(c(NA, NA)))
  expect_equal(length(lookup_xy(data, as_data_frame = FALSE)), 2)
  expect_equal(nrow(lookup_xy(data, as_data_frame = TRUE)), 2)
})

test_that("wrong url fails", {
  options(obistools_xylookup_url = "https://api.obis.org/thisdoesnotexist")
  on.exit(options(obistools_xylookup_url = NULL))
  data <- test_data(x=0,y=0)
  expect_error(lookup_xy(data))
})

test_that("lookup_xy only areas works", {
  skip_on_cran()
  skip_if_offline()
  skip_if_resource_unavailable("https://api.obis.org/")

  data <- test_data(x=c(0,1),y=c(0,0))
  result <- lookup_xy(data, shoredistance = FALSE, grids = FALSE, areas = TRUE)
  expect_equal(nrow(result), 2)
  expect_true(!("shoredistance" %in% names(result)))
  expect_true(!("sstemperature" %in% names(result)))
})

test_that("lookup_xy works with lon/lat vectors (data frame output)", {
  skip_on_cran()
  skip_if_offline()
  skip_if_resource_unavailable("https://api.obis.org/")

  lon <- c(1, 2, 3)
  lat <- c(51, 52, 53)

  results <- lookup_xy(lon = lon, lat = lat, as_data_frame = TRUE, areas = 100)
  expect_equal(nrow(results), 3)
  expect_true(all(c("shoredistance", "bathymetry") %in% colnames(results)))
})
