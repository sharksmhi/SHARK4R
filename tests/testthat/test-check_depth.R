test_that("check_depth detects invalid or impossible depth values", {
  skip_on_cran()
  skip_if_offline()
  skip_if_resource_unavailable("https://api.obis.org/")

  if(Sys.getenv("TZ") == "") {
    Sys.setenv(TZ="Europe/Brussels") # handle warning
  }

  t1 <- data.frame(sample_longitude_dd = 0, sample_latitude_dd = 0,
                   sample_min_depth_m = c("4936", "4938", "4935", "-20"),
                   sample_max_depth_m = c("4936", "4937", "5000", "-10"))
  d1 <- check_depth(t1, depthmargin = 0, shoremargin = NA, report = FALSE)
  expect_equal(2, nrow(d1))
  expect_equal(c("4938", "4935"), as.character(d1$sample_min_depth_m))
  r <- check_depth(t1, depthmargin = 0, shoremargin = NA, report = TRUE)
  expect_equal(4, nrow(r))
  expect_equal(3, sum(grepl("depth.*?margin", r$message)))
  expect_equal(1, sum(grepl("greater than maximum", r$message)))

  r2 <- check_depth(t1, depthmargin = 0, shoremargin = 1e6, report = TRUE)
  expect_equal(r, r2)

  r <- check_depth(t1, depthmargin = 1000, shoremargin = NA, report = TRUE)
  expect_equal(1, nrow(r))
  expect_equal(0, sum(grepl("depth.*?margin", r$message)))
  expect_equal(1, sum(grepl("greater than maximum", r$message)))

  r <- check_depth(t1, depthmargin = 0, shoremargin = 0, report = TRUE)
  expect_equal(6, nrow(r))
  expect_equal(3, sum(grepl("depth.*?margin", r$message)))
  expect_equal(1, sum(grepl("greater than maximum", r$message)))
  expect_equal(2, sum(grepl("shoredistance.*?margin", r$message)))
  expect_equal(2, sum(grepl("negative", r$message)))

  # missing column
  t <- data.frame(sample_longitude_dd=c(0), sample_latitude_dd=c(0), sample_min_depth_m=c("10"))
  d <- check_depth(t, depthmargin = 0, shoremargin = 0, report = FALSE)
  expect_equal(0, nrow(d))
  r <- check_depth(t, depthmargin = 0, shoremargin = 0, report = TRUE)
  expect_equal(1, sum(grepl("missing", r$message)))

  # missing column + an error
  t <- data.frame(sample_longitude_dd=c(0), sample_latitude_dd=c(0), sample_min_depth_m=c("10", "10000"))
  d <- check_depth(t, depthmargin = 0, shoremargin = 0, report = FALSE)
  expect_equal(1, nrow(d))
  r <- check_depth(t, depthmargin = 0, shoremargin = 0, report = TRUE)
  expect_equal(1, sum(grepl("missing", r$message)))
  expect_equal(1, sum(grepl("depth.*?margin", r$message)))
  expect_equal(2, nrow(r))

  t <- data.frame(sample_longitude_dd=c(0), sample_latitude_dd=c(0), sample_min_depth_m=c("", "", ""), sample_max_depth_m="4")
  r <- check_depth(t, depthmargin = 0, shoremargin = 0, report = TRUE)
  expect_equal(1, sum(grepl("empty", r$message)))
  t$sample_min_depth_m <- c("", "a", "1")
  r <- check_depth(t, depthmargin = 0, shoremargin = 0, report = TRUE)
  expect_equal(1, sum(grepl("not numeric", r$message)))
  expect_equal(1, sum(grepl("not numeric", r$message)))
})

test_that("support for tibble", {
  skip_on_cran()
  skip_if_offline()
  skip_if_resource_unavailable("https://api.obis.org/")

  skip_if_not_installed("dplyr")
  t1 <- data.frame(sample_longitude_dd=0, sample_latitude_dd=0,
                   sample_min_depth_m = c("4936", "4938", "4935", "-20"),
                   sample_max_depth_m = c("4936", "4937", "5000", "-10"))
  r1 <- check_depth(t1, depthmargin = 0, shoremargin = NA, report = TRUE)
  t2 <- dplyr::as_tibble(t1)
  r2 <- check_depth(t2, depthmargin = 0, shoremargin = NA, report = TRUE)
  expect_equal(r2, r1)
  expect_equal(nrow(r2), 4)
})


test_that("check_depth works with SpatRaster bathymetry", {

  # Create small test bathymetry raster
  r <- terra::rast(nrows=10, ncols=10, xmin=0, xmax=10, ymin=0, ymax=10)
  terra::values(r) <- matrix(seq(10, 100, length.out=100), nrow=10, ncol=10)

  # Create test data
  test_data <- data.frame(
    sample_latitude_dd = c(1, 5, 9, 5, 11),
    sample_longitude_dd = c(1, 5, 9, 5, 5),
    sample_depth_m = c(5, 50, 110, NA, 20)
  )

  # Case 1: Report errors and warnings
  res <- check_depth(
    data = test_data,
    depth_cols = "sample_depth_m",
    bathymetry = r,
    shoremargin = 0
  )

  # Expect a tibble output
  expect_s3_class(res, "tbl_df")

  # Expect the row with depth -110 to be flagged (too deep)
  too_deep <- res %>% filter(grepl("greater than the value found", message))
  expect_equal(too_deep$row, 3)

  # Expect out-of-bounds latitude to be flagged
  out_lat <- res %>% filter(grepl("Latitude", message))
  expect_equal(out_lat$row, 5)

  # Case 2: Return failing rows instead of report
  failing <- check_depth(
    data = test_data,
    depth_cols = "sample_depth_m",
    bathymetry = r,
    report = FALSE
  )

  # Expect subset of rows that failed any check
  expect_true(all(failing$sample_depth_m %in% c(110, NA, 20)))
})
