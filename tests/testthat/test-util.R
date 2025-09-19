test_that("check lon lat works as expected", {
  x <- SHARK4R:::check_lonlat(data.frame(), report = TRUE)
  expect_equal(2, nrow(x))
  expect_true(all(grepl("missing", x$message)))
  x <- SHARK4R:::check_lonlat(data.frame(sample_latitude_dd = ""), report = TRUE)
  expect_equal(2, nrow(x))
  expect_true(all(grepl("(missing)|(numeric)", x$message)))
  x <- SHARK4R:::check_lonlat(data.frame(sample_latitude_dd = 1), report = TRUE)
  expect_equal(1, nrow(x))
  expect_true(all(grepl("(missing)|(numeric)", x$message)))
  x <- SHARK4R:::check_lonlat(data.frame(sample_longitude_dd = "", sample_latitude_dd = ""), report = TRUE)
  expect_equal(2, nrow(x))
  expect_true(all(grepl("numeric", x$message)))
  x <- SHARK4R:::check_lonlat(data.frame(sample_longitude_dd = "", sample_latitude_dd = 1), report = TRUE)
  expect_equal(1, nrow(x))
  expect_true(all(grepl("numeric", x$message)))
  x <- SHARK4R:::check_lonlat(data.frame(sample_longitude_dd = 1, sample_latitude_dd = 1), report = TRUE)
  expect_equal(0, NROW(x))
  expect_error(SHARK4R:::check_lonlat(data.frame(), report = FALSE), "missing")
})

test_that("cache call works", {
  n <- 5
  set.seed(42)
  original <- SHARK4R:::cache_call("random5", expression(runif(n)))
  set.seed(50)
  same <- SHARK4R:::cache_call("random5", expression(runif(n)))
  set.seed(50)
  different <- SHARK4R:::cache_call("random5diff", expression(runif(n)))
  set.seed(50)
  original2 <- SHARK4R:::cache_call("random5", expression(runif(n, min = 0, max = 1) ))
  expect_equal(length(original), n)
  expect_equal(original, same)
  expect_false(any(original == different))
  expect_equal(original2, different)
  expect_gte(length(SHARK4R:::list_cache()), 3)
  # only run on Travis as clearing the cache between the test runs is annoying
  if(identical(Sys.getenv("TRAVIS"), "true")) {
    clean_shark4r_cache(0)
    expect_equal(length(SHARK4R:::list_cache()), 0)
  }
})

test_that("get_xy_clean_duplicates works", {
  n <- 100
  set.seed(42)
  lots_duplicates <- tibble(sample_longitude_dd = as.numeric(sample(1:10, n, replace = TRUE)), sample_latitude_dd = as.numeric(sample(1:10, n, replace = TRUE)))
  lots_duplicates[5,] <- list(NA, 1.0)
  lots_duplicates[6,] <- list(2.0, NA)
  lots_duplicates[7,] <- list(NA, NA)
  xy <- SHARK4R:::get_xy_clean_duplicates(lots_duplicates)

  replicate <- tibble(sample_longitude_dd = rep(NA, n), sample_latitude_dd = rep(NA, n))
  replicate[xy$isclean,] <- xy$uniquesp[xy$duplicated_lookup,]
  replicate[!xy$isclean,] <- lots_duplicates[!xy$isclean,]
  expect_equal(lots_duplicates, replicate)
})
