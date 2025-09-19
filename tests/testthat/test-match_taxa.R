test_names <- c("Abra", "Dinophysis", "ljkf hlqsdkf")

test_that("match_wormstaxa works as expected", {
  results <- match_wormstaxa(test_names, ask = FALSE)
  expect_true(nrow(results) == length(test_names))
})
