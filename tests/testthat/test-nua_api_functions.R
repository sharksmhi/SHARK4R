test_slug <- "dinophysis-acuta"
nua_url <- "https://nordicmicroalgae.org"

test_that("nua taxa are retrieved", {
  skip_if_offline()
  skip_if_resource_unavailable(nua_url)

  nua_taxa <- get_nua_taxa()

  expect_s3_class(nua_taxa, "data.frame")
  expect_true(nrow(nua_taxa) > 0)
  expect_true(all(c("scientific_name", "authority", "rank", "slug", "nua_url") %in% names(nua_taxa)))
})

test_that("nua links are retrieved", {
  skip_if_offline()
  skip_if_resource_unavailable(nua_url)

  nua_link <- get_nua_external_links(test_slug)

  expect_s3_class(nua_link, "data.frame")
  expect_true(nrow(nua_link) > 0)
  expect_true(all(c("slug", "provider", "label", "external_id", "external_url", "collection") %in% names(nua_link)))
})

test_that("nua harmfullness is retrieved", {
  skip_if_offline()
  skip_if_resource_unavailable(nua_url)

  nua_harmfulness <- get_nua_harmfulness(test_slug)

  expect_s3_class(nua_harmfulness, "data.frame")
  expect_true(nrow(nua_harmfulness) > 0)
  expect_true(all(c("slug", "provider", "label", "external_id", "external_url", "collection") %in% names(nua_harmfulness)))
})

test_that("nua images are retrieved", {
  skip_if_offline()
  skip_if_resource_unavailable(nua_url)

  nua_images <- get_nua_media_links()

  expect_s3_class(nua_images, "data.frame")
  expect_true(nrow(nua_images) > 0)
  expect_true(all(c("slug", "image_l_url", "image_o_url", "image_s_url", "image_m_url", "contributor", "photographer_artist", "copyright_holder", "license", "galleries") %in% names(nua_images)))
})
