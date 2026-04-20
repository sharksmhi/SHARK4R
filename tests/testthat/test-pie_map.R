shark_df <- function() {
  data.frame(
    station_name        = rep(c("BY5", "BY31", "BY29"), each = 3),
    sample_longitude_dd = rep(c(15.98, 18.23, 20.30), each = 3),
    sample_latitude_dd  = rep(c(55.25, 58.59, 58.88), each = 3),
    group = rep(c("Diatoms", "Dinoflagellates", "Cyanobacteria"), 3),
    value = c(50, 30, 20,  10, 60, 30,  70, 20, 10),
    stringsAsFactors = FALSE
  )
}

close_df <- function() {
  data.frame(
    station_name        = rep(c("A", "B"), each = 2),
    sample_longitude_dd = rep(c(15.00, 15.05), each = 2),
    sample_latitude_dd  = rep(c(57.00, 57.05), each = 2),
    group = rep(c("X", "Y"), 2),
    value = c(60, 40, 30, 70),
    stringsAsFactors = FALSE
  )
}

skip_without_basemap <- function() {
  testthat::skip_if_not_installed("rnaturalearth")
  testthat::skip_if_not_installed("rnaturalearthdata")
}

# ── create_pie_map ──────────────────────────────────────────────────────────

test_that("create_pie_map returns a ggplot", {
  skip_without_basemap()
  p <- create_pie_map(shark_df())
  expect_s3_class(p, "ggplot")
})

test_that("create_pie_map works with custom column names", {
  skip_without_basemap()
  df <- shark_df()
  names(df) <- c("stn", "lon", "lat", "grp", "val")
  p <- create_pie_map(df,
                      station_col = "stn", lon_col = "lon", lat_col = "lat",
                      group_col   = "grp", value_col = "val")
  expect_s3_class(p, "ggplot")
})

test_that("create_pie_map errors on missing column", {
  df <- shark_df()
  df$value <- NULL
  expect_error(create_pie_map(df), "missing required columns")
})

test_that("create_pie_map errors on non-numeric value column", {
  df <- shark_df()
  df$value <- as.character(df$value)
  expect_error(create_pie_map(df), "must be numeric")
})

test_that("create_pie_map errors on bad label_col", {
  skip_without_basemap()
  expect_error(create_pie_map(shark_df(), label_col = "no_such_col"),
               "label_col")
})

test_that("create_pie_map respects group_levels ordering", {
  skip_without_basemap()
  lvls <- c("Cyanobacteria", "Diatoms")
  p <- create_pie_map(shark_df(), group_levels = lvls)
  poly_layer <- Filter(function(l) inherits(l$geom, "GeomPolygon"),
                       p$layers)[[1]]
  expect_equal(levels(poly_layer$data$group), lvls)
})

test_that("create_pie_map drops groups not in group_levels", {
  skip_without_basemap()
  p <- create_pie_map(shark_df(),
                      group_levels = c("Diatoms", "Cyanobacteria"))
  fill_scale <- p$scales$get_scales("fill")
  expect_false("Dinoflagellates" %in% fill_scale$limits)
})

test_that("create_pie_map applies custom group_colors", {
  skip_without_basemap()
  cols <- c(Diatoms = "#111111", Dinoflagellates = "#222222",
            Cyanobacteria = "#333333")
  p <- create_pie_map(shark_df(), group_colors = cols)
  fill_scale <- p$scales$get_scales("fill")
  rendered <- fill_scale$palette(3)
  expect_true(all(unname(cols) %in% rendered))
})

test_that("create_pie_map sets title", {
  skip_without_basemap()
  p <- create_pie_map(shark_df(), title = "My map")
  expect_equal(p$labels$title, "My map")
})

test_that("create_pie_map sets legend title", {
  skip_without_basemap()
  p <- create_pie_map(shark_df(), legend_title = "Plankton")
  fill_scale <- p$scales$get_scales("fill")
  expect_equal(fill_scale$name, "Plankton")
})

test_that("create_pie_map aggregates duplicate (station, group) rows", {
  skip_without_basemap()
  df <- rbind(shark_df(), shark_df())
  p  <- create_pie_map(df)
  expect_s3_class(p, "ggplot")
  poly_layer <- Filter(function(l) inherits(l$geom, "GeomPolygon"),
                       p$layers)[[1]]
  n_stations <- length(unique(sub("_\\d+$", "", poly_layer$data$slice_id)))
  expect_equal(n_stations, 3L)
})

test_that("create_pie_map skips stations with zero total value", {
  skip_without_basemap()
  df <- shark_df()
  df$value[df$station_name == "BY29"] <- 0
  p <- create_pie_map(df)
  poly_layer <- Filter(function(l) inherits(l$geom, "GeomPolygon"),
                       p$layers)[[1]]
  n_stations <- length(unique(sub("_\\d+$", "", poly_layer$data$slice_id)))
  expect_equal(n_stations, 2L)
})

test_that("create_pie_map honours xlim / ylim override", {
  skip_without_basemap()
  p <- create_pie_map(shark_df(), xlim = c(10, 22), ylim = c(54, 60))
  coord <- p$coordinates
  expect_equal(coord$limits$x, c(10, 22))
  expect_equal(coord$limits$y, c(54, 60))
})

test_that("create_pie_map shows no labels when show_labels = FALSE", {
  skip_without_basemap()
  p <- create_pie_map(shark_df(), show_labels = FALSE)
  has_text <- any(vapply(p$layers, function(l) inherits(l$geom, "GeomText"),
                         logical(1)))
  expect_false(has_text)
})

test_that("create_pie_map repel = FALSE produces no leader segments with data", {
  skip_without_basemap()
  p <- create_pie_map(close_df(), repel = FALSE)
  seg_layers <- Filter(function(l) inherits(l$geom, "GeomSegment"), p$layers)
  seg_rows <- sum(vapply(seg_layers, function(l) nrow(l$data), integer(1)))
  expect_equal(seg_rows, 0L)
})

test_that("create_pie_map repel = TRUE adds leaders for close stations", {
  skip_without_basemap()
  p <- create_pie_map(close_df(), repel = TRUE, radius = 0.28)
  seg_layers <- Filter(function(l) inherits(l$geom, "GeomSegment"), p$layers)
  expect_gt(length(seg_layers), 0L)
  seg_data <- seg_layers[[1]]$data
  expect_gt(nrow(seg_data), 0L)
})

test_that("create_pie_map size_by = 'total' returns a ggplot", {
  skip_without_basemap()
  p <- create_pie_map(shark_df(), size_by = "total",
                      size_range = c(0.15, 0.40))
  expect_s3_class(p, "ggplot")
})

test_that("create_pie_map size_by = 'total' uses different per-station radii", {
  skip_without_basemap()
  df <- shark_df()
  df$value[df$station_name == "BY5"] <- df$value[df$station_name == "BY5"] * 10
  p <- create_pie_map(df, size_by = "total", size_range = c(0.15, 0.40))
  poly_layer <- Filter(function(l) inherits(l$geom, "GeomPolygon"),
                       p$layers)[[1]]
  by5_x <- poly_layer$data$x[grepl("^1_", poly_layer$data$slice_id)]
  by31_x <- poly_layer$data$x[grepl("^2_", poly_layer$data$slice_id)]
  expect_gt(diff(range(by5_x)), diff(range(by31_x)))
})

test_that("create_pie_map size_by with bad column name errors", {
  skip_without_basemap()
  expect_error(create_pie_map(shark_df(), size_by = "no_such_col"),
               "size_by")
})

test_that("create_pie_map works with a single station", {
  skip_without_basemap()
  df <- shark_df()[shark_df()$station_name == "BY5", ]
  p  <- create_pie_map(df)
  expect_s3_class(p, "ggplot")
  seg_layers <- Filter(function(l) inherits(l$geom, "GeomSegment"), p$layers)
  seg_rows <- sum(vapply(seg_layers, function(l) nrow(l$data), integer(1)))
  expect_equal(seg_rows, 0L)
})

test_that("create_pie_map keeps pies fully within map bounds", {
  skip_without_basemap()
  df <- data.frame(
    station_name = rep(c("EDGE_W", "EDGE_E"), each = 3),
    sample_longitude_dd = rep(c(10.02, 21.98), each = 3),
    sample_latitude_dd = rep(c(54.05, 59.95), each = 3),
    group = rep(c("A", "B", "C"), 2),
    value = c(90, 5, 5, 5, 5, 90),
    stringsAsFactors = FALSE
  )
  p <- create_pie_map(df, size_by = "total", xlim = c(10, 22), ylim = c(54, 60))
  poly_layer <- Filter(function(l) inherits(l$geom, "GeomPolygon"),
                       p$layers)[[1]]
  expect_true(all(poly_layer$data$x >= 10 - 1e-8 & poly_layer$data$x <= 22 + 1e-8))
  expect_true(all(poly_layer$data$y >= 54 - 1e-8 & poly_layer$data$y <= 60 + 1e-8))
})

test_that("create_pie_map uses plain legend text for plain labels", {
  skip_without_basemap()
  df <- data.frame(
    station_name = rep(c("S1", "S2"), each = 4),
    sample_longitude_dd = rep(c(12, 18), each = 4),
    sample_latitude_dd = rep(c(58, 57.5), each = 4),
    group = rep(c("Diatoms", "Dinoflagellates", "Cyanobacteria", "Other"), 2),
    value = c(50, 30, 15, 5, 40, 35, 20, 5),
    stringsAsFactors = FALSE
  )
  p <- create_pie_map(
    df,
    group_labels = c(
      Diatoms = "Diatoms",
      Dinoflagellates = "Dinoflagellates",
      Cyanobacteria = "Cyanobacteria",
      Other = "Other"
    )
  )
  expect_s3_class(p$theme$legend.text, "element_text")
})

test_that("create_pie_map uses markdown legend text for HTML labels", {
  skip_without_basemap()
  testthat::skip_if_not_installed("ggtext")
  df <- data.frame(
    station_name = rep(c("S1", "S2"), each = 2),
    sample_longitude_dd = rep(c(12, 18), each = 2),
    sample_latitude_dd = rep(c(58, 57.5), each = 2),
    group = rep(c("Mesodinium spp.", "Diatoms"), 2),
    value = c(40, 60, 30, 70),
    stringsAsFactors = FALSE
  )
  p <- create_pie_map(
    df,
    group_labels = c(
      `Mesodinium spp.` = "<i>Mesodinium</i> spp.",
      Diatoms           = "Diatoms"
    )
  )
  expect_s3_class(p$theme$legend.text, "element_markdown")
})

test_that("create_pie_map keeps default legend labels when group_labels is NULL", {
  skip_without_basemap()
  df <- data.frame(
    station_name = rep(c("S1", "S2"), each = 4),
    sample_longitude_dd = rep(c(12, 18), each = 4),
    sample_latitude_dd = rep(c(58, 57.5), each = 4),
    group = rep(c("Diatoms", "Dinoflagellates", "Cyanobacteria", "Other"), 2),
    value = c(50, 30, 15, 5, 40, 35, 20, 5),
    stringsAsFactors = FALSE
  )
  p <- create_pie_map(
    df,
    group_levels = c("Diatoms", "Dinoflagellates", "Cyanobacteria", "Other"),
    group_colors = c(
      Diatoms = "#1f77b4",
      Dinoflagellates = "#d62728",
      Cyanobacteria = "#2ca02c",
      Other = "#7f7f7f"
    )
  )
  fill_scale <- p$scales$get_scales("fill")
  expect_s3_class(fill_scale$labels, "waiver")
})

# ── repel_pie_centers (internal) ───────────────────────────────────────────

test_that("repel_pie_centers returns wide unchanged when n = 1", {
  wide <- data.frame(lon = 15, lat = 57, r_pie = 0.28)
  out  <- SHARK4R:::repel_pie_centers(wide)
  expect_equal(out$lon, 15)
  expect_equal(out$lat, 57)
})

test_that("repel_pie_centers pushes two overlapping pies apart", {
  wide <- data.frame(
    lon   = c(15.00, 15.01),
    lat   = c(57.00, 57.01),
    r_pie = c(0.28, 0.28)
  )
  out <- SHARK4R:::repel_pie_centers(wide, min_sep = 2.40, min_disp = 1.60)
  d_before <- sqrt((15.00 - 15.01)^2 + (57.00 - 57.01)^2)
  d_after  <- sqrt((out$lon[1] - out$lon[2])^2 +
                     (out$lat[1] - out$lat[2])^2)
  expect_gt(d_after, d_before)
})

test_that("repel_pie_centers leaves distant stations at their anchors", {
  wide <- data.frame(
    lon   = c(12, 20),
    lat   = c(56, 58),
    r_pie = c(0.28, 0.28)
  )
  out <- SHARK4R:::repel_pie_centers(wide)
  expect_equal(out$lon, wide$lon, tolerance = 1e-6)
  expect_equal(out$lat, wide$lat, tolerance = 1e-6)
})

test_that("repel_pie_centers displaced pie has anchor outside it", {
  wide <- data.frame(
    lon   = c(15.00, 15.01),
    lat   = c(57.00, 57.01),
    r_pie = c(0.28, 0.28)
  )
  out <- SHARK4R:::repel_pie_centers(wide, min_sep = 2.40, min_disp = 1.60)
  for (i in seq_len(nrow(out))) {
    disp <- sqrt((out$lon[i] - out$anchor_lon[i])^2 +
                   (out$lat[i] - out$anchor_lat[i])^2)
    expect_true(disp < 1e-4 || disp >= 1.60 * wide$r_pie[i] - 1e-6)
  }
})

# ── build_pie_polygons (internal) ──────────────────────────────────────────

test_that("build_pie_polygons returns expected columns", {
  wide <- data.frame(lon = 15, lat = 57, r_pie = 0.28,
                     A = 60, B = 40, stringsAsFactors = FALSE)
  out  <- SHARK4R:::build_pie_polygons(wide, c("A", "B"))
  expect_true(all(c("x", "y", "slice_id", "group") %in% names(out)))
})

test_that("build_pie_polygons skips zero-total rows", {
  wide <- data.frame(lon = c(15, 16), lat = c(57, 57), r_pie = c(0.28, 0.28),
                     A = c(50, 0), B = c(50, 0), stringsAsFactors = FALSE)
  out <- SHARK4R:::build_pie_polygons(wide, c("A", "B"))
  expect_true(all(grepl("^1_", out$slice_id)))
})

test_that("build_pie_polygons group column is character", {
  wide <- data.frame(lon = 15, lat = 57, r_pie = 0.28,
                     X = 70, Y = 30, stringsAsFactors = FALSE)
  out <- SHARK4R:::build_pie_polygons(wide, c("X", "Y"))
  expect_type(out$group, "character")
})

# ── place_pie_labels (internal) ────────────────────────────────────────────

test_that("place_pie_labels appends label_x, label_y, hjust, vjust", {
  wide <- data.frame(
    lon   = c(15, 18),
    lat   = c(57, 58),
    r_pie = c(0.28, 0.28),
    r_lon = c(0.28 / cos(57 * pi / 180), 0.28 / cos(58 * pi / 180)),
    label = c("ST1", "ST2"),
    stringsAsFactors = FALSE
  )
  out <- SHARK4R:::place_pie_labels(wide,
                                    map_xlim = c(10, 22),
                                    map_ylim = c(54, 60))
  expect_true(all(c("label_x", "label_y", "hjust", "vjust") %in% names(out)))
  expect_equal(nrow(out), 2L)
})

test_that("place_pie_labels places labels within map bounds", {
  wide <- data.frame(
    lon   = c(14, 16, 18),
    lat   = c(56, 57, 58),
    r_pie = rep(0.28, 3),
    r_lon = 0.28 / cos(c(56, 57, 58) * pi / 180),
    label = c("A", "B", "C"),
    stringsAsFactors = FALSE
  )
  out <- SHARK4R:::place_pie_labels(wide,
                                    map_xlim = c(10, 22),
                                    map_ylim = c(54, 60))
  char_w <- 0.055
  char_h <- 0.055
  for (i in seq_len(nrow(out))) {
    if (!is.finite(out$label_x[i])) next
    hw <- nchar(out$label[i]) * char_w / 2
    lx <- out$label_x[i]
    text_left  <- if (isTRUE(all.equal(out$hjust[i], 0))) lx
                  else if (isTRUE(all.equal(out$hjust[i], 1))) lx - 2 * hw
                  else lx - hw
    text_right <- if (isTRUE(all.equal(out$hjust[i], 0))) lx + 2 * hw
                  else if (isTRUE(all.equal(out$hjust[i], 1))) lx
                  else lx + hw
    expect_true(text_left  >= 10 - 1e-8, info = paste("label", i, "left edge"))
    expect_true(text_right <= 22 + 1e-8, info = paste("label", i, "right edge"))
    expect_true(out$label_y[i] - char_h >= 54 - 1e-8)
    expect_true(out$label_y[i] + char_h <= 60 + 1e-8)
  }
})

test_that("place_pie_labels avoids anchors and leader segments", {
  wide <- data.frame(
    lon = c(12.4, 10.5),
    lat = c(58.2, 58.25),
    r_pie = c(0.55, 0.25),
    r_lon = c(0.55 / cos(58.2 * pi / 180), 0.25 / cos(58.25 * pi / 180)),
    label = c("SLAGGO", "A17"),
    anchor_lon = c(11.55, 10.5),
    anchor_lat = c(58.25, 58.25),
    leader_x = c(11.85, 10.5),
    leader_y = c(58.24, 58.25),
    stringsAsFactors = FALSE
  )
  out <- SHARK4R:::place_pie_labels(
    wide,
    map_xlim = c(10, 13.2),
    map_ylim = c(57.8, 58.8)
  )

  keep <- which(is.finite(out$label_x) & is.finite(out$label_y))
  for (i in keep) {
    hw <- nchar(out$label[i]) * 0.055 / 2
    hh <- 0.055
    left <- out$label_x[i] - hw
    right <- out$label_x[i] + hw
    bottom <- out$label_y[i] - hh
    top <- out$label_y[i] + hh
    expect_false(out$anchor_lon[1] >= left && out$anchor_lon[1] <= right &&
                   out$anchor_lat[1] >= bottom && out$anchor_lat[1] <= top)
  }
})
