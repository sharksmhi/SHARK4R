#' Clamp pie chart centres to lie inside the map panel
#'
#' @param wide Data frame with columns `lon`, `lat` and optional `r_pie`.
#' @param map_xlim,map_ylim Numeric length-2 vectors bounding the pie centre.
#' @return `wide` with clamped `lon`/`lat`.
#' @keywords internal
clamp_pie_centers <- function(wide,
                              map_xlim = c(10.5, 21.5),
                              map_ylim = c(54.2, 59.8)) {
  if (nrow(wide) == 0) return(wide)

  clamp_axis <- function(value, radius, lo, hi) {
    if (!is.finite(radius) || radius <= 0) {
      return(min(max(value, lo), hi))
    }
    inner_lo <- lo + radius
    inner_hi <- hi - radius
    if (inner_lo > inner_hi) {
      return((lo + hi) / 2)
    }
    min(max(value, inner_lo), inner_hi)
  }

  r_lat <- if ("r_pie" %in% names(wide)) wide$r_pie else rep(0, nrow(wide))
  r_lon <- r_lat / cos(wide$lat * pi / 180)

  wide$lon <- vapply(seq_len(nrow(wide)), function(i) {
    clamp_axis(wide$lon[i], r_lon[i], map_xlim[1], map_xlim[2])
  }, numeric(1L))
  wide$lat <- vapply(seq_len(nrow(wide)), function(i) {
    clamp_axis(wide$lat[i], r_lat[i], map_ylim[1], map_ylim[2])
  }, numeric(1L))
  wide
}

#' Displace pie chart centres away from each other
#'
#' Sequential, asymmetric placement so that pie charts never overlap and any
#' pie that *does* get displaced is pushed far enough that its anchor (true
#' station location) lands outside the pie boundary, so the leader line and
#' anchor dot remain visible. Geometry is computed in an isotropic coordinate
#' system \eqn{(lon \cdot \cos(\bar{lat}), lat)} so that visual distance on a
#' `coord_sf` map corresponds approximately to Euclidean distance here.
#'
#' @param wide Data frame with columns `lon`, `lat` and `r_pie`
#'   (the per-station pie radius in latitude degrees). True (anchor)
#'   coordinates are read from `lon`/`lat`.
#' @param map_xlim,map_ylim Numeric length-2 vectors bounding the pie centre.
#' @param min_sep Minimum centre-to-centre separation between two pies,
#'   expressed as a multiple of the larger of the two radii. Default `2.40`.
#' @param min_disp Minimum displacement for a pie that is moved at all,
#'   expressed as a multiple of its radius. Default `1.60`.
#' @return `wide` with added columns `anchor_lon`, `anchor_lat`
#'   (the true station coordinates) and updated `lon`/`lat`
#'   holding the displaced pie centres.
#' @keywords internal
repel_pie_centers <- function(wide,
                              map_xlim = c(10.5, 21.5),
                              map_ylim = c(54.2, 59.8),
                              min_sep  = 2.40,
                              min_disp = 1.60) {
  n <- nrow(wide)
  anchor_lon <- wide$lon
  anchor_lat <- wide$lat
  wide$anchor_lon <- anchor_lon
  wide$anchor_lat <- anchor_lat
  if (n < 2) return(wide)

  r_pie  <- wide$r_pie
  cos_ref <- cos(mean(anchor_lat) * pi / 180)
  ax <- anchor_lon * cos_ref
  ay <- anchor_lat
  x_lo <- map_xlim[1] * cos_ref
  x_hi <- map_xlim[2] * cos_ref

  pair_min_d <- function(i, j) min_sep * max(r_pie[i], r_pie[j])

  neighbours <- integer(n)
  for (i in seq_len(n)) {
    neighbours[i] <- sum(vapply(seq_len(n), function(j) {
      if (i == j) return(0L)
      d <- sqrt((ax[i] - ax[j])^2 + (ay[i] - ay[j])^2)
      as.integer(d < pair_min_d(i, j))
    }, integer(1L)))
  }
  boundary_margin <- vapply(seq_len(n), function(i) {
    min(
      (ax[i] - x_lo)        / r_pie[i],
      (x_hi  - ax[i])       / r_pie[i],
      (ay[i] - map_ylim[1]) / r_pie[i],
      (map_ylim[2] - ay[i]) / r_pie[i]
    )
  }, numeric(1L))
  priority  <- neighbours + 1 / pmax(boundary_margin, 0.5)
  order_idx <- order(priority, decreasing = TRUE)

  x <- rep(NA_real_, n)
  y <- rep(NA_real_, n)

  clamp_xy <- function(i, xi, yi) {
    r_x <- r_pie[i] * cos_ref / cos(anchor_lat[i] * pi / 180)
    r_y <- r_pie[i]
    x_min <- x_lo + r_x
    x_max <- x_hi - r_x
    y_min <- map_ylim[1] + r_y
    y_max <- map_ylim[2] - r_y

    if (x_min > x_max) {
      xi <- (x_lo + x_hi) / 2
    } else {
      xi <- min(max(xi, x_min), x_max)
    }
    if (y_min > y_max) {
      yi <- mean(map_ylim)
    } else {
      yi <- min(max(yi, y_min), y_max)
    }
    c(xi, yi)
  }

  enforce_min_disp <- function(xi, yi, axi, ayi, min_disp_d) {
    dx <- xi - axi
    dy <- yi - ayi
    d  <- sqrt(dx * dx + dy * dy)
    if (d > 1e-6 && d < min_disp_d) {
      s  <- min_disp_d / d
      xi <- axi + dx * s
      yi <- ayi + dy * s
    }
    c(xi, yi)
  }

  for (rank in seq_along(order_idx)) {
    i  <- order_idx[rank]
    xi <- ax[i]
    yi <- ay[i]
    min_disp_d_i <- min_disp * r_pie[i]

    for (iter in seq_len(80L)) {
      moved <- FALSE
      for (k in seq_len(rank - 1L)) {
        j  <- order_idx[k]
        dx <- xi - x[j]
        dy <- yi - y[j]
        d  <- sqrt(dx * dx + dy * dy)
        min_d_ij <- pair_min_d(i, j)
        if (d < 1e-6) {
          dx <- ax[i] - x[j]
          dy <- ay[i] - y[j]
          d  <- sqrt(dx * dx + dy * dy)
          if (d < 1e-6) { dx <- 1; dy <- 0; d <- 1 }
        }
        if (d < min_d_ij) {
          ux <- dx / d
          uy <- dy / d
          push <- (min_d_ij - d) + 1e-4
          xi <- xi + ux * push
          yi <- yi + uy * push
          moved <- TRUE
        }
      }
      if (!moved) break
    }

    xy <- enforce_min_disp(xi, yi, ax[i], ay[i], min_disp_d_i)
    xi <- xy[1L]; yi <- xy[2L]
    xy <- clamp_xy(i, xi, yi)
    xi <- xy[1L]; yi <- xy[2L]

    for (iter in seq_len(40L)) {
      moved <- FALSE
      for (k in seq_len(rank - 1L)) {
        j  <- order_idx[k]
        dx <- xi - x[j]
        dy <- yi - y[j]
        d  <- sqrt(dx * dx + dy * dy)
        min_d_ij <- pair_min_d(i, j)
        if (d < min_d_ij) {
          ux <- dx / max(d, 1e-6)
          uy <- dy / max(d, 1e-6)
          push <- (min_d_ij - d) + 1e-4
          xi <- xi + ux * push
          yi <- yi + uy * push
          moved <- TRUE
        }
      }
      xy <- clamp_xy(i, xi, yi)
      xi <- xy[1L]; yi <- xy[2L]
      if (!moved) break
    }

    x[i] <- xi
    y[i] <- yi
  }

  wide$lon <- x / cos_ref
  wide$lat <- y
  wide
}

#' Find label positions that avoid all pie charts and each other
#'
#' Uses a greedy sequential algorithm: stations are processed most-constrained
#' first, and each label is placed in the direction that maximises clearance
#' from all pie circles *and* from already-placed label bounding boxes.
#'
#' @param wide Data frame with one row per station, columns
#'   `lon`, `lat`, `r_pie`, `r_lon`, and `label`.
#' @param map_xlim,map_ylim Numeric length-2 vectors giving the allowed label
#'   anchor range.
#' @param n_angles Number of candidate directions to test per station.
#' @param char_w Estimated label width per character in longitude degrees.
#' @param char_h Estimated label half-height in latitude degrees.
#' @param radius_mults Candidate radial distances in units of pie radius.
#' @return `wide` with columns `label_x`, `label_y`, `hjust`, `vjust` appended.
#' @keywords internal
place_pie_labels <- function(wide,
                             map_xlim = c(10.8, 21.2),
                             map_ylim = c(54.4, 59.6),
                             n_angles = 48,
                             char_w   = 0.055,
                             char_h   = 0.055,
                             radius_mults = c(1.08, 1.25, 1.55, 1.9,
                                              2.4, 3.0, 3.8, 4.8)) {
  angles <- seq(0, 2 * pi, length.out = n_angles + 1L)[-(n_angles + 1L)]
  lons   <- wide$lon
  lats   <- wide$lat
  r_lats <- wide$r_pie
  r_lons <- wide$r_lon
  labels <- wide$label
  n      <- nrow(wide)
  anchor_lon <- if ("anchor_lon" %in% names(wide)) wide$anchor_lon else lons
  anchor_lat <- if ("anchor_lat" %in% names(wide)) wide$anchor_lat else lats
  leader_x <- if ("leader_x" %in% names(wide)) wide$leader_x else lons
  leader_y <- if ("leader_y" %in% names(wide)) wide$leader_y else lats

  neighbour_count <- vapply(seq_len(n), function(i) {
    sum(vapply(seq_len(n), function(j) {
      if (i == j) return(0L)
      dx <- (lons[i] - lons[j]) / r_lons[j]
      dy <- (lats[i] - lats[j]) / r_lats[j]
      as.integer(sqrt(dx^2 + dy^2) < 5)
    }, integer(1L)))
  }, integer(1L))
  order_idx <- order(neighbour_count, decreasing = TRUE)

  label_x  <- numeric(n)
  label_y  <- numeric(n)
  placed   <- vector("list", n)

  point_in_rect <- function(px, py, left, right, bottom, top) {
    px >= left && px <= right && py >= bottom && py <= top
  }

  segments_intersect <- function(x1, y1, x2, y2, x3, y3, x4, y4) {
    orient <- function(ax, ay, bx, by, cx, cy) {
      (bx - ax) * (cy - ay) - (by - ay) * (cx - ax)
    }
    on_segment <- function(ax, ay, bx, by, cx, cy) {
      min(ax, bx) <= cx && cx <= max(ax, bx) &&
        min(ay, by) <= cy && cy <= max(ay, by)
    }

    o1 <- orient(x1, y1, x2, y2, x3, y3)
    o2 <- orient(x1, y1, x2, y2, x4, y4)
    o3 <- orient(x3, y3, x4, y4, x1, y1)
    o4 <- orient(x3, y3, x4, y4, x2, y2)

    if (((o1 > 0 && o2 < 0) || (o1 < 0 && o2 > 0)) &&
        ((o3 > 0 && o4 < 0) || (o3 < 0 && o4 > 0))) {
      return(TRUE)
    }

    tol <- 1e-10
    if (abs(o1) <= tol && on_segment(x1, y1, x2, y2, x3, y3)) return(TRUE)
    if (abs(o2) <= tol && on_segment(x1, y1, x2, y2, x4, y4)) return(TRUE)
    if (abs(o3) <= tol && on_segment(x3, y3, x4, y4, x1, y1)) return(TRUE)
    if (abs(o4) <= tol && on_segment(x3, y3, x4, y4, x2, y2)) return(TRUE)
    FALSE
  }

  segment_hits_rect <- function(x1, y1, x2, y2, left, right, bottom, top) {
    if (point_in_rect(x1, y1, left, right, bottom, top) ||
        point_in_rect(x2, y2, left, right, bottom, top)) {
      return(TRUE)
    }
    if (max(x1, x2) < left || min(x1, x2) > right ||
        max(y1, y2) < bottom || min(y1, y2) > top) {
      return(FALSE)
    }
    edges <- rbind(
      c(left, bottom, right, bottom),
      c(right, bottom, right, top),
      c(right, top, left, top),
      c(left, top, left, bottom)
    )
    any(vapply(seq_len(nrow(edges)), function(k) {
      segments_intersect(x1, y1, x2, y2,
                         edges[k, 1], edges[k, 2], edges[k, 3], edges[k, 4])
    }, logical(1L)))
  }

  for (rank in seq_along(order_idx)) {
    i      <- order_idx[rank]
    hw_lon <- nchar(labels[i]) * char_w / 2

    best_score    <- -Inf
    best_lx       <- NA_real_
    best_ly       <- NA_real_
    best_center_x <- NA_real_

    for (theta in angles) { for (radius_mult in radius_mults) {
      lx <- lons[i] + r_lons[i] * sin(theta) * radius_mult
      ly <- lats[i] + r_lats[i] * cos(theta) * radius_mult

      s_theta <- sin(theta)
      left   <- if (s_theta >  1e-6) lx              else
                if (s_theta < -1e-6) lx - 2 * hw_lon else lx - hw_lon
      right  <- if (s_theta >  1e-6) lx + 2 * hw_lon else
                if (s_theta < -1e-6) lx              else lx + hw_lon
      center_x <- (left + right) / 2
      bottom <- ly - char_h
      top    <- ly + char_h

      if (left < map_xlim[1] || right > map_xlim[2] ||
          bottom < map_ylim[1] || top > map_ylim[2]) next

      pie_score <- min(vapply(seq_len(n), function(j) {
        r_lon_j <- r_lons[j]
        r_lat_j <- r_lats[j]
        cx_clamp <- min(max(lons[j], left), right)
        cy_clamp <- min(max(lats[j], bottom), top)
        dx <- (cx_clamp - lons[j]) / r_lon_j
        dy <- (cy_clamp - lats[j]) / r_lat_j
        sqrt(dx^2 + dy^2) - 1.0
      }, numeric(1L)))

      label_score <- if (rank <= 1L) Inf else
        min(vapply(seq_len(rank - 1L), function(k) {
          b <- placed[[order_idx[k]]]
          if (is.null(b)) return(Inf)
          gap_x <- abs(center_x - b[1L]) - (hw_lon + b[3L])
          gap_y <- abs(ly       - b[2L]) - (char_h  + b[4L])
          min(gap_x, gap_y)
        }, numeric(1L)))

      is_clear <- pie_score > 0.02 && label_score >= 0
      segment_penalty <- 0
      if (is_clear) {
        anchor_ok <- !any(vapply(seq_len(n), function(j) {
          point_in_rect(anchor_lon[j], anchor_lat[j], left, right, bottom, top)
        }, logical(1L)))
        segment_penalty <- sum(vapply(seq_len(n), function(j) {
          as.integer(segment_hits_rect(anchor_lon[j], anchor_lat[j],
                                       leader_x[j], leader_y[j],
                                       left, right, bottom, top))
        }, integer(1L)))
        is_clear <- anchor_ok
      }
      if (!is_clear) next
      score <- if (is_clear) {
        side_pref <- abs(s_theta)
        100 - radius_mult * 10 + pmin(pie_score, 3) +
          side_pref * 0.25 - segment_penalty * 1.5
      } else {
        pie_score + label_score * 3 - radius_mult * 0.1
      }

      if (score > best_score) {
        best_score    <- score
        best_lx       <- lx
        best_ly       <- ly
        best_center_x <- center_x
      }
    } }

    label_x[i] <- best_lx
    label_y[i] <- best_ly
    if (is.finite(best_lx) && is.finite(best_ly)) {
      placed[[i]] <- c(best_center_x, best_ly, hw_lon, char_h)
    }
  }

  wide$label_x <- label_x
  wide$label_y <- label_y
  adx          <- wide$label_x - lons
  ady          <- wide$label_y - lats
  wide$hjust   <- (1 - sign(adx)) / 2
  wide$vjust   <- (1 - sign(ady)) / 2
  wide
}

#' Build pie-chart polygon data for use with geom_polygon
#'
#' @param wide_data Data frame with one row per station, columns
#'   `lon`, `lat`, `r_pie`, and one numeric column per group.
#' @param group_cols Character vector of column names holding group values.
#' @param n_arc Number of arc points per full circle.
#' @return A data frame with columns `x`, `y`, `slice_id`, and `group`.
#' @keywords internal
build_pie_polygons <- function(wide_data, group_cols, n_arc = 80) {
  pieces <- vector("list", nrow(wide_data) * length(group_cols))
  k <- 0L
  for (i in seq_len(nrow(wide_data))) {
    lon    <- wide_data$lon[i]
    lat    <- wide_data$lat[i]
    r_lat  <- wide_data$r_pie[i]
    r_lon  <- r_lat / cos(lat * pi / 180)
    values <- unlist(wide_data[i, group_cols])
    total  <- sum(values, na.rm = TRUE)
    if (!is.finite(total) || total <= 0) next
    props  <- values / total
    ends   <- cumsum(props) * 2 * pi
    starts <- c(0, utils::head(ends, -1))
    for (j in seq_along(group_cols)) {
      if (props[j] <= 0) next
      k <- k + 1L
      n_pts <- max(3L, ceiling(n_arc * props[j]))
      theta  <- seq(starts[j], ends[j], length.out = n_pts)
      pieces[[k]] <- data.frame(
        x        = c(lon, lon + r_lon * sin(theta), lon),
        y        = c(lat, lat + r_lat * cos(theta), lat),
        slice_id = paste0(i, "_", j),
        group    = group_cols[j],
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, pieces[seq_len(k)])
}

scale_pie_radii <- function(raw, size_range = c(0.15, 0.40)) {
  raw <- as.numeric(raw)
  if (all(!is.finite(raw)) || max(raw, na.rm = TRUE) <= 0) {
    return(rep(mean(size_range), length(raw)))
  }
  s <- sqrt(pmax(raw, 0))
  s <- (s - min(s, na.rm = TRUE)) /
    max(diff(range(s, na.rm = TRUE)), .Machine$double.eps)
  size_range[1] + s * (size_range[2] - size_range[1])
}

#' Pie chart map with displacement and leader lines
#'
#' Draws a pie chart at each station on a map. When pies would overlap in
#' crowded regions, the pies are displaced asymmetrically away from their
#' true station coordinates and a leader line + anchor dot is drawn so the
#' viewer can still tell which pie belongs to which station. Works with any
#' grouping (phytoplankton groups, zooplankton orders, microbial phyla, ...)
#' and any numeric value (biomass, biovolume, abundance, ...).
#'
#' @param data A long-format data.frame with one row per
#'   (station, group). Required columns are configurable through the
#'   `*_col` arguments and default to `station`, `lon`,
#'   `lat`, `group`, `value`.
#' @param station_col,lon_col,lat_col,group_col,value_col Column names in
#'   `data`. Defaults match SHARK conventions:
#'   `"station_name"`, `"sample_longitude_dd"`,
#'   `"sample_latitude_dd"`, `"group"`, `"value"`.
#' @param label_col Column to use for the on-map station label. Defaults to
#'   `station_col`. Set to `NULL` (and `show_labels = FALSE`)
#'   to omit labels entirely.
#' @param group_levels Optional character vector controlling the legend and
#'   slice ordering. Groups not present in `data` are dropped.
#' @param group_colors Optional named character vector of colours, keyed by
#'   group name. If `NULL`, ggplot's default discrete palette is used.
#' @param group_labels Optional named character vector of legend labels,
#'   keyed by group name. Labels may include HTML markup (requires the
#'   `ggtext` package to render).
#' @param radius Pie radius in latitude degrees. Default `0.28`.
#' @param size_by Optional. `NULL` (default) draws all pies at
#'   `radius`. `"total"` scales each pie's radius by the square
#'   root of the station's total value. Any other character value is
#'   interpreted as the name of a numeric per-station column in `data`
#'   (e.g. chlorophyll, secchi depth); values are averaged per station
#'   when duplicated across rows.
#' @param size_range Numeric length-2: minimum and maximum radius (in
#'   latitude degrees) when `size_by` is set. Default `c(0.15, 0.40)`.
#' @param repel Logical. Run the displacement algorithm? Default `TRUE`.
#' @param min_sep Minimum centre-to-centre separation between two pies,
#'   expressed as a multiple of the larger of the two radii. Default `2.40`.
#' @param min_disp Minimum displacement for a pie that has been moved at
#'   all, as a multiple of its radius. Default `1.60`.
#' @param show_labels Logical. Draw station labels next to each pie?
#' @param label_size ggplot text size for the station labels.
#' @param pie_border_color,pie_border_width Aesthetics for the slice borders.
#' @param leader_color,leader_width Aesthetics for the leader segments
#'   drawn from anchor to displaced pie edge.
#' @param anchor_color,anchor_fill,anchor_size Aesthetics for the dot drawn
#'   at the true station location of each displaced pie.
#' @param basemap Optional ggplot layer (or list of layers) used as the
#'   base map. If `NULL`, a coastline polygon from `rnaturalearth`
#'   is drawn (requires the `rnaturalearth` package).
#' @param basemap_scale Resolution passed to
#'   `rnaturalearth::ne_countries()` when `basemap` is `NULL`.
#'   One of `"small"`, `"medium"` or `"large"`.
#' @param basemap_fill,basemap_border,sea_color Colours for the default
#'   coastline basemap.
#' @param xlim,ylim Optional numeric length-2 vectors. If supplied they
#'   override the auto-fitted map extent.
#' @param pad Padding (in degrees) added around station bounds when
#'   auto-fitting the extent.
#' @param title,legend_title Optional plot title and legend title.
#' @return A `ggplot` object.
#' @examples
#' # Six SHARK monitoring stations spanning the Swedish west coast,
#' # Kattegat, and Baltic Proper. Note that SLÄGGÖ and Å17 sit close
#' # enough on the Skagerrak shelf that their pies will be repelled and
#' # drawn with leader lines.
#' stations <- dplyr::tibble(
#'   station_name = rep(c("SLÄGGÖ", "Å17", "ANHOLT E", "BY2 ARKONA",
#'                        "BY31 LANDSORTSDJ", "BY38 KARLSÖDJ"), each = 4),
#'   sample_latitude_dd  = rep(c(58.25984, 58.28434, 56.66866, 54.97116,
#'                               58.59366, 57.11717), each = 4),
#'   sample_longitude_dd = rep(c(11.43567, 10.50432, 12.11117, 14.09883,
#'                               18.23633, 17.66867), each = 4),
#'   group = rep(c("Diatoms", "Dinoflagellates",
#'                 "Cyanobacteria", "Other"), 6),
#'   value = c( 60, 25, 10,  5,   # SLÄGGÖ
#'              55, 30,  5, 10,   # Å17
#'              40, 45,  5, 10,   # ANHOLT E
#'              15, 20, 55, 10,   # BY2 ARKONA
#'              20, 25, 45, 10,   # BY31 LANDSORTSDJ
#'              25, 20, 45, 10)   # BY38 KARLSÖDJ
#' )
#'
#' # The default basemap uses `rnaturalearth` + `rnaturalearthdata` (both
#' # Suggests); each example below is guarded by a single-line check so the
#' # plots render inline rather than all at once after a large `if` block.
#' has_basemap <- requireNamespace("rnaturalearth",     quietly = TRUE) &&
#'                requireNamespace("rnaturalearthdata", quietly = TRUE)
#'
#' \donttest{
#' # 1. Uniform pie size, default palette, automatic map extent.
#' if (has_basemap) create_pie_map(stations)
#'
#' # 2. Scale pie radius by the station's total value (`size_by = "total"`):
#' #    stations with larger summed biomass get bigger pies. `size_range`
#' #    controls the smallest/largest radius (in latitude degrees). Pie
#' #    sizes are relative within the plot only; no size legend is drawn.
#' if (has_basemap) create_pie_map(
#'   stations,
#'   size_by      = "total",
#'   size_range   = c(0.20, 0.55),
#'   group_colors = c(Diatoms         = "#4A90D9",
#'                    Dinoflagellates = "#E74C3C",
#'                    Cyanobacteria   = "#14B8A6",
#'                    Other           = "#95A5A6"),
#'   legend_title = "Taxon group",
#'   title        = "Phytoplankton composition (pie size = total biomass)"
#' )
#'
#' # 3. Scale pie radius by an external per-station metric. Here we add a
#' #    fake chlorophyll-a column and pass its name to `size_by` - any
#' #    numeric per-station column in `data` works (e.g. secchi depth,
#' #    cell counts, nutrient concentrations).
#' stations$chla <- rep(c(3.1, 2.8, 4.2, 1.1, 1.5, 1.3), each = 4)
#' if (has_basemap) create_pie_map(
#'   stations,
#'   size_by      = "chla",
#'   size_range   = c(0.18, 0.50),
#'   legend_title = "Taxon group",
#'   title        = "Pie size scaled by chlorophyll-a"
#' )
#' }
#' @export
create_pie_map <- function(data,
                           station_col   = "station_name",
                           lon_col       = "sample_longitude_dd",
                           lat_col       = "sample_latitude_dd",
                           group_col     = "group",
                           value_col     = "value",
                           label_col     = station_col,
                           group_levels  = NULL,
                           group_colors  = NULL,
                           group_labels  = NULL,
                           radius        = 0.28,
                           size_by       = NULL,
                           size_range    = c(0.15, 0.40),
                           repel         = TRUE,
                           min_sep       = 2.40,
                           min_disp      = 1.60,
                           show_labels   = TRUE,
                           label_size    = 3,
                           pie_border_color = "white",
                           pie_border_width = 0.3,
                           leader_color  = "gray20",
                           leader_width  = 0.5,
                           anchor_color  = "gray10",
                           anchor_fill   = "white",
                           anchor_size   = 1.8,
                           basemap       = NULL,
                           basemap_scale = "medium",
                           basemap_fill  = "gray95",
                           basemap_border = "gray70",
                           sea_color     = "aliceblue",
                           xlim          = NULL,
                           ylim          = NULL,
                           pad           = 1.0,
                           title         = NULL,
                           legend_title  = "Group") {
  required <- c(station_col, lon_col, lat_col, group_col, value_col)
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop("`data` is missing required columns: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  if (!is.numeric(data[[value_col]])) {
    stop("`", value_col, "` must be numeric.", call. = FALSE)
  }
  if (!is.null(label_col) && !label_col %in% names(data)) {
    stop("`label_col` ('", label_col, "') not found in `data`.",
         call. = FALSE)
  }

  long <- data.frame(
    station = as.character(data[[station_col]]),
    lon     = as.numeric(data[[lon_col]]),
    lat     = as.numeric(data[[lat_col]]),
    group   = as.character(data[[group_col]]),
    value   = as.numeric(data[[value_col]]),
    label   = if (!is.null(label_col)) as.character(data[[label_col]])
              else as.character(data[[station_col]]),
    stringsAsFactors = FALSE
  )
  long <- long[!is.na(long$value), , drop = FALSE]
  long <- stats::aggregate(
    value ~ station + lon + lat + group + label,
    data = long, FUN = sum, na.rm = TRUE
  )

  if (is.null(group_levels)) {
    group_levels <- sort(unique(long$group))
  }
  long <- long[long$group %in% group_levels, , drop = FALSE]
  if (nrow(long) == 0) {
    stop("No rows left after filtering by `group_levels`.", call. = FALSE)
  }

  wide <- tidyr::pivot_wider(
    long,
    id_cols     = c("station", "lon", "lat", "label"),
    names_from  = "group",
    values_from = "value",
    values_fill = 0
  )
  wide <- as.data.frame(wide)
  for (g in group_levels) if (!g %in% names(wide)) wide[[g]] <- 0
  group_cols <- intersect(group_levels, names(wide))

  if (is.null(size_by)) {
    wide$r_pie <- radius
  } else {
    raw <- if (identical(size_by, "total")) {
      rowSums(wide[, group_cols, drop = FALSE], na.rm = TRUE)
    } else if (size_by %in% names(wide)) {
      wide[[size_by]]
    } else if (size_by %in% names(data) && is.numeric(data[[size_by]])) {
      # Per-station external metric (e.g. chlorophyll). Take the mean if a
      # station has multiple rows so the result is one value per station.
      metric <- stats::aggregate(
        list(.metric = as.numeric(data[[size_by]])),
        by   = list(station = as.character(data[[station_col]])),
        FUN  = mean,
        na.rm = TRUE
      )
      metric$.metric[match(wide$station, metric$station)]
    } else {
      stop("`size_by` must be NULL, 'total', or the name of a numeric ",
           "column in `data`.", call. = FALSE)
    }
    raw <- as.numeric(raw)
    wide$r_pie <- scale_pie_radii(raw, size_range = size_range)
  }

  if (is.null(xlim)) xlim <- range(wide$lon) + c(-pad, pad)
  if (is.null(ylim)) ylim <- range(wide$lat) + c(-pad, pad)
  wide <- clamp_pie_centers(wide, map_xlim = xlim, map_ylim = ylim)

  if (isTRUE(repel) && nrow(wide) > 1) {
    wide <- repel_pie_centers(
      wide,
      map_xlim = xlim, map_ylim = ylim,
      min_sep  = min_sep, min_disp = min_disp
    )
  } else {
    wide$anchor_lon <- wide$lon
    wide$anchor_lat <- wide$lat
  }
  wide <- clamp_pie_centers(wide, map_xlim = xlim, map_ylim = ylim)

  move_thresh <- 0.05 * radius
  wide$is_displaced <-
    abs(wide$lon - wide$anchor_lon) > move_thresh |
    abs(wide$lat - wide$anchor_lat) > move_thresh

  cos_lat <- cos(wide$lat * pi / 180)
  dx_iso  <- (wide$anchor_lon - wide$lon) * cos_lat
  dy_iso  <- (wide$anchor_lat - wide$lat)
  d_iso   <- sqrt(dx_iso^2 + dy_iso^2)
  d_iso[d_iso < 1e-9] <- 1
  wide$leader_x <- wide$lon + (wide$r_pie * dx_iso / d_iso) / cos_lat
  wide$leader_y <- wide$lat + (wide$r_pie * dy_iso / d_iso)

  pie_data <- build_pie_polygons(wide, group_cols)
  pie_data$group <- factor(pie_data$group, levels = group_levels)

  wide$r_lon <- wide$r_pie / cos(wide$lat * pi / 180)
  if (isTRUE(show_labels)) {
    label_char_w <- min(max(diff(xlim) * 0.0046, 0.014), 0.055)
    label_char_h <- min(max(diff(ylim) * 0.0092, 0.024), 0.055)
    wide <- place_pie_labels(
      wide,
      map_xlim = xlim,
      map_ylim = ylim,
      char_w = label_char_w,
      char_h = label_char_h
    )
  }
  displaced <- wide[wide$is_displaced, , drop = FALSE]

  p <- ggplot2::ggplot()

  if (is.null(basemap)) {
    if (!requireNamespace("rnaturalearth", quietly = TRUE)) {
      stop("The `rnaturalearth` package is required for the default basemap. ",
           "Install it with install.packages(\"rnaturalearth\") or pass a ",
           "custom `basemap` layer.", call. = FALSE)
    }
    world <- rnaturalearth::ne_countries(scale = basemap_scale,
                                         returnclass = "sf")
    p <- p + ggplot2::geom_sf(data = world, fill = basemap_fill,
                              color = basemap_border)
  } else {
    p <- p + basemap
  }

  p <- p +
    ggplot2::coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    ggplot2::geom_polygon(
      data  = pie_data,
      ggplot2::aes(x = .data$x, y = .data$y,
                   group = .data$slice_id, fill = .data$group),
      color = pie_border_color, linewidth = pie_border_width
    ) +
    ggplot2::geom_segment(
      data = displaced,
      ggplot2::aes(x    = .data$anchor_lon,
                   y    = .data$anchor_lat,
                   xend = .data$leader_x,
                   yend = .data$leader_y),
      color = leader_color, linewidth = leader_width
    ) +
    ggplot2::geom_point(
      data = displaced,
      ggplot2::aes(x = .data$anchor_lon, y = .data$anchor_lat),
      color = anchor_color, fill = anchor_fill, shape = 21,
      size = anchor_size, stroke = 0.6
    )

  if (!is.null(group_colors)) {
    if (is.null(group_labels)) {
      p <- p + ggplot2::scale_fill_manual(values = group_colors,
                                          name = legend_title, drop = TRUE)
    } else {
      p <- p + ggplot2::scale_fill_manual(values = group_colors,
                                          labels = group_labels,
                                          name = legend_title, drop = TRUE)
    }
  } else {
    if (is.null(group_labels)) {
      p <- p + ggplot2::scale_fill_discrete(name = legend_title, drop = TRUE)
    } else {
      p <- p + ggplot2::scale_fill_discrete(labels = group_labels,
                                            name = legend_title, drop = TRUE)
    }
  }

  uses_markdown_labels <- !is.null(group_labels) &&
    any(grepl("<[^>]+>", unname(group_labels)))

  if (isTRUE(show_labels)) {
    p <- p + ggplot2::geom_text(
      data = wide,
      ggplot2::aes(x = .data$label_x, y = .data$label_y,
                   label = .data$label,
                   hjust = .data$hjust, vjust = .data$vjust),
      size = label_size,
      na.rm = TRUE
    )
  }

  legend_text_element <- if (uses_markdown_labels) {
    if (!requireNamespace("ggtext", quietly = TRUE)) {
      stop("Rendering HTML in `group_labels` requires the `ggtext` package. ",
           "Install it with install.packages(\"ggtext\") or supply plain-text ",
           "labels.", call. = FALSE)
    }
    ggtext::element_markdown()
  } else {
    ggplot2::element_text()
  }

  p <- p +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = sea_color,
                                               color = NA),
      axis.title = ggplot2::element_blank(),
      legend.position = "right",
      legend.key.size = ggplot2::unit(0.6, "cm"),
      legend.key.height = ggplot2::unit(0.9, "cm"),
      legend.title = ggplot2::element_text(hjust = 0.5),
      legend.text = legend_text_element
    )

  if (!is.null(title)) p <- p + ggplot2::ggtitle(title)
  p
}
