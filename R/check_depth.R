#' Check which points have potentially invalid depths.
#'
#' @usage check_depth(data)
#'
#' @param data The data frame.
#' @return Problematic records or an errors report.
#' @export

check_depth <- function(data) {
  if (!"value" %in% names(data)) {
    stop("data must contain a 'value' column")
  }

  vals <- data$value
  # convert factors to character; numeric -> as.character is fine
  vals_chr <- as.character(vals)

  # strict numeric pattern (integers, decimals, optional scientific notation)
  num_pattern <- "^[+-]?(?:[0-9]*\\.?[0-9]+)([eE][+-]?[0-9]+)?$"

  # determine which entries match numeric pattern (safe for NA)
  num_ok <- grepl(num_pattern, vals_chr)
  num_ok[is.na(num_ok)] <- FALSE

  non_numeric_idx <- !num_ok

  if (any(non_numeric_idx)) {
    message("ERROR: Expected numerical value is formatted as character")
    message("Common characters are e.g. less than '<' or greater than '>' signs or letters.")
    matches <- unique(vals_chr[non_numeric_idx])
    # make a 2D object for DT
    matches_df <- data.frame(value = matches, stringsAsFactors = FALSE)
    return(DT::datatable(matches_df))
  } else {
    message("Expected numerical value is formatted as numerical and no character values were found")
    invisible(NULL)
  }
}
