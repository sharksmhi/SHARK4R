#' Check which points have potentially invalid depths.
#'
#' @usage check_depth(data)
#'
#' @param data The data frame.
#' @return Problematic records or an errors report.
#' @export

check_depth <- function(data) {
  
  if (any(is.character(data$value)==TRUE)) {
    message("ERROR: Expected numerical value is formatted as character")
    message("Common characters are e.g. less than "<" sign:")
    toMatch <- c("<", ">", "[A-z]")
    matches <- unique(grep(paste(toMatch,collapse="|"), 
                           data$value, value=TRUE))
    return(DT::datatable(matches))
  }
  else {
    message("Expected numerical value is formatted as numerical and no character values were found")
  }
}
