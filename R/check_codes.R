#' Check matches of reported project (PROJ) codes in SMHIs codelist
#' @param data for tibble be be checked
#' @return unmatched codes with true or false results 
#' @export

check_code_proj <- function(data) {
  
  toMatch <- unique(data$sample_project_name_sv)
  shark_codes <- read_delim(system.file("extdata", "codelist_SMHI.txt", package = "SHARK4R"), skip = 1, delim ="\t", guess_max = 2000, col_names = T, locale = readr::locale(encoding = "latin1"))
  shark_proj_codes <- shark_codes %>% 
    filter(Data_field == "PROJ")
  match_type <- toMatch %in% shark_proj_codes$Code
  matches <- data.frame(reported_PROJ_code = unique(data$sample_project_name_sv), match_type = match_type)
  
  if (length(which(match_type == FALSE)) > 0) {
    message("ERROR: Unmatched Project (PROJ) code found")
    print(matches[!match_type,])
  }
    else {
      message("All project (PROJ) codes found")
    }
}