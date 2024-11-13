#' @importFrom rappdirs user_cache_dir
#' @importFrom digest digest

missing_fields <- function(data, fields) {
  missing <- !(fields %in% names(data))
  return(fields[missing])
}

missing_values <- function(data) {
  return(data %in% c(NA, ""))
}

check_lonlat <- function(data, report) {
  errors <- c()
  if (!"sample_longitude_dd" %in% names(data)) {
    errors <- c(errors, "Column sample_longitude_dd missing")
  } else if (!is.numeric(data$sample_longitude_dd)) {
    errors <- c(errors, "Column sample_longitude_dd is not numeric")
  }
  if (!"sample_latitude_dd" %in% names(data)) {
    errors <- c(errors, "Column sample_latitude_dd missing")
  } else if (!is.numeric(data$sample_latitude_dd)) {
    errors <- c(errors, "Column sample_latitude_dd is not numeric")
  }
  if(length(errors) > 0) {
    if(report) {
      return(data_frame(level = "error",  message = errors))
    } else {
      stop(paste(errors, collapse = ", "))
    }
  }
  return(NULL)
}

get_xy_clean <- function(data, returnisclean=FALSE) {
  check_lonlat(data, report = FALSE)
  sp <- data.frame(sample_longitude_dd = numeric(0), sample_latitude_dd = numeric(0))
  isclean <- NULL
  if(NROW(data) > 0) {
    sp <- data %>% select(sample_longitude_dd, sample_latitude_dd)
    # Only valid coordinates
    isclean <- stats::complete.cases(sp) &
      sapply(sp$sample_longitude_dd, is.numeric) &
      sapply(sp$sample_latitude_dd, is.numeric) &
      !is.na(sp$sample_longitude_dd) & !is.na(sp$sample_latitude_dd) &
      sp$sample_longitude_dd >= -180.0 & sp$sample_longitude_dd <= 180.0 &
      sp$sample_latitude_dd >= -90.0 & sp$sample_latitude_dd <= 90.0
  }
  cleansp <- sp[isclean,,drop=FALSE]
  if(returnisclean) {
    return(list(cleansp=cleansp, isclean=isclean))
  } else {
    return(cleansp)
  }
}

get_xy_clean_duplicates <- function(data) {
  clean <- get_xy_clean(data, returnisclean = TRUE)
  if(NROW(clean$cleansp) > 0) {
    # Only lookup values for unique coordinates
    key <- paste(clean$cleansp$sample_longitude_dd, clean$cleansp$sample_latitude_dd, sep='\r')
    notdup <- !duplicated(key)
    uniquesp <- clean$cleansp[notdup,]
    duplicated_lookup <- match(key, key[notdup])
    list(uniquesp=uniquesp, isclean=clean$isclean, duplicated_lookup=duplicated_lookup)
  } else {
    list(uniquesp = data.frame(sample_longitude_dd = numeric(0), sample_latitude_dd = numeric(0)),
         isclean = NULL, duplicated_lookup = NULL)
  }
}

list_cache <- function() {
  list.files(rappdirs::user_cache_dir("obistools"), "call_", full.names = TRUE)
}

clear_cache <- function(age=36) {
  cachefiles <- list_cache()
  rmfiles <- cachefiles[difftime(Sys.time(), file.info(cachefiles)[,"mtime"], units = "hours") > age]
  unlink(rmfiles)
  rmfiles
}

cache_call <- function(key, expr, env = NULL) {
  stopifnot(is.expression(expr))
  if(is.null(env)) {
    env = parent.frame()
  }
  cache_dir <- rappdirs::user_cache_dir("obistools")
  cachefile <- file.path(cache_dir, paste0("call_", digest::digest(list(key=key, expr=expr)), ".rds"))
  if(file.exists(cachefile) && difftime(Sys.time(), file.info(cachefile)[,"mtime"], units = "hours") < 10) {
    return(readRDS(cachefile))
  } else {
    result <- eval(expr, envir = NULL, enclos = env)
    if(!dir.exists(cache_dir)) {
      dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
    }
    saveRDS(result, cachefile)
    return(result)
  }
}

service_call <- function(url, msg) {
  cache_call(key = paste(url, msg),
             expression({
               response <- httr::POST(url,
                                      httr::content_type("application/json"),
                                      httr::user_agent("obistools - https://github.com/iobis/obistools"),
                                      body=msg)

               # Parse result
               raw_content <- httr::content(response, as="raw")
               if(response$status_code != 200) {
                 if(is.list(raw_content) && all(c("title", "description") %in% names(raw_content))) {
                   stop(paste0(raw_content$title, ": ", raw_content$description))
                 }
                 stop(rawToChar(raw_content))
               }
               raw_content
             }))
}
