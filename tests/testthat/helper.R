# Skip test if a remote resource is not responding
skip_if_resource_unavailable <- function(url, msg = NULL, allow_status = 0:399, timeout = 5) {
  ok <- tryCatch({
    resp <- httr::GET(url, httr::timeout(timeout), httr::config(nobody = TRUE))
    httr::status_code(resp) %in% allow_status
  }, error = function(e) FALSE)

  if (!ok) {
    if (is.null(msg)) {
      msg <- paste("Resource not responding or status not allowed:", url)
    }
    testthat::skip(msg)
  }
}

get_vector_from_body <- function(fn, name) {
  if (!is.function(fn)) stop("`fn` must be a function")

  found <- NULL

  recurse <- function(expr) {
    if (is.null(found) && is.call(expr)) {
      # assignment forms: <- and =
      if ((identical(expr[[1]], as.name("<-")) || identical(expr[[1]], as.name("="))) &&
          is.symbol(expr[[2]]) && as.character(expr[[2]]) == name) {
        found <<- expr[[3]]
        return()
      }

      # assign("name", value, ...)
      if (identical(expr[[1]], as.name("assign")) && length(expr) >= 3) {
        arg1 <- expr[[2]]
        if ((is.character(arg1) && arg1 == name) ||
            (is.symbol(arg1) && as.character(arg1) == name)) {
          found <<- expr[[3]]
          return()
        }
      }

      # recurse into children
      for (i in seq_along(expr)) {
        recurse(expr[[i]])
        if (!is.null(found)) return()
      }
    }
  }

  recurse(body(fn))

  # not found -> return empty vector (do not error, easier for tests)
  if (is.null(found)) return(character(0))

  # try to evaluate the RHS in the function's environment (safe fallback)
  env_try <- environment(fn)
  val <- tryCatch(eval(found, envir = env_try), error = function(e) {
    tryCatch(eval(found, envir = baseenv()), error = function(e2) NULL)
  })

  if (is.null(val)) return(character(0))

  # if atomic (like c("a","b")) return as character
  if (is.atomic(val)) return(as.character(val))

  # otherwise we don't know how to interpret it -> empty
  character(0)
}

# Helper: extractor for extreme threshold
get_extreme_threshold <- function(fn, fname = NULL, manual_map = list()) {
  # If running under covr, always use manual_map
  if (nzchar(Sys.getenv("R_COVR"))) {
    if (!is.null(fname) && fname %in% names(manual_map)) {
      return(as.numeric(manual_map[[fname]]))
    }
    stop("R_COVR set but no manual threshold provided for: ", fname)
  }
  # recursive search for assignment whose LHS name matches /extreme.*upper/i
  find_assign_rhs <- function(expr) {
    if (is.null(expr)) return(NULL)

    # If it's an assignment call (<- or =)
    if (is.call(expr) && length(expr) >= 3 &&
        (identical(expr[[1]], as.name("<-")) || identical(expr[[1]], as.name("=")))) {
      lhs <- expr[[2]]
      if (is.name(lhs)) {
        lhs_nm <- as.character(lhs)
        if (grepl("extreme.*upper", lhs_nm, ignore.case = TRUE)) {
          return(expr[[3]])
        }
      }
    }

    # Recurse into children (works for calls, pairlists, expressions)
    if (is.call(expr) || is.expression(expr) || is.pairlist(expr)) {
      for (el in as.list(expr)) {
        res <- find_assign_rhs(el)
        if (!is.null(res)) return(res)
      }
    }

    NULL
  }

  # Try automatic extraction
  exprs <- body(fn)
  rhs_expr <- find_assign_rhs(exprs)

  # If not found, try manual map (by function name)
  if (is.null(rhs_expr)) {
    if (!is.null(fname) && fname %in% names(manual_map)) {
      return(as.numeric(manual_map[[fname]]))
    }
    stop("Could not locate extreme.threshold.upper in function body; consider adding to manual_map.")
  }

  # Evaluate the RHS in the function's environment
  val <- tryCatch(eval(rhs_expr), error = function(e) {
    stop("Failed to evaluate extreme threshold expression: ", conditionMessage(e))
  })

  as.numeric(val)
}

# Helper: collect character literals from an expression (recursive)
collect_string_literals <- function(expr) {
  out <- character(0)
  if (is.character(expr)) return(expr)
  if (is.atomic(expr)) return(character(0))
  if (is.call(expr) || is.expression(expr) || is.pairlist(expr)) {
    for (el in as.list(expr)) {
      out <- c(out, collect_string_literals(el))
    }
  }
  unique(out)
}

# Helper: extract first string literal found in the function condition (used as parameter name)
get_parameter_string <- function(fn, fname = NULL, manual_map = list()) {
  # If running under covr, force manual_map lookup
  if (nzchar(Sys.getenv("R_COVR"))) {
    if (!is.null(fname) && fname %in% names(manual_map)) {
      return(manual_map[[fname]])
    }
    stop("R_COVR set but no manual parameter provided for: ", fname)
  }

  exprs <- as.list(body(fn))
  # find the if(...) calls
  if_idx <- which(vapply(exprs, function(x) is.call(x) && identical(x[[1]], as.name("if")), logical(1)))
  if (length(if_idx) == 0) stop("Could not find if() in function body")
  # try each if condition until we find a string literal
  for (i in if_idx) {
    cond <- exprs[[i]][[2]]
    strs <- collect_string_literals(cond)
    if (length(strs) > 0) return(strs[1])
  }
  stop("Could not find any string literal parameter in if() condition")
}
