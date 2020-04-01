python_strings_to_r <- function(xstr) {
  # Python syntax:
  # power() function
  xstr <- gsub("**", "^", xstr, fixed = TRUE)

  # I but not in Inf
  #xstr <- gsub("I(?!nf)", "1i", xstr, ignore.case = FALSE, perl = TRUE)
  # I but not followed by another character
  xstr <- gsub("^I$", "1i", xstr, ignore.case = FALSE, perl = TRUE)
  xstr <- gsub("^-I$", "-1i", xstr, ignore.case = FALSE, perl = TRUE)
  xstr <- gsub("I[^a-z]+", "1i", xstr, ignore.case = FALSE, perl = TRUE)
  
  # Inf
  xstr <- gsub("oo", "Inf", xstr, fixed = TRUE)

  return(xstr)
}

r_strings_to_python <- function(xstr) {
  # Python syntax:
  # power() function
  xstr <- gsub("^", "**", xstr, fixed = TRUE)
  
  # I but not in Inf

  # Inf
  xstr <- gsub("Inf", "oo", xstr, fixed = TRUE)
  
  return(xstr)
}

#' @importFrom utils str 
stop_parse_error <- function(x) {
  print(x)
  print(str(x))
  print(as.character(x))
  stop(paste("Could not convert.", 
             "Please report (including the above info) at", 
             "<https://github.com/r-cas/caracas>."))
}


from_sy_vec <- function(x, as_character = FALSE) {
  y <- gsub("^\\[(.*)\\]$", "\\1", x)
  z <- strsplit(y, ",")

  if (as_character) {
    z[[1L]] <- paste0("'", z[[1L]], "'")
  }
  
  u <- paste0("cbind(", paste0(z[[1L]], collapse = ", "), ")")
  
  return(u)
}

from_sy_mat <- function(x, as_character = FALSE) {
  z <- strsplit(x, "\\][ ]*,[ ]*\\[")
  z <- z[[1L]]
  z[1L] <- gsub("^\\[[ ]*\\[", "", z[1L])
  z[length(z)] <- gsub("\\][ ]*\\]$", "", z[length(z)])
  
  if (as_character) {
    z <- strsplit(z, ",")
    z <- lapply(z, function(zi) paste0("'", trimws(zi, "both"), "'", collapse = ", "))
  }
  
  u <- paste0("rbind(", paste0("cbind(", z, ")", collapse = ", "), ")")
  
  return(u)
}

remove_mat_prefix <- function(x) {
  z <- gsub("^Matrix\\((.*)\\)$", "\\1", x)
  return(z)
}


as_r_symr_worker <- function(x, as_character = FALSE) {
  if (!inherits(x, "caracas_symbol")) {
    stop("x must be a caracas_symbol")
  }
  
  x <- try_doit(x)
  
  xstr <- as.character(x$pyobj)
  
  xstr <- python_strings_to_r(xstr)
  
  if (grepl("^Matrix\\(\\[\\[.*\\]\\]\\)$", xstr)) {
    z <- remove_mat_prefix(xstr)
    return(from_sy_mat(z, as_character))
  }
  
  if (grepl("^\\[\\[.*\\]\\]$", xstr)) {
    return(from_sy_mat(xstr, as_character))
  }
  
  if (grepl("^\\[.*\\]$", xstr)) {
    return(from_sy_vec(xstr, as_character))
  }
  
  return(xstr)
}

expr_has_vars <- function(x) {
  y_vars <- all.vars(x)
  
  # Known "vars"
  y_vars <- setdiff(y_vars,
                    c("pi", "I"))
  
  if (length(y_vars) > 0L) {
    return(TRUE)
  }
  
  return(FALSE)
}



#' Convert symr object to R
#'
#' Potentially calls [doit()].
#'
#' @param x caracas_symbol
#'
#' @concept caracas_symbol
#'
#' @export
as_r <- function(x) {
  UseMethod("as_r")
}


#' @export
as_r.default <- function(x) {
  return(x)
}

#' @export
as_r.caracas_symbol <- function(x) {
  ensure_sympy()
  
  ychr <- as_r_symr_worker(x)
  
  # FIXME:
  #    Catch Matrix([]) ...
  #    ** = ^
  y <- tryCatch(parse(text = ychr),
                error = function(e) {
                  e
                },
                warning = function(w) {
                  w
                })
  
  if (inherits(y, "error") || inherits(y, "warning")) {
    stop_parse_error(x)
  }
  
  if (expr_has_vars(y)) {
    return(y)
  }
  
  #return(y)
  return(eval(y))
}

