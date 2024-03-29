python_strings_to_r <- function(xstr, replace_I = TRUE) {
  # Python syntax:
  # power() function
  xstr <- gsub("**", "^", xstr, fixed = TRUE)

  # I but not in Inf
  #xstr <- gsub("I(?!nf)", "1i", xstr, ignore.case = FALSE, perl = TRUE)
  # I but not followed by another character
  if (replace_I) {
    xstr <- gsub("^I$", "1i", xstr, ignore.case = FALSE, perl = TRUE)
    xstr <- gsub("^-I$", "-1i", xstr, ignore.case = FALSE, perl = TRUE)
    xstr <- gsub("I([^a-zA-Z]+)", "1i \\1", xstr, ignore.case = FALSE, perl = TRUE)
    xstr <- gsub("I$", "1i", xstr, ignore.case = FALSE, perl = TRUE)
  }
  
  # Exponential
  xstr <- gsub("^E$", "exp(1)", xstr, ignore.case = FALSE, perl = TRUE)
  xstr <- gsub("^-E$", "-exp(1)", xstr, ignore.case = FALSE, perl = TRUE)
  xstr <- gsub("E([^a-zA-Z]+)", "exp(1) \\1", xstr, ignore.case = FALSE, perl = TRUE)
  xstr <- gsub("([^a-zA-Z]+)E$", "\\1 exp(1)", xstr, ignore.case = FALSE, perl = TRUE)
  
  # Inf
  xstr <- gsub("oo", "Inf", xstr, fixed = TRUE)
  
  # Other functions
  # Abs()
  xstr <- gsub("(^|[^A-Za-z]+)Abs\\(", "\\1abs(", xstr, ignore.case = FALSE, perl = TRUE)
  # conjugate()
  xstr <- gsub("(^|[^A-Za-z]+)conjugate\\(", "\\1Conj(", xstr, ignore.case = FALSE, perl = TRUE)
  
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
  z <- lapply(z, trimws)

  if (as_character) {
    z[[1L]] <- paste0("'", z[[1L]], "'")
  }

  u <- paste0("cbind(", paste0(z[[1L]], collapse = ", "), ")")

  return(u)
}

# X <- matrix_sym(3, 2)
# as_expr(X)
# x <- remove_mat_prefix(python_strings_to_r(as.character(X$pyobj)))
# from_sy_mat(x)
# from_sy_mat(x, TRUE)
from_sy_mat <- function(x, as_character = FALSE) {
  z <- strsplit(x, "\\][ ]*,[ ]*\\[")
  z <- z[[1L]]
  z[1L] <- gsub("^\\[[ ]*\\[", "", z[1L])
  z[length(z)] <- gsub("\\][ ]*\\]$", "", z[length(z)])
  z <- strsplit(z, ",")
  z <- lapply(z, trimws, which = "both")
  
  # Transpose
  u <- do.call(Map, c(f = c, z))
  
  ###
  stopifnot(length(z) >= 1L)
  w <- unlist(u)
  
  if (as_character) {
    w <- paste0("'", w, "'")
  }

  ww <- paste0("matrix(c(", 
               paste0(w, collapse = ", "), "), nrow = ", length(z), ")")
  
  return(ww)
}

# Org:
# from_sy_mat <- function(x, as_character = FALSE) {
#   z <- strsplit(x, "\\][ ]*,[ ]*\\[")
#   z <- z[[1L]]
#   z[1L] <- gsub("^\\[[ ]*\\[", "", z[1L])
#   z[length(z)] <- gsub("\\][ ]*\\]$", "", z[length(z)])
#   
#   if (as_character) {
#     z <- strsplit(z, ",")
#     z <- lapply(z, function(zi) paste0("'", trimws(zi, "both"), "'", collapse = ", "))
#   }
#   
#   u <- paste0("rbind(", paste0("cbind(", z, ")", collapse = ", "), ")")
#   
#   return(u)
# }

# SH:
# from_sy_mat <- function(x, as_character = FALSE) {
#   z <- strsplit(x, "\\][ ]*,[ ]*\\[")
#   z <- z[[1L]]
#   z[1L] <- gsub("^\\[[ ]*\\[", "", z[1L])
#   z[length(z)] <- gsub("\\][ ]*\\]$", "", z[length(z)])
# 
#   zz <<- z
#   if (as_character) {
#     z <- strsplit(z, ",")
#     z <- lapply(z, function(zi) paste0("'", trimws(zi, "both"), "'", collapse = ", "))
#   }
# 
# 
#   u <- paste0("cbind(", paste0("c(", z, ")", collapse = ", "), ")")
#   
#   return(u)
# }



remove_mat_prefix <- function(x) {
  z <- gsub("^Matrix\\((.*)\\)$", "\\1", x)
  return(z)
}


as_expr_worker <- function(x, as_character = FALSE, first_doit = TRUE) {
  if (!inherits(x, "caracas_symbol")) {
    stop("x must be a caracas_symbol")
  }
  
  if (first_doit) {
    x <- try_doit(x)
  }
  
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
  
  if (as_character) {
    return(paste0("'", xstr, "'"))
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



#' Convert caracas object to R
#'
#' Potentially calls [doit()].
#'
#' @param x caracas_symbol
#' @param first_doit Try `doit()` first
#' 
#' @examples 
#' if (has_sympy()) {
#'   v <- vector_sym(2)
#'   x <- as_expr(v)
#'   x
#'   y <- as.expression(v)
#'   y
#' }
#'
#' @concept caracas_symbol
#'
#' @export
as_expr <- function(x, first_doit = TRUE) {
  UseMethod("as_expr")
}


#' @export
as_expr.default <- function(x, first_doit = TRUE) {
  return(x)
}


#' @export
as_expr.caracas_solve_sys_sol <- function(x, first_doit = TRUE) {
    out <- lapply(x, lapply, as_expr, first_doit)
    return(out)
}





#' @export
as_expr.caracas_symbol <- function(x, first_doit = TRUE) {
  ensure_sympy()
  
  ychr <- as_expr_worker(x, first_doit = first_doit)
  
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

  attributes(y) <- NULL
  
  if (expr_has_vars(y)) {
      return(y)
  } else {
      return(eval(y))
  }
}



#' @export
as_expr.caracas_solve_sys_sol <- function(x, first_doit = TRUE) {
  out <- lapply(x, lapply, as_expr, first_doit)
  return(out)
}


#' @rdname as_expr
#' @param \dots not used
#' @export
as.expression.caracas_symbol <- function(x, ...) {
  as_expr(x, ...)
}

#' @rdname as_expr
#' @param \dots not used
#' @export
as.expression.caracas_solve_sys_sol <- function(x, ...) {
  as_expr(x, ...)
}
