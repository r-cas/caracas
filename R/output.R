indent_not_first_line <- function(x, indent = 0) {
  spaces <- paste0(rep(" ", indent), collapse = '')
  
  # If newline, multiple lines:
  if (grepl("\n", x)) {
    
    x <- gsub("\n", paste0("\n", spaces), x)
  }
  
  #x <- paste0(spaces, x)
  
  return(x)
}

get_caracas_out <- function(x, 
                            caracas_prefix = TRUE, 
                            prettyascii = getOption("caracas.print.prettyascii", 
                                                    default = FALSE),
                            ascii = getOption("caracas.print.ascii", 
                                              default = FALSE),
                            rowvec = getOption("caracas.print.rowvec", 
                                                default = TRUE)) {
  ensure_sympy()
  
  if (is.null(x$pyobj)) {
    stop("Unexpected")
  }
  
  py <- get_py()
  
  suffix <- ""
  
  out <- if (!is.null(prettyascii) && as.logical(prettyascii) == TRUE) {
    # 'prettyascii'
    if (rowvec && symbol_is_matrix(x) && ncol(x) == 1L && nrow(x) > 1L) {
      suffix <- intToUtf8(7488L) # T utf-8
      #reticulate::py_capture_output(get_sympy()$pprint(t(x)$pyobj, use_unicode = FALSE))
      reticulate::py_capture_output(py$print_caracas(t(x)$pyobj))
    } else {
      #reticulate::py_capture_output(get_sympy()$pprint(x$pyobj, use_unicode = FALSE))
      reticulate::py_capture_output(py$print_caracas(x$pyobj))
    }
  } else if (!is.null(ascii) && as.logical(ascii) == TRUE) {
    # 'ascii'
    python_strings_to_r(get_sympy()$sstr(x$pyobj))
  } else {
    # 'utf8'
    if (rowvec && symbol_is_matrix(x) && ncol(x) == 1L && nrow(x) > 1L) {
      suffix <- intToUtf8(7488L) # T utf-8
      #reticulate::py_capture_output(get_sympy()$pprint(t(x)$pyobj))
      reticulate::py_capture_output(py$print_caracas_unicode(t(x)$pyobj))
    } else {
      #reticulate::py_capture_output(get_sympy()$pprint(x$pyobj))
      reticulate::py_capture_output(py$print_caracas_unicode(x$pyobj))
    }
  }
  
  out <- gsub("[ \n]+$", "", out)
  
  if (caracas_prefix) {
    prefix <- '[caracas]: '
    out <- indent_not_first_line(out, indent = nchar(prefix))
    out <- paste0(prefix, out)
  }
  
  out <- paste0(out, suffix)
  
  return(out)
}

#' Print symbol
#' 
#' @param x A `caracas_symbol`
#' @param caracas_prefix Print 'caracas' prefix
#' @param prettyascii `TRUE` to print in pretty ASCII format rather than in utf8
#' @param ascii `TRUE` to print in ASCII format rather than in utf8
#' @param rowvec `FALSE` to print column vectors as is
#' @param \dots not used
#'
#' @concept output
#' 
#' @export
print.caracas_symbol <- function(x, 
                                 caracas_prefix = TRUE, 
                                 prettyascii = getOption("caracas.print.prettyascii", default = FALSE),
                                 ascii = getOption("caracas.print.ascii", default = FALSE), 
                                 rowvec = getOption("caracas.print.rowvec", 
                                                               default = TRUE),
                                 ...) {
  
  out <- get_caracas_out(x, 
                         caracas_prefix = caracas_prefix,
                         prettyascii = prettyascii,
                         ascii = ascii, 
                         rowvec = rowvec)
  out <- paste0(out, "\n")
  cat(out)
  
  return(invisible(x))
}

#' Print solution
#' 
#' @param x A `caracas_symbol`
#' @param simplify Print solution in a simple format
#' @param \dots Passed to [print.caracas_symbol()]
#'
#' @concept output
#' 
#' @export
print.caracas_solve_sys_sol <- function(x, 
                                        simplify = getOption("caracas.print.sol.simplify", default = TRUE), 
                                        ...) {
  ensure_sympy()
  
  if (simplify) {
    for (i in seq_along(x)) {
      cat("Solution ", i, ":\n", sep = "")
      #print(i)
      
      nms <- names(x[[i]])
      nms <- sprintf(paste0("%-", max(nchar(nms)), "s"), nms)
      
      vals <- lapply(x[[i]], get_caracas_out, caracas_prefix = FALSE, ...)
      
      prefix <- "  "
      nms <- paste0(prefix, nms, " = ")
      
      #vals <- lapply(vals, function(l) paste0(prefix, l))
      vals <- lapply(seq_along(vals), function(j) {
        indent_not_first_line(vals[[j]], nchar(nms[j]))
      })
      
      for (j in seq_along(nms)) {
        cat(nms[j], vals[[j]], "\n", sep =)
      }
      
      # xi <- paste0(nms, " = ", vals)
      # xi <- paste0("{", paste0(xi, collapse = ", "), "}")
      # cat(xi, "\n", sep = "")
    }
    
  } else {
    y <- x
    class(y) <- setdiff(class(y), "caracas_solve_sys_sol")
    print(y)
  }
  
  return(invisible(x))
}



#' Export object to TeX
#'
#' @param x A `caracas_symbol`
#'
#' @concept output
#'
#' @export
tex <- function(x) {
  UseMethod("tex")
}

#' @export
tex.caracas_symbol <- function(x) {
  ensure_sympy()
  
  if (!is.null(x$pyobj)) {
     return(get_sympy()$latex(x$pyobj))
  }
  # if (!is.null(x$pyobj)) {
  #   py <- get_py()
  #   o <- reticulate::py_capture_output(py$print_caracas_latex(x$pyobj))
  #   return(o)
  # }
  
  stop("Unexpected")
}

#' Convert symbol to character
#'
#' @param x A `caracas_symbol`
#' @param replace_I Replace constant I (can both be identity and imaginary unit)
#' @param \dots not used
#'
#' @concept output
#'
#' @export
as.character.caracas_symbol <- function(x, replace_I = TRUE, ...) {
  y <- as.character(x$pyobj)
  y <- python_strings_to_r(y, replace_I = replace_I)
  return(y)
}
