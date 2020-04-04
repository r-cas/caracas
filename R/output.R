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
                            ascii = getOption("caracas.print.ascii", default = FALSE), 
                            caracas_prefix = TRUE) {
  ensure_sympy()
  
  if (is.null(x$pyobj)) {
    stop("Unexpected")
  }
  
  out <- if (!is.null(ascii) && as.logical(ascii) == TRUE) {
    # 'string'
    python_strings_to_r(get_sympy()$sstr(x$pyobj))
  } else {
    reticulate::py_capture_output(get_sympy()$pprint(x$pyobj))
  }
  
  out <- gsub("[ \n]+$", "", out)
  
  if (caracas_prefix) {
    prefix <- '[caracas]: '
    out <- indent_not_first_line(out, indent = nchar(prefix))
    out <- paste0(prefix, out)
  }
  
  return(out)
}

#' Print symbol
#' 
#' @param x A `caracas_symbol`
#' @param ascii `TRUE` to print in ASCII format rather than in utf8
#' @param caracas_prefix Print 'caracas' prefix
#' @param \dots not used
#'
#' @concept output
#' 
#' @export
print.caracas_symbol <- function(x, 
                                 ascii = getOption("caracas.print.ascii", default = FALSE), 
                                 caracas_prefix = TRUE, 
                                 ...) {
  
  out <- get_caracas_out(x, 
                         ascii = ascii, 
                         caracas_prefix = caracas_prefix)
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
  
  stop("Unexpected")
}

#' Convert symbol to character
#'
#' @param x A `caracas_symbol`
#' @param \dots not used
#'
#' @concept output
#'
#' @export
as.character.caracas_symbol <- function(x, ...) {
  y <- as.character(x$pyobj)
  y <- python_strings_to_r(y)
  return(y)
}
