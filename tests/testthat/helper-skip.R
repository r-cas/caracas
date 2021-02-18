skip_if_no_sympy <- function() {
  if (!has_sympy()) {
    skip("sympy not available for testing")
  }
}
