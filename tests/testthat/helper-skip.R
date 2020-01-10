skip_if_no_sympy <- function() {
  if (!have_sympy()) {
    skip("sympy not available for testing")
  }
}
