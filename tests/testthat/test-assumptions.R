context("assumptions")

test_that("ls_sym", {
  skip_if_no_sympy()
  
  expect_length(ls_sym(), 0L)
  
  x <- symbol("x")
  expect_length(ls_sym(), 1L)
})

test_that("ask", {
  skip_if_no_sympy()
  
  B <- as_sym("[[y + 1, 1], [1, 1]]")
  y <- as_sym("y", declare_symbols = FALSE)
  
  reticulate::py_run_string("w = Symbol('w', positive = True)")
  w <- as_sym("w", declare_symbols = FALSE)
  
  expect_null(ask(y, "positive"))
  expect_true(ask(w, "positive"))
  expect_false(ask(B, "positive"))
})
