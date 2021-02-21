context("output")

test_that("as.character / tex", {
  skip_if_no_sympy()
  
  x <- symbol('x')
  z <- cos(x)^2 + sin(x)^2
  
  expect_equal(as.character(z), "sin(x)^2 + cos(x)^2")
  expect_equal(tex(z), "\\sin^{2}{\\left(x \\right)} + \\cos^{2}{\\left(x \\right)}")
})

test_that("print", {
  skip_if_no_sympy()
  
  x <- symbol('x')
  z <- cos(x)^2 + sin(x)^2
  
  expect_equal(paste0(capture.output(print(z)), collapse = ""), 
               "[caracas]:    2         2              sin (x) + cos (x)")
  
  x <- symbol('x')
  y <- symbol('y')
  eq <- x^2 + 3*x + 4*y + y^4
  #der(eq, x) # eq = x^2+3*x+4*y+y^4
  H <- der2(eq, c(x, y)) # Hessian
  expect_false(grepl("[caracas]", 
                     paste0(capture.output(print(H, caracas_prefix = FALSE)), collapse = ""),
                     fixed = TRUE))
})

test_that("util:", {
  expect_equal(indent_not_first_line("test"), "test")
  expect_equal(indent_not_first_line("test1\ntest2"), "test1\ntest2")
  
  expect_equal(indent_not_first_line("test", 2), "test")
  expect_equal(indent_not_first_line("test1\ntest2", 2), "test1\n  test2")
})

test_that("print solve_sys:", {
  skip_if_no_sympy()
  
  x <- symbol("x")
  y <- symbol("y")
  lhs <- cbind(3*x*y - y, x)
  rhs <- cbind(-5*x, y+4)
  sol <- solve_sys(lhs, rhs, c(x, y))
  
  out <- paste0(capture.output(print(sol)), collapse = "")
  expect_match(out, "Solution")
  expect_match(out, "x[ =]+2/3")
  expect_match(out, "y[ =]+-10/3")

  out <- paste0(capture.output(print(sol, simplify = FALSE)), collapse = "")
  expect_true(grepl("[[1]]$x", out, fixed = TRUE))
  
  ###
  
  options(caracas.print.sol.simplify = TRUE) # default is TRUE
  out <- paste0(capture.output(print(sol)), collapse = "")
  expect_match(out, "Solution")
  expect_match(out, "x[ =]+2/3")
  expect_match(out, "y[ =]+-10/3")
  
  options(caracas.print.sol.simplify = FALSE) # default is TRUE
  out <- paste0(capture.output(print(sol)), collapse = "")
  expect_true(grepl("[[1]]$x", out, fixed = TRUE))
  
  options(caracas.print.sol.simplify = NULL)
})

test_that("print ascii", {
  skip_if_no_sympy()

  x <- symbol("x") 
  eq <- x^2 + 3*x
  eq_print <- paste0(capture.output(print(eq)), collapse = "")
  expect_false(grepl("^", eq_print, fixed = TRUE))
  
  options(caracas.print.ascii = TRUE) # default is FALSE
  eq_print <- paste0(capture.output(print(eq)), collapse = "")
  expect_true(grepl("^", eq_print, fixed = TRUE))
  
  options(caracas.print.ascii = FALSE)
  
  eq_print <- paste0(capture.output(print(eq, ascii = TRUE)), collapse = "")
  expect_true(grepl("^", eq_print, fixed = TRUE))
  
  eq_print <- paste0(capture.output(print(eq, ascii = FALSE)), collapse = "")
  expect_false(grepl("^", eq_print, fixed = TRUE))
  
  options(caracas.print.ascii = NULL)
})

test_that("print prettyascii", {
  skip_if_no_sympy()
  
  x <- symbol("x") 
  eq <- x^2 + 3*x
  eq_print <- paste0(capture.output(print(eq)), collapse = "")
  expect_false(grepl("^", eq_print, fixed = TRUE))
  
  options(caracas.print.prettyascii = TRUE) # default is FALSE
  eq_print <- paste0(capture.output(print(eq)), collapse = "")
  expect_true(grepl("*", eq_print, fixed = TRUE))
  
  options(caracas.print.prettyascii = FALSE)
  
  eq_print <- paste0(capture.output(print(eq, prettyascii = TRUE)), collapse = "")
  expect_true(grepl("*", eq_print, fixed = TRUE))
  
  options(caracas.print.prettyascii = NULL)
})


test_that("print vector", {
  skip_if_no_sympy()
  
  B <- as_sym(1:3)
  expect_true(grepl("^\\[caracas\\]: \\[1  2  3\\].+$", 
                    capture.output(print(B)))) # .{1} == transpose 
})


test_that("custom printer: exp", {
  skip_if_no_sympy()
  
  n <- symbol('n')
  
  expect_output(print(exp(2*n), prettyascii = FALSE, ascii = FALSE), "exp(2⋅n)", fixed = TRUE)
  expect_output(print(exp(2*n), prettyascii = TRUE, ascii = FALSE), "exp(2*n)", fixed = TRUE)
  expect_output(print(exp(2*n), prettyascii = FALSE, ascii = TRUE), "exp(2*n)", fixed = TRUE)
  
  
  f <- (1 + 1/n)^n
  lim_f <- limf(f, n, Inf)
  
  expect_output(print(lim_f, prettyascii = FALSE, ascii = FALSE), "exp(1)", fixed = TRUE)
  expect_output(print(lim_f, prettyascii = TRUE, ascii = FALSE), "exp(1)", fixed = TRUE)
  expect_output(print(lim_f, prettyascii = FALSE, ascii = TRUE), "exp(1)", fixed = TRUE)
  
  
  lim_f_sym <- limf(f, n, Inf, doit = FALSE)
  
  outstr <- function(x) {
    paste0(capture.output(x), collapse = "")
  }
  
  o1 <- outstr(print(lim_f_sym, prettyascii = FALSE, ascii = FALSE))
  o2 <- outstr(print(lim_f_sym, prettyascii = TRUE, ascii = FALSE))
  o3 <- outstr(print(lim_f_sym, prettyascii = FALSE, ascii = TRUE))
  
  expect_equal(o1, "[caracas]:            n               ⎛    1⎞            lim ⎜1 + ─⎟            n─→∞⎝    n⎠")
  expect_equal(o2, "[caracas]:             n                /    1\\             lim |1 + -|            n->oo\\    n/")
  expect_equal(o3, "[caracas]: Limit((1 + 1/n)^n, n, Inf, dir='-')")
  
})
