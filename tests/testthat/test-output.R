context("output")

test_that("as.character / tex", {
  skip_if_no_sympy()
  
  x <- symbol('x')
  z <- cos(x)^2 + sin(x)^2
  
  expect_equal(as.character(z), "sin(x)^2 + cos(x)^2")
  expect_equal(tex(z), "\\sin^{2}{\\left(x \\right)} + \\cos^{2}{\\left(x \\right)}")
})

# test_that("tex exp", {
#   skip_if_no_sympy()
#   
#   # FIXME: Issue 36: https://github.com/r-cas/caracas/issues/36
#   # n <- symbol("n")
#   # f <- (1 + 1/n)^n
#   # lim_f <- lim(f, n, Inf)
#   # tex(lim_f)
#   # get_py()$print_caracas_latex(lim_f$pyobj)
#   
# })

test_that("print", {
  skip_if_no_sympy()
  
  x <- symbol('x')
  z <- cos(x)^2 + sin(x)^2
  
  expect_equal(paste0(capture.output(print(z)), collapse = ""), 
               "C:    2         2      sin (x) + cos (x)")
  
  x <- symbol('x')
  y <- symbol('y')
  eq <- x^2 + 3*x + 4*y + y^4
  #der(eq, x) # eq = x^2+3*x+4*y+y^4
  H <- der2(eq, c(x, y)) # Hessian
  expect_false(grepl("C: ", 
                     paste0(capture.output(print(H, prompt = '')), collapse = ""),
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
  sol <- solve_sys(lhs, rhs, list(x, y))
  
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
  
  o <- getOption("caracas.print.method") 
  options(caracas.print.method = "ascii") 
  eq_print <- paste0(capture.output(print(eq)), collapse = "")
  expect_true(grepl("^", eq_print, fixed = TRUE))
  
  options(caracas.print.method = NULL) # reset, default = 'utf8'
  
  eq_print <- paste0(capture.output(print(eq, method = "ascii")), collapse = "")
  expect_true(grepl("^", eq_print, fixed = TRUE))
  
  eq_print <- paste0(capture.output(print(eq)), collapse = "")
  expect_false(grepl("^", eq_print, fixed = TRUE))
  
  options(caracas.print.method = o) 
})

test_that("print prettyascii", {
  skip_if_no_sympy()
  
  x <- symbol("x") 
  eq <- x^2 + 3*x
  eq_print <- paste0(capture.output(print(eq)), collapse = "")
  expect_false(grepl("^", eq_print, fixed = TRUE))
  
  options(caracas.print.method = "prettyascii")
  eq_print <- paste0(capture.output(print(eq)), collapse = "")
  expect_true(grepl("*", eq_print, fixed = TRUE))
  
  options(caracas.print.method = NULL)
  
  eq_print <- paste0(capture.output(print(eq, method = "prettyascii")), collapse = "")
  expect_true(grepl("*", eq_print, fixed = TRUE))
  
  options(caracas.print.method = NULL)
})


test_that("print vector", {
  skip_if_no_sympy()
  
  B <- as_sym(1:3)
  expect_true(grepl("^C: \\[1  2  3\\].+$", 
                    capture.output(print(B)))) # .{1} == transpose 
})


test_that("custom printer: exp", {
  skip_if_no_sympy()
  
  n <- symbol('n')
  
  # Error on Win 3.6
  #expect_output(print(exp(2*n), prettyascii = FALSE, ascii = FALSE), "exp(2⋅n)", fixed = TRUE)
  expect_output(print(exp(2*n), method = "prettyascii"), "exp(2*n)", fixed = TRUE)
  expect_output(print(exp(2*n), method = "ascii"), "exp(2*n)", fixed = TRUE)
  
  
  f <- (1 + 1/n)^n
  lim_f <- lim(f, n, Inf)
  
  expect_output(print(lim_f), "exp(1)", fixed = TRUE)
  expect_output(print(lim_f, method = "utf8"), "exp(1)", fixed = TRUE)
  expect_output(print(lim_f, method = NULL), "exp(1)", fixed = TRUE)
  
  expect_output(print(lim_f, method = "prettyascii"), "exp(1)", fixed = TRUE)
  expect_output(print(lim_f, method = "ascii"), "exp(1)", fixed = TRUE)
  
  
  lim_f_sym <- lim(f, n, Inf, doit = FALSE)
  
  outstr <- function(x) {
    paste0(capture.output(x), collapse = "")
  }
  
  o1 <- outstr(print(lim_f_sym))
  o1 <- outstr(print(lim_f_sym, method = "utf8"))
  o1 <- outstr(print(lim_f_sym, method = NULL))
  o2 <- outstr(print(lim_f_sym, method = "prettyascii"))
  o3 <- outstr(print(lim_f_sym, method = "ascii"))
  
  expect_equal(o3, "C: Limit((1 + 1/n)^n, n, Inf, dir='-')")
  expect_equal(o2, "C:             n        /    1\\     lim |1 + -|    n->oo\\    n/")
  
  if (grepl("UTF-8", Sys.getlocale())) {
    # UTF-8 system:
    expect_equal(o1, "C:            n       ⎛    1⎞    lim ⎜1 + ─⎟    n─→∞⎝    n⎠")
  }
  
})
