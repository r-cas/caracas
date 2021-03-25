context("def_sym()")

test_that("small", {
  skip_if_no_sympy()
  
  expect_equal(length(ls()), 0L)
  
  def_sym(n1, n2, n3)
  expect_equal(length(ls()), 3L)
  
  def_sym("x1", "x2", "x3")
  expect_equal(length(ls()), 6L)
  
  expect_warning(def_sym("x1", "x2", "x3", warn = TRUE))
})

test_that("charvec", {
  skip_if_no_sympy()
  
  def_sym(charvec = c("x1", "x2", "x3"))
  expect_equal(length(ls()), 3L)
})
