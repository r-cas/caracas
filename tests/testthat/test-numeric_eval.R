test_that("smoke", {
  skip_if_no_sympy()
  
  n_2 <- as_symbol("2")
  n_pi <- as_symbol("pi", declare_variables = FALSE)
  x <- sqrt(n_2) * n_pi
  
  expect_equal(as.character(N(x)), "4.44288293815837")
  expect_equal(as.character(N(x, 5)), "4.4429")
  expect_equal(as.character(N(x, 50)), "4.4428829381583662470158809900606936986146216893757")
})
