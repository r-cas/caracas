context("output")

test_that("as.character / tex", {
  x <- symbol('x')
  z <- cos(x)^2 + sin(x)^2
  
  expect_equal(as.character(z), "sin(x)^2 + cos(x)^2")
  expect_equal(tex(z), "\\sin^{2}{\\left(x \\right)} + \\cos^{2}{\\left(x \\right)}")
})
