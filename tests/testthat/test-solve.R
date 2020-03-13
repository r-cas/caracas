# inverse
# linsolve
# rootsolve
# systemsolve

test_that("inverse + linsolve", {
  skip_if_no_sympy()
  
  A <- as_symbol(matrix(c(1, "x", 0, 2, -1, 3, 4, 2, 5), nrow = 3, ncol = 3))
  b <- as_symbol(c(4, 1, 7))
  x <- A %*% b
  
  # -------------------------
  # inverse(A)
  # -------------------------
  Ai <- inverse(A)
  maybe_b <- simplify(Ai %*% x)
  expect_equal(as.character(b), as.character(maybe_b))
  
  # -------------------------
  # linsolve(a, x)
  # -------------------------
  maybe_b <- simplify(linsolve(A, x))
  expect_equal(as.character(b), as.character(maybe_b))
})

test_that("rootsolve", {
  skip_if_no_sympy()
  
  ###################################################################
  # Single variable
  ###################################################################
  #------------------------------------------------------------------
  sol1 <- rootsolve(as_symbol("x**2 + 1"), "x")
  
  expect_equal(length(sol1), 2L)
  expect_equal(unname(unlist(lapply(sol1, lapply, as.character))), 
               c("-1i", "1i"))
  expect_equal(names(sol1[[1L]]), "x")
  expect_equal(as.character(sol1[[1L]]$x), "-1i")
  expect_equal(names(sol1[[2L]]), "x")
  expect_equal(as.character(sol1[[2L]]$x), "1i")
  #------------------------------------------------------------------
  
  #------------------------------------------------------------------
  x <- symbol("x")
  sol2 <- rootsolve(as_symbol("x**2 + 1"), x)
  
  expect_equal(as.character(sol1), as.character(sol2))
  
  expect_equal(length(sol2), 2L)
  expect_equal(unname(unlist(lapply(sol2, lapply, as.character))), 
               c("-1i", "1i"))
  expect_equal(names(sol2[[1L]]), "x")
  expect_equal(as.character(sol2[[1L]]$x), "-1i")
  expect_equal(names(sol2[[2L]]), "x")
  expect_equal(as.character(sol2[[2L]]$x), "1i")
  #------------------------------------------------------------------
  
  ###################################################################
  # Multiple variables
  ###################################################################
  # Must be as a row vector (1 x m matrix), not a list....
  lhs <- t(as_symbol(matrix(c("x**2 + 1", "y+3"))))
  sol1 <- rootsolve(lhs, c("x", "y"))
  sol1_ord <- order(unlist(lapply(sol1, function(l) Arg(as_r(l$x)))))
  sol1 <- sol1[sol1_ord]
  
  expect_equal(length(sol1), 2L)
  expect_equal(sort(names(sol1[[1L]])), c("x", "y"))
  expect_equal(as.character(sol1[[1L]]$x), "-1i")
  expect_equal(as.character(sol1[[1L]]$y), "-3")
  expect_equal(sort(names(sol1[[2L]])), c("x", "y"))
  expect_equal(as.character(sol1[[2L]]$x), "1i")
  expect_equal(as.character(sol1[[2L]]$y), "-3")
  
  y <- symbol("y")
  sol2 <- rootsolve(lhs, c(x, y))
  sol2_ord <- order(unlist(lapply(sol2, function(l) Arg(as_r(l$x)))))
  sol2 <- sol2[sol2_ord]
  
  expect_equal(length(sol2), 2L)
  expect_equal(sort(names(sol2[[1L]])), c("x", "y"))
  expect_equal(as.character(sol2[[1L]]$x), "-1i")
  expect_equal(as.character(sol2[[1L]]$y), "-3")
  expect_equal(sort(names(sol2[[2L]])), c("x", "y"))
  expect_equal(as.character(sol2[[2L]]$x), "1i")
  expect_equal(as.character(sol2[[2L]]$y), "-3")
})

test_that("systemsolve", {
  skip_if_no_sympy()
  
  sol1 <- systemsolve(as_symbol("x**2"), as_symbol("-1"), "x")
  #sol1
  
  expect_equal(length(sol1), 2L)
  expect_equal(unname(unlist(lapply(sol1, lapply, as.character))), 
               c("-1i", "1i"))
  expect_equal(names(sol1[[1L]]), "x")
  expect_equal(as.character(sol1[[1L]]$x), "-1i")
  expect_equal(names(sol1[[2L]]), "x")
  expect_equal(as.character(sol1[[2L]]$x), "1i")
})


test_that("solve system of non-linear equations", {
  skip_if_no_sympy()
  
  # Multinomial likelihood
  p <- as_symbol(paste0("p", 1:3))
  y <- as_symbol(paste0("y", 1:3))
  a <- as_symbol("a")
  l <- sum(y*log(p))
  L <- -l + a*(sum(p) - 1)
  g <- dd(L, c("a", paste0("p", 1:3)))
  
  sol <- rootsolve(g, c("p1", "p2", "p3", "a"))
  
  expect_equal(length(sol), 1L)
  
  expect_equal(sort(names(sol[[1L]])), sort(c("p1", "p2", "p3", "a")))
  expect_equal(as.character(sol[[1L]]$p1), "y1/(y1 + y2 + y3)")
  expect_equal(as.character(sol[[1L]]$p2), "y2/(y1 + y2 + y3)")
  expect_equal(as.character(sol[[1L]]$p3), "y3/(y1 + y2 + y3)")
  expect_equal(as.character(sol[[1L]]$a), "y1 + y2 + y3")
})