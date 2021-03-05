# inv
# solve_lin
# solve_sys

test_that("inv/solve_lin", {
  skip_if_no_sympy()
  
  A <- as_sym(matrix(c(1, "x", 0, 2, -1, 3, 4, 2, 5), nrow = 3, ncol = 3))
  b <- as_sym(c(4, 1, 7))
  x <- A %*% b
  
  # -------------------------
  # inv(A)
  # -------------------------
  Ai <- inv(A)
  maybe_b <- simplify(Ai %*% x)
  expect_equal(as.character(b), as.character(maybe_b))

  # -------------------------
  # solve_lin(A)
  # -------------------------
  Ai2 <- solve_lin(A)
  expect_equal(as.character(Ai), as.character(Ai2))
  
  # -------------------------
  # solve_lin(A, x)
  # -------------------------
  maybe_b <- simplify(solve_lin(A, x))
  expect_equal(as.character(b), as.character(maybe_b))
})

test_that("solve_sys(lhs, vars)", {
  skip_if_no_sympy()
  
  ###################################################################
  # Single variable
  ###################################################################
  #------------------------------------------------------------------
  sol1 <- solve_sys(as_sym("x**2 + 1"), "x")
  
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
  sol2 <- solve_sys(as_sym("x**2 + 1"), x)
  
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
  lhs <- t(as_sym(matrix(c("x**2 + 1", "y+3"))))
  sol1 <- solve_sys(lhs, c("x", "y"))
  sol1_ord <- order(unlist(lapply(sol1, function(l) Arg(as_expr(l$x)))))
  sol1 <- sol1[sol1_ord]
  
  expect_equal(length(sol1), 2L)
  expect_equal(sort(names(sol1[[1L]])), c("x", "y"))
  expect_equal(as.character(sol1[[1L]]$x), "-1i")
  expect_equal(as.character(sol1[[1L]]$y), "-3")
  expect_equal(sort(names(sol1[[2L]])), c("x", "y"))
  expect_equal(as.character(sol1[[2L]]$x), "1i")
  expect_equal(as.character(sol1[[2L]]$y), "-3")
  
  y <- symbol("y")
  sol2 <- solve_sys(lhs, list(x, y))
  sol2_ord <- order(unlist(lapply(sol2, function(l) Arg(as_expr(l$x)))))
  sol2 <- sol2[sol2_ord]
  
  expect_equal(length(sol2), 2L)
  expect_equal(sort(names(sol2[[1L]])), c("x", "y"))
  expect_equal(as.character(sol2[[1L]]$x), "-1i")
  expect_equal(as.character(sol2[[1L]]$y), "-3")
  expect_equal(sort(names(sol2[[2L]])), c("x", "y"))
  expect_equal(as.character(sol2[[2L]]$x), "1i")
  expect_equal(as.character(sol2[[2L]]$y), "-3")
})

test_that("solve_sys(lhs, rhs, vars)", {
  skip_if_no_sympy()
  
  sol1 <- solve_sys(as_sym("x**2"), as_sym("-1"), "x")
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
  p <- as_sym(paste0("p", 1:3))
  y <- as_sym(paste0("y", 1:3))
  a <- as_sym("a")
  l <- sum(y*log(p))
  L <- -l + a*(sum(p) - 1)
  g <- der(L, list(a, p))
  expect_match(as.character(g), 
               "[p1 + p2 + p3 - 1, a - y1/p1, a - y2/p2, a - y3/p3]", 
               fixed = TRUE)
  
  sols <- solve_sys(g, list(a, p))
  expect_equal(length(sols), 1L)
  
  sol <- sols[[1L]]
  expect_equal(sort(names(sol)), sort(c("p1", "p2", "p3", "a")), fixed = TRUE)
  expect_match(as.character(sol$p1), "y1/(y1 + y2 + y3)", fixed = TRUE)
  expect_match(as.character(sol$p2), "y2/(y1 + y2 + y3)", fixed = TRUE)
  expect_match(as.character(sol$p3), "y3/(y1 + y2 + y3)", fixed = TRUE)
  expect_match(as.character(sol$a), "y1 + y2 + y3", fixed = TRUE)
  
  H <- der2(L, list(p, a))
  H_sol <- subs_lst(H, sol)
  expect_match(as.character(H_sol), 
               "[[(y1 + y2 + y3)^2/y1, 0, 0, 1], [0, (y1 + y2 + y3)^2/y2, 0, 1], [0, 0, (y1 + y2 + y3)^2/y3, 1], [1, 1, 1, 0]]", 
               fixed = TRUE)
})
