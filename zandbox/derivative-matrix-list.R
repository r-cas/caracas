#' # Issues with derivatives, matrices and lists/vectors

load_all()

#' ## Helper functions
do_unbracket <- function(x){
  gsub("\\[(.+?)\\]", "\\1", x)
}

do_bracket <- function(x){
  paste0("[", x, "]")
}

do_split_rows <- function(x){
  xx3 <- strsplit(x, "\\],")[[1]]
  for (i in 1:(length(xx3)-1))
    xx3[i] <- paste(xx3[i], "]")
  xx3
  xx3 <- lapply(xx3, function(o) gsub("[[:space:]]*", "", o))
  xx3
  xx4 <- gsub("\\[+(.+?)\\]+", "\\1", xx3)
  ## split by ,
  out <- strsplit(xx4, ",")
}

do_comma <- function(x){
  if (is.list(x))
    lapply(x, paste, collapse=", ")
  else
    paste0(x, collapse=", ")
}

#' ## Michaelis menten

N <- 3
y <- as_sym(matrix(paste0("y", 1:N)))
x <- as_sym(matrix(paste0("x", 1:N)))
b <- as_sym(paste0("b", 1:2))

num <- b[1] * x
den <- b[2] + x

#' ## Different representations
#' 
## mu a matrix; a convention?
mu1 <- num / den
mu1
symbol_is_matrix(mu1)

mu2 <-mu1 %>% remove_mat_prefix %>% do_unbracket %>% as_sym
mu2
symbol_is_matrix(mu2)

mu3 <- as_sym(do_unbracket(mu2))
mu3
symbol_is_matrix(mu3)

mu1$pyobj
mu2$pyobj
mu3$pyobj

#' ### gradients
g1 <- der(mu1, b)
g2 <- der(mu2, b)
g3 <- der(mu3, b)
g1
g2
g3

symbol_is_matrix(g1)
symbol_is_matrix(g2)
symbol_is_matrix(g3)

g1a <- g1$pyobj %>% do_unbracket %>% as_sym
g2a <- g2$pyobj %>% do_unbracket %>% as_sym # Dimension lost
g3a <- g3$pyobj %>% do_unbracket %>% as_sym # Dimension lost
g1a
g2a
g3a

symbol_is_matrix(g1a)
symbol_is_matrix(g2a)
symbol_is_matrix(g3a)

#' ## Jacobi matrix

h1 <- der2(mu1, b)
h2 <- der2(mu2, b)
h3 <- der2(mu3, b)
h1
h2
h3

symbol_is_matrix(h1)
symbol_is_matrix(h2)
symbol_is_matrix(33)


h1a <- h1$pyobj %>% do_unbracket %>% do_unbracket %>% as_sym
h2a <- h2$pyobj %>% do_unbracket %>% do_unbracket %>% as_sym # Dimension lost
h3a <- h3$pyobj %>% do_unbracket %>% do_unbracket %>% as_sym # Dimension lost
h1a
h2a
h3a

symbol_is_matrix(h1a)
symbol_is_matrix(h2a)
symbol_is_matrix(h3a)
