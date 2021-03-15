# Issues with derivatives, matrices and lists/vectors


```r
load_all()
```

```
## Loading caracas
```

```
## 
## Attaching package: 'testthat'
```

```
## The following object is masked from 'package:devtools':
## 
##     test_file
```

## Helper functions


```r
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
```

## Michaelis menten


```r
N <- 3
y <- as_sym(matrix(paste0("y", 1:N)))
x <- as_sym(matrix(paste0("x", 1:N)))
b <- as_sym(paste0("b", 1:2))

num <- b[1] * x
den <- b[2] + x
```

## Different representations



```r
## mu a matrix; a convention?
mu1 <- num / den
mu1
```

```
## [caracas]: ⎡ b₁⋅x₁    b₁⋅x₂    b₁⋅x₃ ⎤
##            ⎢───────  ───────  ───────⎥
##            ⎣b₂ + x₁  b₂ + x₂  b₂ + x₃⎦ᵀ
```

```r
symbol_is_matrix(mu1)
```

```
## [1] TRUE
```

```r
mu2 <-mu1 %>% remove_mat_prefix %>% do_unbracket %>% as_sym
mu2
```

```
## [caracas]: ⎡ b₁⋅x₁    b₁⋅x₂    b₁⋅x₃ ⎤
##            ⎢───────, ───────, ───────⎥
##            ⎣b₂ + x₁  b₂ + x₂  b₂ + x₃⎦
```

```r
symbol_is_matrix(mu2)
```

```
## [1] FALSE
```

```r
mu3 <- as_sym(do_unbracket(mu2))
mu3
```

```
## [caracas]: ⎛ b₁⋅x₁    b₁⋅x₂    b₁⋅x₃ ⎞
##            ⎜───────, ───────, ───────⎟
##            ⎝b₂ + x₁  b₂ + x₂  b₂ + x₃⎠
```

```r
symbol_is_matrix(mu3)
```

```
## [1] FALSE
```

```r
mu1$pyobj
```

```
## Matrix([[b1*x1/(b2 + x1)], [b1*x2/(b2 + x2)], [b1*x3/(b2 + x3)]])
```

```r
mu2$pyobj
```

```
## [b1*x1/(b2 + x1), b1*x2/(b2 + x2), b1*x3/(b2 + x3)]
```

```r
mu3$pyobj
```

```
## (b1*x1/(b2 + x1), b1*x2/(b2 + x2), b1*x3/(b2 + x3))
```

### gradients


```r
g1 <- der(mu1, b)
g2 <- der(mu2, b)
g3 <- der(mu3, b)
g1
```

```
## [caracas]: ⎡           ⎡ -b₁⋅x₁   ⎤⎤
##            ⎢           ⎢──────────⎥⎥
##            ⎢⎡   x₁  ⎤  ⎢         2⎥⎥
##            ⎢⎢───────⎥  ⎢(b₂ + x₁) ⎥⎥
##            ⎢⎢b₂ + x₁⎥  ⎢          ⎥⎥
##            ⎢⎢       ⎥  ⎢ -b₁⋅x₂   ⎥⎥
##            ⎢⎢   x₂  ⎥  ⎢──────────⎥⎥
##            ⎢⎢───────⎥  ⎢         2⎥⎥
##            ⎢⎢b₂ + x₂⎥  ⎢(b₂ + x₂) ⎥⎥
##            ⎢⎢       ⎥  ⎢          ⎥⎥
##            ⎢⎢   x₃  ⎥  ⎢ -b₁⋅x₃   ⎥⎥
##            ⎢⎢───────⎥  ⎢──────────⎥⎥
##            ⎢⎣b₂ + x₃⎦  ⎢         2⎥⎥
##            ⎣           ⎣(b₂ + x₃) ⎦⎦
```

```r
g2
```

```
## [caracas]: ⎡    x₁          x₂          x₃    ⎤
##            ⎢ ───────     ───────     ───────  ⎥
##            ⎢ b₂ + x₁     b₂ + x₂     b₂ + x₃  ⎥
##            ⎢                                  ⎥
##            ⎢ -b₁⋅x₁      -b₁⋅x₂      -b₁⋅x₃   ⎥
##            ⎢──────────  ──────────  ──────────⎥
##            ⎢         2           2           2⎥
##            ⎣(b₂ + x₁)   (b₂ + x₂)   (b₂ + x₃) ⎦
```

```r
g3
```

```
## [caracas]: ⎡    x₁          x₂          x₃    ⎤
##            ⎢ ───────     ───────     ───────  ⎥
##            ⎢ b₂ + x₁     b₂ + x₂     b₂ + x₃  ⎥
##            ⎢                                  ⎥
##            ⎢ -b₁⋅x₁      -b₁⋅x₂      -b₁⋅x₃   ⎥
##            ⎢──────────  ──────────  ──────────⎥
##            ⎢         2           2           2⎥
##            ⎣(b₂ + x₁)   (b₂ + x₂)   (b₂ + x₃) ⎦
```

```r
symbol_is_matrix(g1)
```

```
## [1] FALSE
```

```r
symbol_is_matrix(g2)
```

```
## [1] FALSE
```

```r
symbol_is_matrix(g3)
```

```
## [1] FALSE
```

```r
g1a <- g1$pyobj %>% do_unbracket %>% as_sym
g2a <- g2$pyobj %>% do_unbracket %>% as_sym # Dimension lost
g3a <- g3$pyobj %>% do_unbracket %>% as_sym # Dimension lost
g1a
```

```
## [caracas]: ⎡    x₁          x₂          x₃    ⎤
##            ⎢ ───────     ───────     ───────  ⎥
##            ⎢ b₂ + x₁     b₂ + x₂     b₂ + x₃  ⎥
##            ⎢                                  ⎥
##            ⎢ -b₁⋅x₁      -b₁⋅x₂      -b₁⋅x₃   ⎥
##            ⎢──────────  ──────────  ──────────⎥
##            ⎢         2           2           2⎥
##            ⎣(b₂ + x₁)   (b₂ + x₂)   (b₂ + x₃) ⎦
```

```r
g2a
```

```
## [caracas]: ⎡   x₁       x₂       x₃     -b₁⋅x₁      -b₁⋅x₂      -b₁⋅x₃   ⎤
##            ⎢───────, ───────, ───────, ──────────, ──────────, ──────────⎥
##            ⎢b₂ + x₁  b₂ + x₂  b₂ + x₃           2           2           2⎥
##            ⎣                           (b₂ + x₁)   (b₂ + x₂)   (b₂ + x₃) ⎦
```

```r
g3a
```

```
## [caracas]: ⎡   x₁       x₂       x₃     -b₁⋅x₁      -b₁⋅x₂      -b₁⋅x₃   ⎤
##            ⎢───────, ───────, ───────, ──────────, ──────────, ──────────⎥
##            ⎢b₂ + x₁  b₂ + x₂  b₂ + x₃           2           2           2⎥
##            ⎣                           (b₂ + x₁)   (b₂ + x₂)   (b₂ + x₃) ⎦
```

```r
symbol_is_matrix(g1a)
```

```
## [1] TRUE
```

```r
symbol_is_matrix(g2a)
```

```
## [1] FALSE
```

```r
symbol_is_matrix(g3a)
```

```
## [1] FALSE
```

## Jacobi matrix


```r
h1 <- der2(mu1, b)
h2 <- der2(mu2, b)
h3 <- der2(mu3, b)
h1
```

```
## [caracas]: ⎡              ⎡   -x₁    ⎤⎤
##            ⎢              ⎢──────────⎥⎥
##            ⎢              ⎢         2⎥⎥
##            ⎢              ⎢(b₂ + x₁) ⎥⎥
##            ⎢              ⎢          ⎥⎥
##            ⎢    ⎡0⎤       ⎢   -x₂    ⎥⎥
##            ⎢    ⎢ ⎥       ⎢──────────⎥⎥
##            ⎢    ⎢0⎥       ⎢         2⎥⎥
##            ⎢    ⎢ ⎥       ⎢(b₂ + x₂) ⎥⎥
##            ⎢    ⎣0⎦       ⎢          ⎥⎥
##            ⎢              ⎢   -x₃    ⎥⎥
##            ⎢              ⎢──────────⎥⎥
##            ⎢              ⎢         2⎥⎥
##            ⎢              ⎣(b₂ + x₃) ⎦⎥
##            ⎢                          ⎥
##            ⎢⎡   -x₁    ⎤  ⎡ 2⋅b₁⋅x₁  ⎤⎥
##            ⎢⎢──────────⎥  ⎢──────────⎥⎥
##            ⎢⎢         2⎥  ⎢         3⎥⎥
##            ⎢⎢(b₂ + x₁) ⎥  ⎢(b₂ + x₁) ⎥⎥
##            ⎢⎢          ⎥  ⎢          ⎥⎥
##            ⎢⎢   -x₂    ⎥  ⎢ 2⋅b₁⋅x₂  ⎥⎥
##            ⎢⎢──────────⎥  ⎢──────────⎥⎥
##            ⎢⎢         2⎥  ⎢         3⎥⎥
##            ⎢⎢(b₂ + x₂) ⎥  ⎢(b₂ + x₂) ⎥⎥
##            ⎢⎢          ⎥  ⎢          ⎥⎥
##            ⎢⎢   -x₃    ⎥  ⎢ 2⋅b₁⋅x₃  ⎥⎥
##            ⎢⎢──────────⎥  ⎢──────────⎥⎥
##            ⎢⎢         2⎥  ⎢         3⎥⎥
##            ⎣⎣(b₂ + x₃) ⎦  ⎣(b₂ + x₃) ⎦⎦
```

```r
h2
```

```
## [caracas]: ⎡                                      ⎡   -x₁         -x₂         -x₃    ⎤⎤
##            ⎢⎡    0           0           0     ⎤  ⎢──────────  ──────────  ──────────⎥⎥
##            ⎢⎢                                  ⎥  ⎢         2           2           2⎥⎥
##            ⎢⎢   -x₁         -x₂         -x₃    ⎥  ⎢(b₂ + x₁)   (b₂ + x₂)   (b₂ + x₃) ⎥⎥
##            ⎢⎢──────────  ──────────  ──────────⎥  ⎢                                  ⎥⎥
##            ⎢⎢         2           2           2⎥  ⎢ 2⋅b₁⋅x₁     2⋅b₁⋅x₂     2⋅b₁⋅x₃  ⎥⎥
##            ⎢⎣(b₂ + x₁)   (b₂ + x₂)   (b₂ + x₃) ⎦  ⎢──────────  ──────────  ──────────⎥⎥
##            ⎢                                      ⎢         3           3           3⎥⎥
##            ⎣                                      ⎣(b₂ + x₁)   (b₂ + x₂)   (b₂ + x₃) ⎦⎦
```

```r
h3
```

```
## [caracas]: ⎡                                      ⎡   -x₁         -x₂         -x₃    ⎤⎤
##            ⎢⎡    0           0           0     ⎤  ⎢──────────  ──────────  ──────────⎥⎥
##            ⎢⎢                                  ⎥  ⎢         2           2           2⎥⎥
##            ⎢⎢   -x₁         -x₂         -x₃    ⎥  ⎢(b₂ + x₁)   (b₂ + x₂)   (b₂ + x₃) ⎥⎥
##            ⎢⎢──────────  ──────────  ──────────⎥  ⎢                                  ⎥⎥
##            ⎢⎢         2           2           2⎥  ⎢ 2⋅b₁⋅x₁     2⋅b₁⋅x₂     2⋅b₁⋅x₃  ⎥⎥
##            ⎢⎣(b₂ + x₁)   (b₂ + x₂)   (b₂ + x₃) ⎦  ⎢──────────  ──────────  ──────────⎥⎥
##            ⎢                                      ⎢         3           3           3⎥⎥
##            ⎣                                      ⎣(b₂ + x₁)   (b₂ + x₂)   (b₂ + x₃) ⎦⎦
```

```r
symbol_is_matrix(h1)
```

```
## [1] FALSE
```

```r
symbol_is_matrix(h2)
```

```
## [1] FALSE
```

```r
symbol_is_matrix(33)
```

```
## [1] FALSE
```

```r
h1a <- h1$pyobj %>% do_unbracket %>% do_unbracket %>% as_sym
h2a <- h2$pyobj %>% do_unbracket %>% do_unbracket %>% as_sym # Dimension lost
h3a <- h3$pyobj %>% do_unbracket %>% do_unbracket %>% as_sym # Dimension lost
h1a
```

```
## [caracas]: ⎡                                       -x₁         -x₂         -x₃    ⎤
##            ⎢    0           0           0       ──────────  ──────────  ──────────⎥
##            ⎢                                             2           2           2⎥
##            ⎢                                    (b₂ + x₁)   (b₂ + x₂)   (b₂ + x₃) ⎥
##            ⎢                                                                      ⎥
##            ⎢   -x₁         -x₂         -x₃       2⋅b₁⋅x₁     2⋅b₁⋅x₂     2⋅b₁⋅x₃  ⎥
##            ⎢──────────  ──────────  ──────────  ──────────  ──────────  ──────────⎥
##            ⎢         2           2           2           3           3           3⎥
##            ⎣(b₂ + x₁)   (b₂ + x₂)   (b₂ + x₃)   (b₂ + x₁)   (b₂ + x₂)   (b₂ + x₃) ⎦
```

```r
h2a
```

```
## [caracas]: ⎡            -x₁         -x₂         -x₃         -x₁         -x₂         -x₃  
##            ⎢0, 0, 0, ──────────, ──────────, ──────────, ──────────, ──────────, ────────
##            ⎢                  2           2           2           2           2          
##            ⎣         (b₂ + x₁)   (b₂ + x₂)   (b₂ + x₃)   (b₂ + x₁)   (b₂ + x₂)   (b₂ + x₃
##            
##                 2⋅b₁⋅x₁     2⋅b₁⋅x₂     2⋅b₁⋅x₃  ⎤
##            ──, ──────────, ──────────, ──────────⎥
##             2           3           3           3⎥
##            )   (b₂ + x₁)   (b₂ + x₂)   (b₂ + x₃) ⎦
```

```r
h3a
```

```
## [caracas]: ⎡            -x₁         -x₂         -x₃         -x₁         -x₂         -x₃  
##            ⎢0, 0, 0, ──────────, ──────────, ──────────, ──────────, ──────────, ────────
##            ⎢                  2           2           2           2           2          
##            ⎣         (b₂ + x₁)   (b₂ + x₂)   (b₂ + x₃)   (b₂ + x₁)   (b₂ + x₂)   (b₂ + x₃
##            
##                 2⋅b₁⋅x₁     2⋅b₁⋅x₂     2⋅b₁⋅x₃  ⎤
##            ──, ──────────, ──────────, ──────────⎥
##             2           3           3           3⎥
##            )   (b₂ + x₁)   (b₂ + x₂)   (b₂ + x₃) ⎦
```

```r
symbol_is_matrix(h1a)
```

```
## [1] TRUE
```

```r
symbol_is_matrix(h2a)
```

```
## [1] FALSE
```

```r
symbol_is_matrix(h3a)
```

```
## [1] FALSE
```

