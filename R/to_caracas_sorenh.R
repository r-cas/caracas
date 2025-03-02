##' @title Create a vector along
##' @param x a caracas object 
##' @param entry value 
##' @return a caracas vector
##' @author Søren Højsgaard
vec_along <- function(x, entry="v") { 
    if (length_(x) == 1){
        if (is_sym(x)){
            return(matrix_(entry))            
        } else {
            if (is.numeric(x) && (x > 0)){
                if (x == 1) {
                    return(matrix_(entry))
                } else {
                    return(vector_sym(x, entry))                    
                }
            }
            else {
                stop("can not create symbol\n")
            }
        }
    } else {
        vector_sym(prod(dim(x)), entry)
    }
}




##' @title Length of (caracas) object
##' @param x an object 
##' @return length
##' @author Søren Højsgaard
##' @export
length_ <- function(x) {
    if (is_sym(x)) {
        switch(sym_class(x),
               "atomic"={1},
               "matrix"={prod(dim(x))},
               "vector"={length(as_character_matrix(x))}
               )
    } else {
        switch(class(x),
               "numeric"=,"integer"=,"character"={length(x)},
               "list"  ={length(x)})        
    }
}

           



##' @title Creates block diagonal caracas matrix
##' @param ... Elements to be put on diagonals
##' @return a caracas matrix
##' @author Søren Højsgaard
##' @export
bdiag_ <- function(...){

    args <- list(...)
    if (is.list(args[[1]]) && length(args)==1){
        args <- args[[1]]
    }
    
## 1. make all entries caracas matrices
    lst2 <- lapply(args, function(z){
        if (inherits(z, "caracas_symbol")){
            if (symbol_is_matrix(z)){
                return(z)
            } else {
            return(matrify(z))
            }
        } else { 
            return(matrix_(z))
        }    
    }
    )

    ## 2. Where do blocks start and end?
    rr <- sapply(lst2, nrow)
    cc <- sapply(lst2, ncol)

    br <- cumsum(c(1, rr))[1:length(rr)]
    er <- cumsum(rr)
    
    bc <- cumsum(c(1, cc))[1:length(cc)]
    ec <- cumsum(cc)

    ## 3. Insert blocks in B
    B <- zeros_sym(sum(rr), sum(cc))
    
    for (k in seq_along(lst2)){
        B[br[k]:er[k], bc[k]:ec[k]] <- lst2[[k]]
    }
    
    return(B)
}

