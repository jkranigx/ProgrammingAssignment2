## The functions makeCacheMatrix() and cacheSolve()
## work together to determine the inverse of an
## invertible matrix and cache the inverted matrix
## for better performance. 


## mackeCacheMatrix() creates a wrapper around
## a standard R matrix, binding the functions 
## set(), get() (for setting and getting the 
## underlying R matrix) and setMatInv() and 
## getMatInv() for setting and getting the 
## calculated inverse of the wrapped matrix.
## Parameters:
## mat - an R matrix to be wrapped.
makeCacheMatrix <- function(mat = matrix()) {

    matInv <- NULL
    
    set <- function(matToSet) {
        
        mat <<- matToSet
        matInv <<- NULL
    }
    
    
    get <- function() { 
        
        mat 
    }
    
    
    setMatInv <- function(matInvToSet) { 
        
        matInv <<- matInvToSet 
    }
    
    
    getMatInv <- function() {
        
        matInv
    }
    
    list(set = set, 
         get = get, 
         setMatInv = setMatInv, 
         getMatInv = getMatInv)
    
}
 

## cacheSolve() determines the inverse of the matrix 
## cachingMat if it was NOT been determined previously
## and caches the resulting inverted matrix; otherwise, 
## cacheSolve() will simply return the cached inverted matrix.
## Parameters:
## cachingMat - a "cachedMatrix" produced by the 
##              makeCacheMatrix function.
cacheSolve <- function(cachingMat, ...) {
    
    matInv <- cachingMat$getMatInv()
    
    if (!is.null(matInv)) {
        
        message("Getting cached matrix inverse.")
        return (matInv)
    }
    
    mat <- cachingMat$get()
    matInv <- solve(mat)
    cachingMat$setMatInv(matInv)
    
    matInv
        
}
