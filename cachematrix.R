## The makeCacheMatrix creates an object with supporting methods for
## getting/setting the matrix and it's inverse.
## The cacheSolve method calculates and caches the inverse if its not
## cached and returns the inverse.

## The makeCacheMatrix creates an object that can cache it's inverse. 
## It provides methods to get and set the matrix and it's inverse.
## The function takes one argument which should be an invertible matrix

makeCacheMatrix <- function(x = matrix()) {
        
        validateInvertible(x)
        
        inverse <- NULL
        
        set <- function(mtx) {
                
                validateInvertible(mtx)
                
                x <<- mtx
                inverse <<- NULL
        }
        
        get <- function() x
        
        setInverse <- function(invr) inverse <<- invr
        
        getInverse <- function() inverse
        
        list(set=set, get=get, 
             getInverse=getInverse, setInverse=setInverse)
}


## The cacheSolve function returns the inverse of the matrix argument. 
## It computes the inverse, if it has not been computed/cached using the solve function, 
## caches the inverse and returns the inverse. If the cached inverse of the matrix is 
## available then the cached is returned.
##
## It's arguments are the enhanced matrix objects returned from
## the makeCacheMatrix and variable arguments that are passed
## to the 'solve()' method
##
## If the matrix's determinant is not availble or zero, it invokes 'stop'
## with a message referring to the error

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' if it cached
        inverse <- x$getInverse()
        if ( !is.null(inverse) ) {
                message("returning cached solve value")
                return (inverse)
        }
        
        ## Compute the inverse, cache it and return the inverse
        mat <- x$get()

        ## Validate matrix is invertible
        validateInvertible(mat)
        
        inv <- solve(mat,...)
        x$setInverse(inv)

        inv
}

## Helper method to check if the argument is a matrix and has a determinant
validateInvertible <- function(mat) {
        if ( !is.matrix(mat) ) {
                stop("The argument should be a matrix")
        }

        if ( det(mat) == 0 || is.na(det(mat) )) {
                stop("Matrix is not invertible since det is zero or NA")
        }
}
