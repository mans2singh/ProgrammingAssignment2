## The makeCacheMatrix creates a matrix with supporting methods to 
## support getting/setting the matrix and it's cached inverse
## The cacheSolve method calculates and caches the inverse if its not
## cached and then returns the inverse

## The makeCacheMatrix creates an object that can cache it's inverse
## The function takes one argument which should be an invertible matrix

makeCacheMatrix <- function(x = matrix()) {
        
        inverse <- NULL
        
        set <- function(mtx) {
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
## It computes the inverse if it is not available, caches the inverse in the enhanced
## matrix object and returns it. If the cached inverse of the matrix is 
## available then the cached is returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' if it cached
        inverse <- x$getInverse()
        if ( !is.null(inverse)) {
                message("returning cached solve value")
                return (inverse)
        }
        
        ## Compute the inverse, cache it and return the inverse
        mat <- x$get()
        
        if ( det(mat) == 0 || is.na(det(mat) )) {
                stop("Matrix is not invertible since det is zero or NA")
        }
        
        inv <- solve(mat,...)
        x$setInverse(inv)

        inv
}
