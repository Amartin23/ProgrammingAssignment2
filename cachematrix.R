## Programming Assignment 2
## Author:  Andy Martin
## Date:    7/27/2014 

# This code caches potentially time-consuming computations.
# Two functions are utilized:  makeCacheMatrix and cacheSolve

# makeCacheMatrix is a special vector which is really a list. It uses functions
# to cache the value of inverse calculation.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    SetInverse <- function(inverse) inverse <<- inverse
    GetInverse <- function() inverse
    list(set = set, get = get,
         SetInverse = SetInverse,
         GetInverse = GetInverse)
}

# cacheSolve is a function that calculates the inverse of a matrix.  
# However, it first checks to see if the Inverse has already been calculated. 
# If so, it gets the Inverse from the cache and skips the computation. 
# Otherwise, it calculates the Inverse of the data and sets the value of the 
# Inverse in the cache via the SetInverse function.

cacheSolve <- function(x, ...) {
    inverse <- x$GetInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$SetInverse(inverse)
    inverse
}