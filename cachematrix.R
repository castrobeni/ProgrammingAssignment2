## This script showcases R functions that can be used to cache potentially
## time-consuming computations for a specific type of matrix operation, which
## is the inverse of a matrix. The method can be of practical value when working
## with large matrices which involves looped processes. However, this only takes
## into account the case where the contents of the matrix remain static. Caching
## the output values would be more efficient since it will no longer involve
## recomputations if they are already done and cached. Below are two functions
## that will be used to create a special object that stores a numeric matrix and
## caches its inverse.


## The first function, makeCacheVector creates a special "matrix" object which
## can cache its inverse for a square matrix x that is invertible. It returns a
## list with the following functions
##      1. set - set the matrix variable
##      2. get - get the matrix variable
##      3. set_inverse - set the inverse of the matrix 
##      4. get_inverse - get the inverse of the matrx

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        set_inverse <- function(inverse) inv <<- inverse
        get_inverse <- function() inv
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}

## The following function cacheSolve computes the inverse of the special 
## "matrix" object returned by makeCacheMatrix above. Given that the matrix
## remained the same and its inverse has already been computed, then cacheSolve
## would retrieve the inverse from the cache, making it essentially faster.

cacheSolve <- function(x, ...) {
        inv <- x$get_inverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$set_inverse(inv)
        inv
}
