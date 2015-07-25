##
## This file contains the two functions for the Programming Assignment 2 of the R Programming course
##

## This function creates a special "matrix" object that can cache its inverse. It returns a list containing
## the following functions:
##
## set         - set the value of the matrix
## get         - get the value of the matrix
## setInverse  - set the inverse of the matrix
## getInverse  - get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
   i <- NULL
   set <- function(y) {
      x <<- y
      i <<- NULL
   }
   get <- function() x
   setInverse <- function(inverse) i <<- inverse
   getInverse <- function() i
   list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve will
## retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
   i <- x$getInverse()
   if(!is.null(i)) {
      message("getting cached data")
      return(i)
   }
   data <- x$get()
   i <- solve(data, ...)
   x$setInverse(i)
   i
}
