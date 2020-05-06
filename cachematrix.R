## Put comments here that give an overall description of what your
## functions do

## This function is designed to build a unique matrix object for cachin the inverse of itself

rm(list = ls())

makeCacheMatrix <- function(ma = matrix()) {
   im <- NULL
   setMatrix <- function(y) {
       ma <<- y
       im <<- NULL
   }
   getMatrix <- function() ma
   setinverse <- function(inv) im <<- inv
   getinverse <- function() im
   list(setMatrix = setMatrix,
        getMatrix = getMatrix,
        setinverse = setinverse,
        getinverse = getinverse)
}


## This function calculates the inverse of the unique matrix produced by makeCacheMatrix.
## The cacheSolve function should return the inverse of the cache matrix, so long as the inverse has already been compiled
## This function will only work if the matrix has not been changed

cacheSolve <- function(x, ...) {
    im <- x$getinverse()
    if(!is.null(im)){
        message("returning cached inverse matrix")
        return(im)
    }
    data <- x$getMatrix()
    i <- solve(data, ...)
    x$setinverse(i)
    i
        ## Return a matrix that is the inverse of 'ma'
}