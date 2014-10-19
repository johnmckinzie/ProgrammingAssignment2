## This file contains functions that provide the ability to calculate and cache
## the inverse of an existing matrix. The function makeCacheMatrix must be
## called first on an invertible matrix to create a list that can be used
## by cachSolve.

## The function takes an invertible matrix and creates a list that can be used
## by a cacheSolve to calculate and cache the inverse of the provided matrix.
## The list contains the following:
##
##   * set - function to set the matrix
##   * get - function to get the matrix
##   * setinv - funciton to set the inversve matrix
##   * getinv - function to get the inverse matrix
##
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(i) inv <<- i
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function takes a list created using makeCacheMartix to calculate and
## cache the inverse of the matrix contain in the list.
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
