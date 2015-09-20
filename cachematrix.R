## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than computing 
## it repeatedly. The following functions allows to cache the result of 
## inverse matrix computation
## Usage:
## > m <- makeCacheMatrix()
## > m$set(matrix(1:4, nrow = 2, ncol = 2))
## > cacheSolve(m) # this call computes and caches the reverse of m
## > cacheSolve(m) # this call returns the cached reverse of m

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  solved <- NULL
  
  set <- function(y) {
    x <<- y
    solved <<- NULL
  }
  
  get <- function() x
  setsolved <- function(s) solved <<- s
  getsolved <- function()  solved
  list (set = set, get = get, 
        getsolved = getsolved, 
        setsolved = setsolved)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix. If the inverse has already been calculated 
## and the matrix has not changed, then cacheSolve retrieves
## the inverse from the cache.
cacheSolve <- function(x, ...) {
  s <- x$getsolved()
  
  if (!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolved(s)
  s
}