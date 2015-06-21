## There are two functions in this file.  makeCacheMatrix will cache a matrix
## and its inverse.  cacheSolve checks to see if the inverse of a matrix is
## cached and retreives cached version to improve performance. If the inverse
## is not cached, cacheSolve calculates, caches, and returns the inverse.
## -------------------------------------------------------------------

## -------------------------------------------------------------------
## This function creates a list of functions that can be used to cache a 
## matrix and its inverse.  Defined methods include 1) store matrix 2) get matrix
## 3) store inverse matrix 4) get inverse matrix

makeCacheMatrix <- function(x = matrix()) {
      ## Initialize inverse matrix
      inv <- matrix()
      inv <- NULL
      
      ## cache new matrix and clear inverse matrix
      cache <- function(y) {
            x <<- y
            inv <<- NULL
      }
      
      ## get matrix
      get <- function() x
      
      ## cache inverse matrix
      cacheinv <- function(z) inv <<- z
      
      ## get inverse matrix
      getinv <- function() inv
      
      ## create function matrix
      matrix(c(cache = cache, get = get, cacheinv = cacheinv, getinv = getinv), 
            nrow = 2, ncol = 2)
}
## -------------------------------------------------------------------

## -------------------------------------------------------------------
## This function returns the inverse of matrix 'x' drawing on a cached
## version of the inverse if it is available.  Any new inverse that is
## calculated is also cached.

cacheSolve <- function(x, ...) {
      ## if new matrix equals cached matrix, return cached inverse
      y <- x$get()
      if (x == y) {
            message("getting cached data")
            return(x$getinv())
      }
      
      ## if inverse is not cached, calculate new inverse and cache
      ## both original matrix and its inverse
      x$cache(x)
      z <- solve(x)
      x$cacheinv(z)
      z
}
## -------------------------------------------------------------------