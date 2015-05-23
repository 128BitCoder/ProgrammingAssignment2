## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. The
## following two functions are used to cache the inverse of a matrix.

## Creates an "object" which stores matrix and its inverse 
# and provides list of methods to get/set them
makeCacheMatrix <- function(x = matrix()) {
  inv_cache <- NULL
  set <- function(y) {
    x <<- y
    inv_cache <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv_cache <<- inverse
  getInverse <- function() inv_cache
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Calculates inverse matrix, caches result to speed up mutliple
## calls with the same input
cacheSolve <- function(x, ...) {
  # check cache and retrun cached value
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    message("Using cached data.")
    return(inverse)
  }
  # calculate inverse matrix and cache it
  message("Calculating new inverse.")
  inverse <- solve(x$get())
  x$setInverse(inverse)
  # return inverse matrix
  inverse
}
